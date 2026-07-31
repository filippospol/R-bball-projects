# NBA Single-Year RAPM Pipeline -- Runbook (v2: all-v3 schema + cross-validated lambda)

Paste this whole file into a new chat with Claude (code execution enabled) and say
**"Run this pipeline to build the RAPM dataset"**. It is self-contained: data source,
methodology, every script, exact commands, bugs already fixed, and open limitations.

## What this produces

One CSV, one row per player per season:

```
SEASON, PLAYER_ID, PLAYER_NAME, GP, MIN, POSS, O_RAPM, D_RAPM, RAPM
```

- `PLAYER_ID` = NBA Stats' own player ID.
- `PLAYER_NAME` = transliterated to Latin-only characters (Jokic, Doncic, Saric...).
- `POSS` = **exact** possession count the player was on court for (own + opponent),
  counted from play-by-play, not the `0.44*FTA` estimate.
- `RAPM = O_RAPM + D_RAPM`. `D_RAPM` is signed so **positive = good defense** (points
  prevented per 100 possessions); higher `RAPM` is always better.

Coverage: 1997-98 through 2025-26 (29 seasons), ~14,100 player-season rows.

## What changed from v1 of this runbook

1. **Every season now uses the `nbastatsv3_` files** (not the classic `nbastats_`
   format for old years). One schema for all 29 seasons = less parser branching and
   consistent columns. Verified it reproduces the same validated numbers on old seasons
   (1997-98 pace 90.07 vs real ~90.1; Jordan 39.06 mpg vs real ~38.8).
2. **Lambda (ridge penalty) is now chosen by K-fold cross-validation per season**,
   instead of a hard-coded value. See `solve_rapm.py`.

---

## 1. Data source

[`shufinskiy/nba_data`](https://github.com/shufinskiy/nba_data) mirrors stats.nba.com
play-by-play back to 1996-97. No API key. This pipeline uses the **`nbastatsv3_<year>`**
tarballs for every season (`nbastatsv3_1997` = 1997-98, ..., `nbastatsv3_2025` = 2025-26).

Get the manifest to see what's published:

```bash
mkdir -p rapm && cd rapm
curl -s https://raw.githubusercontent.com/shufinskiy/nba_data/main/list_data.txt -o list_data.txt
grep '^nbastatsv3_' list_data.txt | grep -v po_ | sort
```

Next year, add the new season by running the driver for that year, then re-combining.

---

## 2. Methodology

1. **Lineup reconstruction.** Walk each game's events in order, tracking the 5-per-team
   on-court set via substitutions. Re-derive who was on court at the start of **every
   period** (not just tip-off), because some games don't log every between-period sub --
   a player's first mention in a period that isn't the "entering" side of a SUB means
   they were already on the floor.
2. **Exact possession counting.** A state machine ends a possession on: a made FG, a
   defensive rebound, a turnover, or the final made/missed FT of a trip -- with
   and-1/technical/flagrant/clear-path fouls correctly NOT adding a possession (team
   keeps the ball).
3. **Regression.** Each stint yields two rows (each team's offense vs the other's
   defense). Target = points per 100 possessions; weight = possessions. Ridge:
   `(X'WX + lambda*I)beta = X'Wy`, intercept unpenalized. **Lambda is cross-validated**
   (see below).
4. **Cross-validation for lambda.** 4-fold CV over the grid
   `[300, 600, 1200, 2400, 4800, 9600]`, scoring each fold by possession-weighted
   held-out squared error, pick the min, refit on the full season. In practice most
   seasons land on 2400 or 4800, so it adapts rather than defaulting to one value.

### Validation benchmarks (always sanity-check against Basketball-Reference)

- League pace near the real historical value (e.g. 1997-98 ~= 90.1 poss/team/game).
- League points-per-100 in the right range (~103-105 late-90s, ~113-116 mid-2020s).
- Spot-check individual `MIN`/`GP` vs real totals -- this is how every bug below surfaced.

---

## 3. Setup

```bash
mkdir -p rapm/data rapm/work rapm/season_outputs rapm/legacy_names && cd rapm
pip install pandas numpy scipy nba_api --break-system-packages -q
```

> **Note on `legacy_names/`:** the v3 files only ever give short surnames (e.g.
> "Leonard"), for every season. Full names come from (1) `nba_api`'s static player list,
> which covers ~99.8% of players, (2) the initialed form `playerNameI` ("K. Leonard"),
> (3) the bare surname as last resort. If you have full-name CSVs from a previous run,
> drop them in `legacy_names/` as the highest-priority source; otherwise nba_api alone
> is sufficient. Each file is `PLAYER_ID,PLAYER_NAME`.

---

## 4. Scripts

Save each of the following into the `rapm/` directory.

### `parse_season_v3.py`

```python
import sys
import re
import unicodedata
import numpy as np
import pandas as pd
from collections import defaultdict

def norm_name(s):
    """Strip diacritics for robust name matching (description text sometimes drops
    accents that the structured playerName field keeps, e.g. 'Jokic' vs 'Jokić')."""
    if not isinstance(s, str):
        return s
    nfkd = unicodedata.normalize('NFKD', s)
    return ''.join(c for c in nfkd if not unicodedata.combining(c))

def clock_to_sec(s):
    m = re.match(r'PT(\d+)M([\d.]+)S', str(s))
    if not m:
        return 0.0
    return int(m.group(1)) * 60 + float(m.group(2))

def period_len(period):
    return 720.0 if period <= 4 else 300.0

def period_offset(period):
    if period <= 4:
        return (period - 1) * 720.0
    return 4 * 720.0 + (period - 5) * 300.0

FT_TRIP_RE = re.compile(r'(\d+)\s+OF\s+(\d+)')


def build_stints_for_game(g, global_name_map=None, global_namei_map=None):
    g = g.reset_index(drop=True)

    # --- home/away team ids via the location field (h/v), which v3 gives directly ---
    h_rows = g[g['location'] == 'h']
    v_rows = g[g['location'] == 'v']
    if len(h_rows) == 0 or len(v_rows) == 0:
        return None
    home_id = h_rows['teamId'].mode().iloc[0] if (h_rows['teamId'] > 0).any() else None
    away_id = v_rows['teamId'].mode().iloc[0] if (v_rows['teamId'] > 0).any() else None
    if not home_id or not away_id or home_id == away_id:
        return None

    global_name_map = global_name_map or {}
    global_namei_map = global_namei_map or {}

    # --- name -> personId lookup, to resolve incoming players in substitutions.
    # Built primarily from this game's own rows, falling back to season-wide maps for
    # players who never touch the ball in this particular game (e.g. brief garbage-time
    # subs) and to the initialed form (playerNameI) to disambiguate two same-surname
    # teammates (e.g. two players named "Williams" on the same roster).
    name_map = {}
    for _, row in g.iterrows():
        pid = row['personId']
        tid = row['teamId']
        nm = row['playerName']
        if pid > 0 and tid in (home_id, away_id) and isinstance(nm, str) and nm.strip():
            name_map[(tid, norm_name(nm))] = pid

    sub_re = re.compile(r'^SUB:\s*(.+?)\s+FOR\s+(.+)$')

    def resolve_sub(row):
        """Returns (out_pid, in_pid, team) or None if unresolvable."""
        tid = row['teamId']
        out_pid = row['personId']
        m = sub_re.match(str(row['description']))
        if not m or tid not in (home_id, away_id):
            return None
        in_name, out_name = m.group(1).strip(), m.group(2).strip()
        key = (tid, norm_name(in_name))
        in_pid = (global_namei_map.get(key) or name_map.get(key) or
                  global_name_map.get(key))
        if in_pid is None:
            return None
        return out_pid, in_pid, tid

    # --- resolve every substitution up front (needed for period-starter detection) ---
    resolved_subs = {}  # row index -> (out_pid, in_pid, team)
    for idx, row in g.iterrows():
        if row['actionType'] == 'Substitution':
            r = resolve_sub(row)
            if r is not None:
                resolved_subs[idx] = r

    # --- period starters: who was already on court when each period began ---
    period_starters = {}
    for period, gp_ in g.groupby('period', sort=True):
        first_seen = {}
        for idx, row in gp_.iterrows():
            tid = row['teamId']
            if row['actionType'] == 'Substitution':
                r = resolved_subs.get(idx)
                if r is None:
                    continue
                out_pid, in_pid, tid = r
                if out_pid not in first_seen:
                    first_seen[out_pid] = (idx, False, tid)  # out player was on court (leaving)
                if in_pid not in first_seen:
                    first_seen[in_pid] = (idx, True, tid)    # in player is entering
            else:
                pid = row['personId']
                if pid > 0 and tid in (home_id, away_id) and pid not in first_seen:
                    first_seen[pid] = (idx, False, tid)
        starters = {home_id: [], away_id: []}
        for pid, (idx, is_entering, tid) in sorted(first_seen.items(), key=lambda x: x[1][0]):
            if tid not in (home_id, away_id) or is_entering:
                continue
            if len(starters[tid]) < 5:
                starters[tid].append(pid)
        period_starters[period] = starters

    ps1 = period_starters.get(1, {home_id: [], away_id: []})
    if len(ps1[home_id]) != 5 or len(ps1[away_id]) != 5:
        return None

    lineup = {home_id: set(ps1[home_id]), away_id: set(ps1[away_id])}

    stints = []
    def new_stint():
        return {'home_pts': 0, 'away_pts': 0, 'home_poss': 0, 'away_poss': 0}

    cur = new_stint()
    last_miss_team = None
    stint_start_elapsed = 0.0

    def flush_stint(end_elapsed):
        nonlocal cur, stint_start_elapsed
        dur = max(0.0, end_elapsed - stint_start_elapsed)
        stints.append({
            'home_players': tuple(sorted(lineup[home_id])),
            'away_players': tuple(sorted(lineup[away_id])),
            'duration': dur,
            **cur
        })
        cur = new_stint()
        stint_start_elapsed = end_elapsed

    def credit_pending():
        nonlocal last_miss_team
        if last_miss_team is not None:
            if last_miss_team == home_id:
                cur['home_poss'] += 1
            else:
                cur['away_poss'] += 1
        last_miss_team = None

    def resolve_pending(new_team):
        nonlocal last_miss_team
        if last_miss_team is not None and new_team != last_miss_team:
            credit_pending()
        last_miss_team = None

    def loc_team(row):
        loc = row['location']
        if loc == 'h':
            return home_id
        if loc == 'v':
            return away_id
        tid = row['teamId']
        return tid if tid in (home_id, away_id) else None

    cur_period = 1
    for idx, row in g.iterrows():
        at = row['actionType']
        period = row['period']
        clock_sec = clock_to_sec(row['clock'])
        elapsed = period_offset(period) + (period_len(period) - clock_sec)

        if period > cur_period:
            boundary_elapsed = period_offset(period)
            credit_pending()
            flush_stint(boundary_elapsed)
            ps_ = period_starters.get(period, {})
            for tid_ in (home_id, away_id):
                fivep = ps_.get(tid_, [])
                if len(fivep) == 5:
                    lineup[tid_] = set(fivep)
            cur_period = period
        elif period < cur_period:
            continue  # corrupted/stray trailing row with a stale period value; skip it

        if at == 'Substitution':
            r = resolved_subs.get(idx)
            if r is not None:
                out_pid, in_pid, tid = r
                flush_stint(elapsed)
                lineup[tid].discard(out_pid)
                lineup[tid].add(in_pid)
            continue

        team = loc_team(row)
        if team is None:
            continue

        if at == 'Made Shot':
            resolve_pending(team)
            shot_val = row.get('shotValue', np.nan)
            if pd.notna(shot_val) and shot_val:
                try:
                    pts = int(shot_val)
                except Exception:
                    pts = 2
            else:
                pts = 3 if '3PT' in str(row['description']).upper() else 2
            if team == home_id:
                cur['home_pts'] += pts
                cur['home_poss'] += 1
            else:
                cur['away_pts'] += pts
                cur['away_poss'] += 1
            last_miss_team = None

        elif at == 'Missed Shot' or at == 'Heave':
            resolve_pending(team)
            last_miss_team = team

        elif at == 'Free Throw':
            desc_u = str(row['description']).upper()
            subtype_u = str(row['subType']).upper()
            made = 'MISS' not in desc_u
            if made:
                if team == home_id:
                    cur['home_pts'] += 1
                else:
                    cur['away_pts'] += 1

            if 'TECHNICAL' in subtype_u or 'FLAGRANT' in subtype_u or 'CLEAR PATH' in subtype_u:
                continue

            m = FT_TRIP_RE.search(subtype_u)
            if m:
                x, ycount = int(m.group(1)), int(m.group(2))
            else:
                x, ycount = 1, 1

            if ycount == 1:
                continue  # and-1: possession already ended at the preceding made basket

            if x == ycount:
                if made:
                    if team == home_id:
                        cur['home_poss'] += 1
                    else:
                        cur['away_poss'] += 1
                    last_miss_team = None
                else:
                    last_miss_team = team

        elif at == 'Rebound':
            if last_miss_team is not None:
                if team == last_miss_team:
                    pass  # offensive rebound: continues
                else:
                    if last_miss_team == home_id:
                        cur['home_poss'] += 1
                    else:
                        cur['away_poss'] += 1
                last_miss_team = None

        elif at == 'Turnover':
            resolve_pending(team)
            if team == home_id:
                cur['home_poss'] += 1
            else:
                cur['away_poss'] += 1
            last_miss_team = None

    credit_pending()
    last_period = g['period'].max()
    final_elapsed = period_offset(last_period) + period_len(last_period)
    flush_stint(final_elapsed)

    out = []
    player_min = defaultdict(float)
    player_poss = defaultdict(float)
    players_in_game = set()
    for s in stints:
        home_poss = s['home_poss']
        away_poss = s['away_poss']
        total_poss = home_poss + away_poss
        if home_poss > 0:
            out.append((s['home_players'], s['away_players'], s['home_pts'], home_poss))
        if away_poss > 0:
            out.append((s['away_players'], s['home_players'], s['away_pts'], away_poss))
        for pid in s['home_players'] + s['away_players']:
            player_min[pid] += s['duration'] / 60.0
            player_poss[pid] += total_poss
            players_in_game.add(pid)

    return out, player_min, player_poss, players_in_game


def process_season_file(csv_path, out_path):
    # Column availability differs by era (e.g. 'shotValue' is missing from older files),
    # so detect what's actually present rather than hard-coding a usecols list.
    header = pd.read_csv(csv_path, nrows=0).columns.tolist()
    wanted = ['gameId', 'actionNumber', 'period', 'clock', 'teamId', 'personId', 'playerName',
              'playerNameI', 'location', 'description', 'actionType', 'subType', 'shotValue', 'pointsTotal']
    cols = [c for c in wanted if c in header]
    df = pd.read_csv(csv_path, usecols=cols, low_memory=False)

    # Some older-era files pad string fields with trailing whitespace (fixed-width
    # serialization artifact), which silently breaks exact string matching everywhere.
    for c in ['actionType', 'subType', 'location', 'playerName', 'playerNameI', 'description']:
        if c in df.columns:
            df[c] = df[c].apply(lambda x: x.strip() if isinstance(x, str) else x)

    if 'shotValue' not in df.columns:
        df['shotValue'] = np.nan

    df['teamId'] = pd.to_numeric(df['teamId'], errors='coerce').fillna(0).astype('int64')
    df['personId'] = pd.to_numeric(df['personId'], errors='coerce').fillna(0).astype('int64')
    df = df.sort_values(['gameId', 'actionNumber'])

    name_map = {}
    sub = df[['personId', 'playerName']].dropna()
    for pid, nm in zip(sub['personId'], sub['playerName']):
        if pid > 0 and isinstance(nm, str) and nm.strip():
            name_map[pid] = nm

    # season-wide (team, name) -> personId maps, used as a fallback when a player never
    # touches the ball in one particular game, and to disambiguate same-surname teammates
    # via the initialed form (playerNameI, e.g. "K. Williams" vs "J. Williams")
    valid = df[(df['personId'] > 0) & (df['teamId'] > 0)]
    global_name_map = {}
    global_namei_map = {}
    for tid, pid, nm, nmi in zip(valid['teamId'], valid['personId'], valid['playerName'], valid['playerNameI']):
        if isinstance(nm, str) and nm.strip():
            global_name_map[(tid, norm_name(nm))] = pid
        if isinstance(nmi, str) and nmi.strip():
            global_namei_map[(tid, norm_name(nmi))] = pid

    rows = []
    n_games = 0
    n_ok = 0
    gp = defaultdict(int)
    mins = defaultdict(float)
    poss_tot = defaultdict(float)
    for gid, g in df.groupby('gameId', sort=False):
        n_games += 1
        res = build_stints_for_game(g, global_name_map, global_namei_map)
        if res is None:
            continue
        n_ok += 1
        stint_rows, player_min, player_poss, players_in_game = res
        for off_players, def_players, pts, poss in stint_rows:
            rows.append((gid, off_players, def_players, pts, poss))
        for pid in players_in_game:
            gp[pid] += 1
            mins[pid] += player_min[pid]
            poss_tot[pid] += player_poss[pid]

    stints_df = pd.DataFrame(rows, columns=['GAME_ID', 'OFF_PLAYERS', 'DEF_PLAYERS', 'PTS', 'POSS'])
    stints_df.to_pickle(out_path)

    names_df = pd.DataFrame(list(name_map.items()), columns=['PLAYER_ID', 'PLAYER_NAME'])
    names_df.to_csv(out_path.replace('.pkl', '_names.csv'), index=False)

    agg_rows = [(pid, gp[pid], mins[pid], poss_tot[pid]) for pid in gp]
    agg_df = pd.DataFrame(agg_rows, columns=['PLAYER_ID', 'GP', 'MIN', 'POSS'])
    agg_df.to_csv(out_path.replace('.pkl', '_playeragg.csv'), index=False)

    print(f"games={n_games} parsed_ok={n_ok} stint_rows={len(stints_df)}", file=sys.stderr)


if __name__ == '__main__':
    csv_path = sys.argv[1]
    out_path = sys.argv[2]
    process_season_file(csv_path, out_path)

```

### `solve_rapm.py  (cross-validated lambda)`

```python
import sys
import numpy as np
import pandas as pd
import scipy.sparse as sp

LAMBDA_GRID = [300.0, 600.0, 1200.0, 2400.0, 4800.0, 9600.0]
N_FOLDS = 4
CV_SEED = 42


def build_design(stints):
    all_players = sorted(set(p for tup in stints['OFF_PLAYERS'] for p in tup) |
                          set(p for tup in stints['DEF_PLAYERS'] for p in tup))
    idx = {pid: i for i, pid in enumerate(all_players)}
    P = len(all_players)
    ncols = 2 * P + 1  # offense cols, defense cols, intercept

    n = len(stints)
    rows, cols, data = [], [], []
    y = np.zeros(n)
    w = np.zeros(n)

    off_arr = stints['OFF_PLAYERS'].values
    def_arr = stints['DEF_PLAYERS'].values
    pts_arr = stints['PTS'].values.astype(float)
    poss_arr = stints['POSS'].values.astype(float)

    for i in range(n):
        for p in off_arr[i]:
            rows.append(i); cols.append(idx[p]); data.append(1.0)
        for p in def_arr[i]:
            rows.append(i); cols.append(idx[p] + P); data.append(1.0)
        rows.append(i); cols.append(2 * P); data.append(1.0)
        y[i] = 100.0 * pts_arr[i] / poss_arr[i]
        w[i] = poss_arr[i]

    X = sp.csr_matrix((data, (rows, cols)), shape=(n, ncols))
    return X, y, w, all_players, P


def ridge_fit(X, y, w, lam, P):
    ncols = X.shape[1]
    W = sp.diags(w)
    XtW = X.T @ W
    A = (XtW @ X).toarray()
    b = XtW @ y
    reg = np.full(ncols, lam)
    reg[2 * P] = 0.0  # never regularize the intercept
    A += np.diag(reg)
    beta = np.linalg.solve(A, b)
    return beta


def cv_select_lambda(X, y, w, P, grid=LAMBDA_GRID, n_folds=N_FOLDS, seed=CV_SEED):
    """K-fold cross-validation over a lambda grid, using possession-weighted held-out
    squared error as the selection criterion (each fold's weight = possessions, matching
    how the final model itself is fit)."""
    n = X.shape[0]
    rng = np.random.RandomState(seed)
    fold_id = rng.randint(0, n_folds, size=n)

    scores = {}
    for lam in grid:
        total_wse = 0.0
        total_w = 0.0
        for f in range(n_folds):
            train_mask = fold_id != f
            val_mask = ~train_mask
            if val_mask.sum() == 0 or train_mask.sum() == 0:
                continue
            Xtr, ytr, wtr = X[train_mask], y[train_mask], w[train_mask]
            Xval, yval, wval = X[val_mask], y[val_mask], w[val_mask]
            beta = ridge_fit(Xtr, ytr, wtr, lam, P)
            pred = Xval @ beta
            resid = yval - pred
            total_wse += float(np.sum(wval * resid ** 2))
            total_w += float(np.sum(wval))
        scores[lam] = total_wse / total_w if total_w > 0 else np.inf

    best_lam = min(scores, key=scores.get)
    return best_lam, scores


def solve_season(stints_pkl, agg_csv, names_csv, season_label, out_csv,
                  grid=LAMBDA_GRID, n_folds=N_FOLDS):
    stints = pd.read_pickle(stints_pkl)
    agg = pd.read_csv(agg_csv)
    names = pd.read_csv(names_csv)

    X, y, w, all_players, P = build_design(stints)

    best_lam, cv_scores = cv_select_lambda(X, y, w, P, grid=grid, n_folds=n_folds)
    beta = ridge_fit(X, y, w, best_lam, P)

    o_rapm = beta[:P]
    d_rapm_raw = beta[P:2 * P]  # raw: higher = more points ALLOWED (bad defense)
    d_rapm = -d_rapm_raw        # flip sign: positive = points PREVENTED (good defense)
    intercept = beta[2 * P]

    name_map = dict(zip(names['PLAYER_ID'], names['PLAYER_NAME']))
    agg_map_gp = dict(zip(agg['PLAYER_ID'], agg['GP']))
    agg_map_min = dict(zip(agg['PLAYER_ID'], agg['MIN']))
    agg_map_poss = dict(zip(agg['PLAYER_ID'], agg['POSS']))

    out_rows = []
    for i, pid in enumerate(all_players):
        out_rows.append({
            'SEASON': season_label,
            'PLAYER_ID': pid,
            'PLAYER_NAME': name_map.get(pid, ''),
            'GP': agg_map_gp.get(pid, 0),
            'MIN': round(agg_map_min.get(pid, 0.0), 1),
            'POSS': round(agg_map_poss.get(pid, 0.0), 1),
            'O_RAPM': round(o_rapm[i], 2),
            'D_RAPM': round(d_rapm[i], 2),
            'RAPM': round(o_rapm[i] + d_rapm[i], 2),
        })
    out_df = pd.DataFrame(out_rows)
    out_df.to_csv(out_csv, index=False)

    pts_arr = stints['PTS'].values.astype(float)
    poss_arr = stints['POSS'].values.astype(float)
    score_str = ', '.join(f'{lam:.0f}:{cv_scores[lam]:.3f}' for lam in grid)
    print(f"{season_label}: players={P} stint_rows={len(stints)} "
          f"best_lambda={best_lam:.0f} intercept={intercept:.2f} "
          f"leagueavg_check={100*pts_arr.sum()/poss_arr.sum():.2f} "
          f"cv_scores=[{score_str}]", file=sys.stderr)

    # keep a small audit trail of what lambda was chosen and why
    with open(out_csv.replace('.csv', '_lambda.txt'), 'w') as f:
        f.write(f"season={season_label}\nbest_lambda={best_lam}\n")
        for lam in grid:
            f.write(f"lambda={lam}\tcv_weighted_mse={cv_scores[lam]}\n")

    return out_df


if __name__ == '__main__':
    stints_pkl, agg_csv, names_csv, season_label, out_csv = sys.argv[1:6]
    solve_season(stints_pkl, agg_csv, names_csv, season_label, out_csv)

```

### `resolve_names_v3.py`

```python
"""
The nbastatsv3 schema only gives short surnames (e.g. "Leonard") or an initialed
form ("K. Leonard") -- no full "First Last" field, for ANY season (old or new).
This resolves full names for a v3-parsed season's _rapm.csv by, in priority order:
  1. legacy_names/*.csv -- a one-time archive of full names captured from the
     classic v2 schema (which does have full names) for 1997-98 through 2024-25,
     built once before switching the whole pipeline to v3. Exact and authoritative.
  2. nba_api's static player list (`pip install nba_api`), which independently
     covers the vast majority of NBA history + recent draft classes offline.
  3. playerNameI from the raw v3 csv for that season (e.g. "K. Leonard").
  4. Leaving the original short surname if nothing else resolves (rare --
     usually two-way/G-League players who never appear elsewhere).

Usage:
    python3 resolve_names_v3.py <year> <raw_v3_csv_path> <rapm_csv_path_to_update>
"""
import sys
import glob
import pandas as pd


def main(year, raw_csv_path, rapm_csv_path, legacy_dir='legacy_names'):
    legacy_names = {}
    for f in glob.glob(f'{legacy_dir}/*.csv'):
        df = pd.read_csv(f)
        for pid, nm in zip(df.PLAYER_ID, df.PLAYER_NAME):
            legacy_names[pid] = nm

    try:
        from nba_api.stats.static import players
        api_names = {p['id']: p['full_name'] for p in players.get_players()}
    except ImportError:
        print("nba_api not installed (pip install nba_api --break-system-packages); "
              "skipping that fallback.", file=sys.stderr)
        api_names = {}

    nameI_df = pd.read_csv(raw_csv_path, usecols=['personId', 'playerNameI'], low_memory=False)
    nameI_df = nameI_df[nameI_df.personId > 0].drop_duplicates('personId')
    nameI = dict(zip(nameI_df.personId, nameI_df.playerNameI))

    def resolve(pid, fallback):
        if pid in legacy_names:
            return legacy_names[pid]
        if pid in api_names:
            return api_names[pid]
        if pid in nameI:
            return nameI[pid]
        return fallback

    rapm = pd.read_csv(rapm_csv_path)
    rapm['PLAYER_NAME'] = rapm.apply(lambda r: resolve(r['PLAYER_ID'], r['PLAYER_NAME']), axis=1)
    rapm.to_csv(rapm_csv_path, index=False)
    print(f"Resolved names for {len(rapm)} players in {rapm_csv_path}")


if __name__ == '__main__':
    year, raw_csv_path, rapm_csv_path = sys.argv[1:4]
    main(year, raw_csv_path, rapm_csv_path)

```

### `run_season_v3.sh`

```bash
#!/bin/bash
# Driver for a season using the nbastatsv3_ prefix (needed once nbastats_/nbastatsv3_
# stops publishing the classic v2 format -- check list_data.txt each year to see
# which prefix has the season you want).
set -e
YEAR=$1
LABEL="${YEAR}-$(printf '%02d' $(( (YEAR+1) % 100 )))"

DATA_DIR=/home/claude/rapm/data
WORK_DIR=/home/claude/rapm/work
OUT_DIR=/home/claude/rapm/season_outputs
mkdir -p "$DATA_DIR" "$WORK_DIR" "$OUT_DIR"

TAR="$DATA_DIR/nbastatsv3_${YEAR}.tar.xz"
CSV="$DATA_DIR/nbastatsv3_${YEAR}.csv"
PKL="$WORK_DIR/${YEAR}.pkl"
AGG="$WORK_DIR/${YEAR}_playeragg.csv"
NAMES="$WORK_DIR/${YEAR}_names.csv"
OUT="$OUT_DIR/${YEAR}_rapm.csv"

if [ -f "$OUT" ]; then
  echo "[$LABEL] already done, skipping"
  exit 0
fi

echo "[$LABEL] downloading..."
curl -sL "https://github.com/shufinskiy/nba_data/raw/main/datasets/nbastatsv3_${YEAR}.tar.xz" -o "$TAR"

echo "[$LABEL] extracting..."
tar -xf "$TAR" -C "$DATA_DIR"

echo "[$LABEL] parsing (v3 schema)..."
python3 /home/claude/rapm/parse_season_v3.py "$CSV" "$PKL"

echo "[$LABEL] solving RAPM..."
python3 /home/claude/rapm/solve_rapm.py "$PKL" "$AGG" "$NAMES" "$LABEL" "$OUT"

echo "[$LABEL] resolving full player names (v3 only gives short surnames)..."
python3 /home/claude/rapm/resolve_names_v3.py "$YEAR" "$CSV" "$OUT"

echo "[$LABEL] cleaning up raw files..."
rm -f "$TAR" "$CSV"

echo "[$LABEL] done."

```

### `combine_seasons.py`

```python
"""
Combine all per-season *_rapm.csv files in season_outputs/ into one master CSV,
transliterating player names to Latin-only characters (strips diacritics:
Jokic, Doncic, Saric, Bogdanovic, etc.).

Usage:
    python3 combine_seasons.py <output_csv_path>
"""
import sys
import glob
import unicodedata
import pandas as pd


def to_latin(s):
    if not isinstance(s, str):
        return s
    # a few characters unicodedata alone won't decompose cleanly
    s = (s.replace('Đ', 'Dj').replace('đ', 'dj')
           .replace('Ł', 'L').replace('ł', 'l')
           .replace('Ø', 'O').replace('ø', 'o'))
    nfkd = unicodedata.normalize('NFKD', s)
    return ''.join(c for c in nfkd if not unicodedata.combining(c))


def main(out_path, season_outputs_dir='season_outputs'):
    files = sorted(glob.glob(f'{season_outputs_dir}/*_rapm.csv'))
    if not files:
        raise SystemExit(f'No *_rapm.csv files found in {season_outputs_dir}/')

    dfs = [pd.read_csv(f) for f in files]
    full = pd.concat(dfs, ignore_index=True)
    full['PLAYER_NAME'] = full['PLAYER_NAME'].apply(to_latin).fillna('Unknown')
    full = full.sort_values(['SEASON', 'RAPM'], ascending=[True, False]).reset_index(drop=True)

    cols = ['SEASON', 'PLAYER_ID', 'PLAYER_NAME', 'GP', 'MIN', 'POSS', 'O_RAPM', 'D_RAPM', 'RAPM']
    full = full[cols]
    full.to_csv(out_path, index=False)

    print(f"Combined {len(files)} seasons -> {len(full)} player-season rows -> {out_path}")
    print(full['SEASON'].value_counts().sort_index())


if __name__ == '__main__':
    out_path = sys.argv[1] if len(sys.argv) > 1 else 'nba_single_year_rapm.csv'
    main(out_path)

```

---

## 5. Run everything

```bash
mkdir -p rapm/data rapm/work rapm/season_outputs rapm/legacy_names && cd rapm
pip install pandas numpy scipy nba_api --break-system-packages -q
# save the 5 scripts above into this directory, then:
chmod +x run_season_v3.sh

# all 29 seasons (adjust upper bound as new seasons publish)
for y in $(seq 1997 2025); do bash run_season_v3.sh $y; done

# combine + transliterate names to Latin-only
python3 combine_seasons.py nba_single_year_rapm_1997_2025.csv
```

**Practical notes:**
- One season = download + parse + CV-solve + name-resolve, roughly 2-4 min. Inside a
  chat tool with a per-call time limit, run 1-2 seasons per shell command.
- `run_season_v3.sh` deletes the raw `.tar.xz` and `.csv` right after parsing, keeping
  only the small pickled stints + per-season CSV, so disk stays modest across 29 seasons.
- To add one new season later: run the driver for that year, then re-run
  `combine_seasons.py`.
- CV lambda selection adds only ~3s/season; the grid is in `solve_rapm.py` (`LAMBDA_GRID`)
  if you want to widen it.

---

## 6. Real bugs found and fixed (read before touching the logic)

All caught by comparing computed `MIN`/`GP`/pace to real Basketball-Reference numbers --
that validation step is essential, not optional.

1. **Team-level events hide the team ID.** Rows like "Bulls Rebound"/"Rockets Turnover"
   (no individual player) carry `teamId=0` but a populated `location` (h/v). Resolve the
   team from `location` first, or these possessions get dropped.
2. **Rebound `(Off:X Def:Y)` numbers are running tallies, not per-event flags.** They're
   the player's cumulative counts at that moment, so using them as an "is this off/def"
   flag misclassifies every rebound after a player's first. Instead: same team as the
   miss = offensive (continue), different team = defensive (possession ends).
3. **A miss isn't always followed by an explicit rebound event.** ~6% of misses go
   straight to the next action with no rebound row. Handle with a "resolve pending miss"
   step: when a new shot/turnover happens for a *different* team than the unresolved
   miss, credit that miss as an ended possession first.
4. **Corrupted trailing rows with a stale `period`.** A few games have rows after the
   end-of-game marker mislabeled with an earlier period, which makes the parser "go back
   in time" and inflates minutes (e.g. 94 min in a 58-min double-OT game). Guard:
   forward-only period transitions; skip any row whose period < current.
5. **v3 substitutions: incoming player is free text and can mismatch the structured
   name.** `"SUB: Jokic FOR Pickett"` vs structured `"Jokic"` (with diacritic in the
   source) -- a plain match fails and silently truncates that player's tracked minutes.
   This is exactly why Jokic's 2025-26 first looked absurdly low (26.9 computed mpg vs
   real 34.8). Fix: strip diacritics on both sides before matching, build the name->ID
   map **season-wide** (some players never touch the ball in a given game), and also
   match the initialed form (`playerNameI`) to disambiguate same-surname teammates.
   Dropped the sub-resolution failure rate to ~0.3%.
6. **Old-era v3 files differ subtly from recent ones.** They lack the `shotValue` column
   (infer 2 vs 3 from the description text instead) and pad string fields like
   `actionType` with trailing whitespace (strip all text columns before matching, but
   preserve genuine NaN rather than turning it into the string "nan").

**After any change to possession or lineup logic, re-run the §2 validation** -- these
bugs all produced plausible-looking output and only surfaced on cross-checking specific
players against a known source.

---

## 7. Known limitations (still true after all fixes)

- **Single-season RAPM is noisy**, especially for players who rarely sit (their impact is
  collinear with their lineup's). Sanity-check any one number before leaning on it.
- **CV picks lambda from a finite grid** and uses a fixed random-fold split (seed=42);
  a different fold scheme or a finer/wider grid could shift the chosen value slightly.
- **~0.3% of v3 substitutions stay unresolved** (players with no recorded action all
  season); their final period(s) of court time may be slightly undercounted.
- **A rare "dribble out the clock, no shot" possession** isn't counted -- negligible in
  aggregate.
- **A handful of NBA-Stats data quirks remain**, e.g. one single-appearance player
  (ID 1277, 12 min in 2017-18) that isn't in any name database and stays "Unknown". Not
  a pipeline bug -- an upstream data gap.
- **No single "official" RAPM exists.** This is one defensible, validated methodology,
  not the only one; it won't match other public RAPM sets exactly.
