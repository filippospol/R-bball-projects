# NBA Single-Year RAPM Pipeline — Runbook

Paste this whole file into a new chat with Claude (with code execution enabled) and say
something like **"Run this pipeline to build/update the RAPM dataset"**. It contains
everything needed to reconstruct the process from scratch: data source, methodology,
every script, exact commands, known bugs already fixed, and open limitations.

## What this produces

A CSV with one row per player per season:

```
SEASON, PLAYER_ID, PLAYER_NAME, GP, MIN, POSS, O_RAPM, D_RAPM, RAPM
```

- `PLAYER_ID` is the NBA Stats website's own player ID.
- `PLAYER_NAME` is transliterated to Latin-only characters (Jokić → Jokic, Dončić → Doncic).
- `POSS` = exact possession count the player was on court for (own + opponent's), not an
  estimate.
- `RAPM = O_RAPM + D_RAPM`. `D_RAPM` is signed so **positive = good defense** (points
  prevented per 100 possessions), matching the convention that higher `RAPM` is always
  better.

Coverage so far: 1997-98 through 2025-26 (29 seasons), ~14,000 player-season rows.

---

## 1. Data source

[`shufinskiy/nba_data`](https://github.com/shufinskiy/nba_data) — a GitHub mirror of raw
stats.nba.com play-by-play, going back to the 1996-97 season. No API key needed, just
`curl`/`git`. Two relevant file families:

- **`nbastats_<year>.tar.xz`** — classic v2 schema (season `1997` = 1997-98, etc.), used
  for 1997-98 through 2024-25. Check `list_data.txt` (below) for the exact year range
  currently published in this schema.
- **`nbastatsv3_<year>.tar.xz`** — newer v3 schema, structurally different (JSON-ish
  columns, no separate offense/defense rebound flag, substitutions only give the
  outgoing player directly). Needed once a season stops being published in the classic
  format — this happened first for the 2025-26 season.

Get the manifest of what's available:

```bash
mkdir -p rapm && cd rapm
curl -s https://raw.githubusercontent.com/shufinskiy/nba_data/main/list_data.txt -o list_data.txt
grep '^nbastats_' list_data.txt | grep -v po_ | sort    # classic v2 seasons
grep '^nbastatsv3_' list_data.txt | grep -v po_ | sort  # v3 seasons
```

**Before running a new season, check which prefix actually has it** — the classic
`nbastats_` format may or may not keep getting updated; fall back to `nbastatsv3_` if the
season you want isn't there yet.

---

## 2. Methodology

1. **Lineup reconstruction.** Walk each game's play-by-play in order, tracking which 5
   players are on court per team via substitution events. Starters are identified by:
   whoever's *first* mention in a period is not as the "entering" side of a substitution.
   This is re-derived **every period**, not just at tip-off — some games (especially
   older ones) don't log every between-period substitution, so a player can silently
   leave/return with no event; re-deriving period-starters from scratch corrects this.
2. **Exact possession counting** (not the `FGA + 0.44×FTA − OREB + TOV` estimate). A
   state machine walks events and counts a possession as ending on: a made field goal,
   a defensive rebound, a turnover, or the final made/missed free throw of a multi-shot
   trip (with and-1/technical/flagrant/clear-path fouls correctly *not* creating an extra
   possession, since the team keeps the ball). See §4 for the real bugs found and fixed
   while building this.
3. **Regression.** Each stint (interval between lineup changes) produces two rows: one
   for the team on offense (their 5 players = offense columns, opponent's 5 = defense
   columns), one for the reverse. Target = points scored per 100 possessions during that
   stint; weight = possessions. Solved as one ridge regression per season:
   `(X'WX + λI)β = X'Wy`, with a fixed `λ = 2000` and an unpenalized intercept for
   league-average pace. This is a **fixed** penalty, not cross-validated per season —
   a known simplification.

### Validation benchmarks used throughout (compare against Basketball-Reference)

- League pace should land close to the real historical value (e.g. 1997-98 ≈ 90.1
  possessions/team/game).
- League-average points-per-100-possessions should be in the right historical range
  (~103-105 in the late 90s, climbing to ~113-116 by the mid-2020s).
- Spot-check individual players' `MIN`/`GP` against their real Basketball-Reference or
  ESPN totals — this is how every bug below was actually caught.

---

## 3. Scripts

Create a working directory and save each of these files.

```bash
mkdir -p rapm/data rapm/work rapm/season_outputs
cd rapm
pip install pandas numpy scipy nba_api --break-system-packages -q
```

### `parse_season.py` — classic v2 schema parser (1997-98 through the last `nbastats_`
year available)

```python
import sys
import pandas as pd
import numpy as np
from collections import defaultdict
DBG = defaultdict(int)

MADE_FG = 1
MISSED_FG = 2
FT = 3
REBOUND = 4
TURNOVER = 5
FOUL = 6
VIOLATION = 7
SUB = 8
TIMEOUT = 9
JUMPBALL = 10
EJECTION = 11
PERIOD_START = 12
PERIOD_END = 13

def clock_to_sec(s):
    try:
        m, sec = s.split(':')
        return int(m) * 60 + float(sec)
    except Exception:
        return 0.0

def period_len(period):
    return 720.0 if period <= 4 else 300.0

def period_offset(period):
    # cumulative length of all periods before `period`
    if period <= 4:
        return (period - 1) * 720.0
    return 4 * 720.0 + (period - 5) * 300.0

def build_stints_for_game(g):
    """g: DataFrame for one GAME_ID, sorted by PERIOD, EVENTNUM"""
    g = g.reset_index(drop=True)

    # --- determine home/away team ids ---
    home_mask = g['HOMEDESCRIPTION'].fillna('').astype(str).str.len() > 0
    away_mask = g['VISITORDESCRIPTION'].fillna('').astype(str).str.len() > 0
    home_teams = g.loc[home_mask & (g['PLAYER1_TEAM_ID'] > 0), 'PLAYER1_TEAM_ID']
    away_teams = g.loc[away_mask & (g['PLAYER1_TEAM_ID'] > 0), 'PLAYER1_TEAM_ID']
    if len(home_teams) == 0 or len(away_teams) == 0:
        return None
    home_id = home_teams.mode().iloc[0]
    away_id = away_teams.mode().iloc[0]
    if home_id == away_id:
        return None

    # --- for EVERY period, determine who was already on court at that period's tip-off. ---
    # Some (esp. older) games don't log every between-period substitution, so a player can
    # silently leave/return with no SUB event. To correct for this we re-derive the "on
    # court at period start" five for each team using each period's own events: a player's
    # first mention within that period, if it's not as the "entering" side of a SUB, means
    # they were already on the floor when the period began.
    period_starters = {}  # period -> {team_id: [players]}
    for period, gp_ in g.groupby('PERIOD', sort=True):
        first_seen = {}
        for idx, row in gp_.iterrows():
            et = row['EVENTMSGTYPE']
            for pnum in (1, 2, 3):
                pid = row[f'PLAYER{pnum}_ID']
                tid = row[f'PLAYER{pnum}_TEAM_ID']
                if pid <= 0 or tid <= 0:
                    continue
                if pid in first_seen:
                    continue
                is_entering = (et == SUB and pnum == 2)
                first_seen[pid] = (idx, is_entering, tid)
        starters = {home_id: [], away_id: []}
        for pid, (idx, is_entering, tid) in sorted(first_seen.items(), key=lambda x: x[1][0]):
            if tid not in (home_id, away_id) or is_entering:
                continue
            if len(starters[tid]) < 5:
                starters[tid].append(pid)
        period_starters[period] = starters

    ps1 = period_starters.get(1, {home_id: [], away_id: []})
    if len(ps1[home_id]) != 5 or len(ps1[away_id]) != 5:
        return None  # bad/incomplete data for this game, skip

    lineup = {home_id: set(ps1[home_id]), away_id: set(ps1[away_id])}

    # --- walk through events, building stints ---
    stints = []  # dict: home_players, away_players, home_pts, away_pts, home_poss, away_poss
    def new_stint():
        return {'home_pts': 0, 'away_pts': 0, 'home_poss': 0, 'away_poss': 0}

    cur = new_stint()
    last_miss_team = None  # team id that just missed a *possession-ending-eligible* shot/FT
    stint_start_elapsed = 0.0
    import re
    ft_trip_re = re.compile(r'(\d+)\s+OF\s+(\d+)')

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

    def credit_pending(team_id):
        nonlocal last_miss_team
        if last_miss_team is not None:
            if last_miss_team == home_id:
                cur['home_poss'] += 1
            else:
                cur['away_poss'] += 1
            DBG['implicit_credit'] += 1
        last_miss_team = None

    def resolve_pending(new_team):
        nonlocal last_miss_team
        if last_miss_team is not None and new_team != last_miss_team:
            credit_pending(new_team)
        last_miss_team = None

    cur_period = 1
    for idx, row in g.iterrows():
        et = row['EVENTMSGTYPE']
        desc = str(row['HOMEDESCRIPTION']) + ' ' + str(row['VISITORDESCRIPTION']) + ' ' + str(row['NEUTRALDESCRIPTION'])
        desc_u = desc.upper()
        is_miss = 'MISS' in desc_u
        p1_team = row['PLAYER1_TEAM_ID']
        period = row['PERIOD']
        elapsed = period_offset(period) + (period_len(period) - clock_to_sec(str(row['PCTIMESTRING'])))

        if period > cur_period:
            # entering a new period: any still-pending miss from the prior period is over
            boundary_elapsed = period_offset(period)
            credit_pending(None)
            flush_stint(boundary_elapsed)
            ps_ = period_starters.get(period, {})
            for tid_ in (home_id, away_id):
                fivep = ps_.get(tid_, [])
                if len(fivep) == 5:
                    lineup[tid_] = set(fivep)
            cur_period = period
        elif period < cur_period:
            continue  # corrupted/stray trailing row with a stale period value; skip it

        if et == SUB:
            out_pid = row['PLAYER1_ID']
            in_pid = row['PLAYER2_ID']
            tid = row['PLAYER1_TEAM_ID']
            if tid in (home_id, away_id):
                flush_stint(elapsed)
                lineup[tid].discard(out_pid)
                lineup[tid].add(in_pid)
            continue

        # team-level events (e.g. "Bulls Rebound", "Rockets Turnover") store the team id in
        # PLAYER1_ID rather than PLAYER1_TEAM_ID; fall back to that so they aren't dropped.
        if p1_team not in (home_id, away_id):
            p1_id = row['PLAYER1_ID']
            if p1_id in (home_id, away_id):
                p1_team = p1_id
            else:
                continue

        if et == MADE_FG:
            resolve_pending(p1_team)
            DBG['made_fg'] += 1
            pts = 3 if '3PT' in desc_u else 2
            if p1_team == home_id:
                cur['home_pts'] += pts
                cur['home_poss'] += 1
            else:
                cur['away_pts'] += pts
                cur['away_poss'] += 1
            last_miss_team = None

        elif et == MISSED_FG:
            resolve_pending(p1_team)
            last_miss_team = p1_team

        elif et == FT:
            made = not is_miss
            if p1_team == home_id:
                if made:
                    cur['home_pts'] += 1
            else:
                if made:
                    cur['away_pts'] += 1

            if 'TECHNICAL' in desc_u or 'FLAGRANT' in desc_u or 'CLEAR PATH' in desc_u:
                # possession/team retains the ball regardless of make/miss; no possession event
                continue

            m = ft_trip_re.search(desc_u)
            if m:
                x, ycount = int(m.group(1)), int(m.group(2))
            else:
                x, ycount = 1, 1

            if ycount == 1:
                # and-1: the possession already ended at the preceding made field goal
                continue

            if x == ycount:
                # last FT of a multi-shot (non and-1) trip
                if made:
                    if p1_team == home_id:
                        cur['home_poss'] += 1
                    else:
                        cur['away_poss'] += 1
                    last_miss_team = None
                    DBG['ft_last_made'] += 1
                else:
                    last_miss_team = p1_team
            # non-final FT in the trip: no possession implication either way

        elif et == REBOUND:
            if last_miss_team is not None:
                if p1_team == last_miss_team:
                    pass  # offensive rebound: same team recovered, possession continues
                else:
                    # defensive rebound: the other team recovered, possession ends
                    if last_miss_team == home_id:
                        cur['home_poss'] += 1
                    else:
                        cur['away_poss'] += 1
                    DBG['dreb'] += 1
                last_miss_team = None

        elif et == TURNOVER:
            resolve_pending(p1_team)
            DBG['tov'] += 1
            if p1_team == home_id:
                cur['home_poss'] += 1
            else:
                cur['away_poss'] += 1
            last_miss_team = None

    credit_pending(None)
    last_period = g['PERIOD'].max()
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
    cols = ['GAME_ID','EVENTNUM','EVENTMSGTYPE','EVENTMSGACTIONTYPE','PERIOD','PCTIMESTRING',
            'HOMEDESCRIPTION','NEUTRALDESCRIPTION','VISITORDESCRIPTION',
            'PLAYER1_ID','PLAYER1_NAME','PLAYER1_TEAM_ID',
            'PLAYER2_ID','PLAYER2_NAME','PLAYER2_TEAM_ID',
            'PLAYER3_ID','PLAYER3_NAME','PLAYER3_TEAM_ID']
    df = pd.read_csv(csv_path, usecols=cols, low_memory=False)
    for c in ['PLAYER1_ID','PLAYER1_TEAM_ID','PLAYER2_ID','PLAYER2_TEAM_ID','PLAYER3_ID','PLAYER3_TEAM_ID']:
        df[c] = pd.to_numeric(df[c], errors='coerce').fillna(0).astype('int64')
    df = df.sort_values(['GAME_ID', 'PERIOD', 'EVENTNUM'])

    # player id -> name map
    name_map = {}
    for pnum in (1, 2, 3):
        sub = df[[f'PLAYER{pnum}_ID', f'PLAYER{pnum}_NAME']].dropna()
        for pid, nm in zip(sub[f'PLAYER{pnum}_ID'], sub[f'PLAYER{pnum}_NAME']):
            if pid > 0 and isinstance(nm, str) and nm.strip():
                name_map[pid] = nm

    rows = []
    n_games = 0
    n_ok = 0
    gp = defaultdict(int)
    mins = defaultdict(float)
    poss_tot = defaultdict(float)
    for gid, g in df.groupby('GAME_ID', sort=False):
        n_games += 1
        res = build_stints_for_game(g)
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

    stints_df = pd.DataFrame(rows, columns=['GAME_ID','OFF_PLAYERS','DEF_PLAYERS','PTS','POSS'])
    stints_df.to_pickle(out_path)

    names_df = pd.DataFrame(list(name_map.items()), columns=['PLAYER_ID','PLAYER_NAME'])
    names_df.to_csv(out_path.replace('.pkl', '_names.csv'), index=False)

    agg_rows = [(pid, gp[pid], mins[pid], poss_tot[pid]) for pid in gp]
    agg_df = pd.DataFrame(agg_rows, columns=['PLAYER_ID','GP','MIN','POSS'])
    agg_df.to_csv(out_path.replace('.pkl', '_playeragg.csv'), index=False)

    print(f"games={n_games} parsed_ok={n_ok} stint_rows={len(stints_df)}", file=sys.stderr)


if __name__ == '__main__':
    csv_path = sys.argv[1]
    out_path = sys.argv[2]
    process_season_file(csv_path, out_path)
```

### `parse_season_v3.py` — newer v3 schema parser (needed once `nbastats_` stops
covering a season, e.g. 2025-26)

```python
import sys
import re
import unicodedata
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
            shot_val = row.get('shotValue', 2)
            try:
                pts = int(shot_val) if pd.notna(shot_val) and shot_val else 2
            except Exception:
                pts = 2
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
    cols = ['gameId', 'actionNumber', 'period', 'clock', 'teamId', 'personId', 'playerName',
            'playerNameI', 'location', 'description', 'actionType', 'subType', 'shotValue', 'pointsTotal']
    df = pd.read_csv(csv_path, usecols=cols, low_memory=False)
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

### `solve_rapm.py` — ridge regression, same for both schemas

```python
import sys
import numpy as np
import pandas as pd
import scipy.sparse as sp

LAMBDA = 2000.0  # ridge penalty on possession-weighted normal equations (tuned for stability)

def solve_season(stints_pkl, agg_csv, names_csv, season_label, out_csv, lam=LAMBDA):
    stints = pd.read_pickle(stints_pkl)
    agg = pd.read_csv(agg_csv)
    names = pd.read_csv(names_csv)

    # Universe of players who actually appear in stints
    all_players = sorted(set(p for tup in stints['OFF_PLAYERS'] for p in tup) |
                          set(p for tup in stints['DEF_PLAYERS'] for p in tup))
    idx = {pid: i for i, pid in enumerate(all_players)}
    P = len(all_players)
    # columns: [0..P-1] = offense coef per player, [P..2P-1] = defense coef per player, [2P] = intercept
    ncols = 2 * P + 1

    n = len(stints)
    rows = []
    cols = []
    data = []
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
        rows.append(i); cols.append(2 * P); data.append(1.0)  # intercept
        y[i] = 100.0 * pts_arr[i] / poss_arr[i]
        w[i] = poss_arr[i]

    X = sp.csr_matrix((data, (rows, cols)), shape=(n, ncols))
    W = sp.diags(w)
    XtW = X.T @ W
    A = (XtW @ X).toarray()
    b = XtW @ y

    reg = np.full(ncols, lam)
    reg[2 * P] = 0.0  # don't regularize intercept
    A += np.diag(reg)

    beta = np.linalg.solve(A, b)

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
    print(f"{season_label}: players={P} stint_rows={n} intercept={intercept:.2f} "
          f"leagueavg_check={100*pts_arr.sum()/poss_arr.sum():.2f}", file=sys.stderr)
    return out_df


if __name__ == '__main__':
    stints_pkl, agg_csv, names_csv, season_label, out_csv = sys.argv[1:6]
    solve_season(stints_pkl, agg_csv, names_csv, season_label, out_csv)
```

### `run_season.sh` — driver for classic-schema seasons (downloads, parses, solves,
cleans up)

```bash
#!/bin/bash
set -e
YEAR=$1
LABEL="${YEAR}-$(printf '%02d' $(( (YEAR+1) % 100 )))"

DATA_DIR=/home/claude/rapm/data
WORK_DIR=/home/claude/rapm/work
OUT_DIR=/home/claude/rapm/season_outputs
mkdir -p "$DATA_DIR" "$WORK_DIR" "$OUT_DIR"

TAR="$DATA_DIR/nbastats_${YEAR}.tar.xz"
CSV="$DATA_DIR/nbastats_${YEAR}.csv"
PKL="$WORK_DIR/${YEAR}.pkl"
AGG="$WORK_DIR/${YEAR}_playeragg.csv"
NAMES="$WORK_DIR/${YEAR}_names.csv"
OUT="$OUT_DIR/${YEAR}_rapm.csv"

if [ -f "$OUT" ]; then
  echo "[$LABEL] already done, skipping"
  exit 0
fi

echo "[$LABEL] downloading..."
curl -sL "https://github.com/shufinskiy/nba_data/raw/main/datasets/nbastats_${YEAR}.tar.xz" -o "$TAR"

echo "[$LABEL] extracting..."
tar -xf "$TAR" -C "$DATA_DIR"

echo "[$LABEL] parsing..."
python3 /home/claude/rapm/parse_season.py "$CSV" "$PKL"

echo "[$LABEL] solving RAPM..."
python3 /home/claude/rapm/solve_rapm.py "$PKL" "$AGG" "$NAMES" "$LABEL" "$OUT"

echo "[$LABEL] cleaning up raw files..."
rm -f "$TAR" "$CSV"

echo "[$LABEL] done."
```

### `run_season_v3.sh` — driver for v3-schema seasons (2025-26 onward, until further notice)

```bash
#!/bin/bash
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

### `resolve_names_v3.py` — full-name resolution for v3 seasons (they only give short
surnames like "Leonard", not "Kawhi Leonard")

```python
"""
The nbastatsv3 schema only gives short surnames (e.g. "Leonard") or an initialed
form ("K. Leonard") -- no full "First Last" field. This resolves full names for
a v3-parsed season's _rapm.csv by, in priority order:
  1. Looking up the PLAYER_ID against every other already-completed season's
     _names.csv in work/ (most players who appeared before will match).
  2. Falling back to nba_api's static player list (`pip install nba_api`),
     which covers most historical + recent-draft players offline.
  3. Falling back to playerNameI from the raw v3 csv (e.g. "K. Leonard").
  4. Leaving the original short surname if nothing else resolves (rare --
     usually two-way/G-League players who never appear elsewhere).

Usage:
    python3 resolve_names_v3.py <year> <raw_v3_csv_path> <rapm_csv_path_to_update>
"""
import sys
import glob
import pandas as pd


def main(year, raw_csv_path, rapm_csv_path, work_dir='work'):
    year = str(year)

    master_names = {}
    for f in glob.glob(f'{work_dir}/*_names.csv'):
        if year in f:
            continue
        df = pd.read_csv(f)
        for pid, nm in zip(df.PLAYER_ID, df.PLAYER_NAME):
            master_names[pid] = nm

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
        if pid in master_names:
            return master_names[pid]
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

### `combine_seasons.py` — merge every per-season CSV + transliterate names to Latin

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

## 4. Exact commands to run everything

```bash
mkdir -p rapm/data rapm/work rapm/season_outputs && cd rapm
pip install pandas numpy scipy nba_api --break-system-packages -q

# save all six scripts above into this directory, then:
chmod +x run_season.sh run_season_v3.sh

# classic-schema seasons (adjust the year range to whatever nbastats_ covers now)
for y in $(seq 1997 2024); do bash run_season.sh $y; done

# v3-schema seasons (whatever isn't covered by nbastats_ yet)
bash run_season_v3.sh 2025
# add more v3 years here as they become available, e.g.:
# bash run_season_v3.sh 2026

# combine everything + transliterate names
python3 combine_seasons.py nba_single_year_rapm.csv
```

**Practical notes:**
- Downloads run one season per `curl` call; batch 1-2 seasons per shell command if
  running inside a chat tool with an execution time limit — a full season download +
  parse + solve takes roughly 1-3 minutes.
- Disk space: raw extracted CSVs run 90-250MB per season. `run_season.sh` /
  `run_season_v3.sh` delete the `.tar.xz` and `.csv` immediately after parsing, keeping
  only the much smaller pickled stints + per-season output CSV, so total footprint stays
  small even across 29 seasons.
- To add just one new season next year: run the appropriate driver script for that year,
  then re-run `combine_seasons.py`.

---

## 5. Real bugs found and fixed while building this (read before touching the logic)

These were all caught by cross-checking computed `MIN`/`GP`/pace against real
Basketball-Reference/ESPN numbers — that validation step is essential, not optional.

1. **Team-level events store the team ID in the wrong field.** Rows like "Bulls
   Rebound" or "Rockets Turnover" (no individual player credited) put the team ID in
   `PLAYER1_ID` instead of `PLAYER1_TEAM_ID` (v2) or in `personId` with `teamId=0` (v3).
   Naive filtering on the normal team-ID field silently drops these, undercounting
   possessions. Fixed by falling back to the alternate field when the primary one isn't
   one of the game's two team IDs.

2. **Rebound `(Off:X Def:Y)` numbers are running tallies, not per-event flags.** Looks
   like a boolean ("this rebound was offensive/defensive") but is actually the player's
   cumulative rebound counts *as of that moment in the game* — so a player's 2nd
   defensive rebound shows `Def:2`, not `Def:1`. Using it as a per-event flag
   systematically misclassifies rebounds after a player's first of each type. Fixed by
   ignoring it entirely and using the definitionally-correct rule instead: same team as
   the miss = offensive rebound (continues), different team = defensive rebound (ends
   the possession).

3. **A miss isn't always followed by an explicit rebound event before the next shot.**
   ~6% of missed shots are immediately followed by another shot attempt with no
   `REBOUND` row logged in between (especially in older seasons). If unhandled, that
   possession-end is silently dropped. Fixed with a general "resolve pending miss"
   step: whenever a new shot/turnover happens for a *different* team than whoever's
   miss is still unresolved, credit that stale miss as an ended possession first.

4. **Corrupted trailing rows with a stale `period` value.** A small number of games
   have a handful of rows *after* the official end-of-game marker, mislabeled with an
   earlier period number (observed in the v3 feed; may not be systematic). Left
   unguarded, this makes the parser think the game "went back in time," corrupting
   elapsed-time math and inflating some players' minutes past what's physically
   possible in the game (e.g. 94 minutes in a 58-minute double-OT game). Fixed with a
   forward-only period guard: if `period < current_period`, skip the row as corrupted
   rather than processing it.

5. **v3 substitutions only give the outgoing player structurally; the incoming player's
   name is free text and can mismatch the structured name field.** `"SUB: Jokic FOR
   Pickett"` vs. the structured `playerName` field `"Jokić"` (with diacritic) — a plain
   string match fails, silently truncating that player's tracked minutes at the point
   of the failed substitution (the bug that caused Nikola Jokić's 2025-26 RAPM to look
   absurdly low on the first pass: 26.9 computed mpg vs. his real 34.8). Fixed by:
   normalizing accents on both sides before comparing, building the name→ID lookup
   **season-wide** rather than per-game (some players never touch the ball in one
   specific game), and also checking the initialed form (`playerNameI`, e.g.
   "K. Williams") to disambiguate two teammates who share a surname. This dropped the
   substitution-resolution failure rate from ~5.6% to ~0.32% (residual failures are
   almost entirely players who never recorded a single action in any game all season —
   an inherent data limitation, not a fixable bug).

**If you extend this pipeline, re-run the validation step from §2 after any change to
the possession-counting or lineup logic** — these bugs were each individually
plausible-looking (nothing crashed, output looked like a CSV) and only surfaces by
comparing specific players' numbers against a known-real source.

---

## 6. Known limitations (still true after all the fixes above)

- **Single-season RAPM is inherently noisy**, especially for players who rarely sit
  (their impact is highly collinear with their teammates', so the model struggles to
  separate "is it him or his lineup"). Don't over-index on one season's number for any
  one player without sanity-checking it.
- **Fixed ridge penalty (λ=2000), not cross-validated per season.** A real production
  RAPM would tune this per season/split.
- **~0.3% of substitutions in v3 seasons remain unresolved** (players who never
  recorded any action in any game that season) — those players' final period(s) of
  court time may be slightly undercounted if the failure happens mid-game.
- **No possessions are inferred for a very rare "held the ball at the buzzer with no
  shot attempt" situation** (team just dribbles out the clock) — negligible in
  aggregate, but technically an undercounted possession when it happens.
- **Different RAPM implementations never match exactly.** There's no single "official"
  RAPM — this is one defensible, validated methodology, not the only one.
