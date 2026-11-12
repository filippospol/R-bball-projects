#### Basketball Reference scrapers

A set of R functions for pulling data off [Basketball Reference](https://www.basketball-reference.com/).

They all behave the same way, and the shared bits live in `bbref_helpers.R`:

Seasons go in as nba.com strings like `"2026-27"`, not bbref's `2027`. Team codes come back in nba.com form, so `BRK` becomes `BKN`, `CHO` becomes `CHA` and `PHO` becomes `PHX` (those are the only three that don't already match). When a player was traded mid-season you get a row for each team he played for, plus one combined row where the team shows the actual codes joined up, like `DAL/LAL`, instead of bbref's `TOT` or `2TM`.

There's also a throttle built in. Basketball Reference starts returning errors at roughly 20 requests a minute and will lock you out for an hour if you push past it, so every request waits its turn (about 3.2 seconds apart, with a rolling check over the last minute and an automatic pause if a `429` does come back). That means you can loop over game logs or fire off a few scrapers in a row without babysitting it. Tables are grabbed by their id after un-hiding the ones bbref buries inside HTML comments, which is what used to trip up the team and opponent shooting tables.

Here's what's in the folder:

| Function | What it does |
| --- | --- |
| `bbref_player_stats(season, mode)` | The full player table for a given mode: `totals`, `per_game`, `per36`, `per100`, `advanced`, `play_by_play`, `shooting` or `adj_shooting`. |
| `bbref_team_stats(season, mode, side)` | Team or opponent numbers for a mode (`per_game`, `totals`, `per100`, `advanced`, `shooting`). Set `side` to `"team"` or `"opponent"`. |
| `bbref_standings(season)` | East and West standings: wins, losses, win %, games back, points for and against, SRS. |
| `bbref_schedule(season)` | Every game in the season with date, both teams, score, OT, attendance and the box-score id. |
| `bbref_odds(season)` | Preseason over/under win totals and title odds, next to how the season actually finished. |
| `bbref_lineups(season)` | How many different starting lineups each team used. |
| `bbref_gameid_date(year, month, day)` | Every bbref game id for a given day. |
| `bbref_game_log(gameID)` | A single game's box score, with starter flags and each player's status (active, inactive, or the DNP reason). |

A quick example:

```r
source("scraper functions/bbref/bbref_helpers.R")
source("scraper functions/bbref/bbref_player_stats.R")

adv  <- bbref_player_stats("2026-27", mode = "advanced")
opp  <- bbref_team_stats("2024-25", mode = "shooting", side = "opponent")
ids  <- bbref_gameid_date(2025, 4, 13)
box  <- purrr::map_dfr(ids, bbref_game_log)
```

Each function pulls in `bbref_helpers.R` the first time it runs, so make sure that file is in the repo too. And `bbref_schedule()` hasn't been tested against a saved page yet, so keep an eye on it the first time you run it.
