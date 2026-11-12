#### Scraping data from PBP Stats

R functions for pulling data off the [PBP Stats](https://www.pbpstats.com/) API at `api.pbpstats.com`.

The shared code sits in `pbpstats_helpers.R`. A few notes before you dig in.

Seasons go in as nba.com strings like `"2026-27"`, which is what the API already uses, so there's no converting back and forth. Team codes also come back the way pbpstats stores them, which happens to be nba.com style (`BKN`, `CHA`, `PHX`), so there's nothing to remap. Each row already has the player's name, nba.com id and team on it, so these functions don't go near `hoopR` or stats.nba.com like the old ones did. That's most of why they're quicker and break less often.

Every call shares one throttle, so you can loop over players or run a few functions one after another without pounding the site. pbpstats is a small operation, so the gap between requests is set on the generous side, and it backs off on its own if a 429 ever shows up. When a function needs a player's id, like WOWY or a game log by name, it grabs it from a single totals call and remembers it, so looking up five players isn't five separate requests.

Here's what's in the folder:

| Function | What it does |
| --- | --- |
| `pbp_totals(season, type, stat_type)` | Season totals for a player, team or opponent. `type` is `"Player"`, `"Team"` or `"Opponent"`; `stat_type` is `"PerGame"`, `"Totals"` or `"Per100Possessions"`. Shot distribution lives here now, it's just some of the columns. |
| `pbp_game_logs(season, entity, type)` | Game-by-game logs for one player (name or id) or team. |
| `pbp_wowy(season, players)` | WOWY for two or more team-mates. Hand it a vector of names and it gives you every on/off combination with Off/Def/Net rating and FG%. |
| `pbp_on_off(season)` | On/off and the on-off swing for every player in the league. |

The columns keep pbpstats' own names by default (`AtRimFGM`, `TsPct`, `OnOffRtg` and the rest). If you want them tidied up instead, every function takes `clean_names = "all_caps"` or `"snake"`.

```r
source("pbpstats/pbpstats_helpers.R")
source("pbpstats/pbp_totals.R")

tot  <- pbp_totals("2026-27", type = "Player", stat_type = "Per100Possessions")
logs <- pbp_game_logs("2026-27", "CJ McCollum")
wowy <- pbp_wowy("2026-27", c("Dorian Finney-Smith", "Nickeil Alexander-Walker"))
```

Each function grabs `pbpstats_helpers.R` the first time it runs, so that file needs to be in the repo as well. `pbp_on_off` is the only one I haven't run against a saved response yet, so keep an eye on it the first time.
