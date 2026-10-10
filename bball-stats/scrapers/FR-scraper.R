# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script extracts box-score data for France's Betclic ELITE (Pro A).
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOAD LIBRARIES*
library(dplyr)
library(purrr)
library(stringr)
library(stringi)
library(httr)
library(jsonlite)
library(lubridate)
library(readr)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *SETTINGS*
league = "Pro A" ; season = "2026-27"
season_id = "4e3b2f66-6a4c-11f1-a5ad-25baed35874f"   # Sportradar id of "Betclic ELITE 2026"
base_url = "https://embed-api.eui.connect.sportradar.com/v1/embed/12"

# GET one widget endpoint and return the JSON as nested lists (NULL if it fails):
get_json = function(path, query = list()) {
  res = RETRY("GET", paste0(base_url, "/", path), query = query,
              add_headers(`user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/154.0.0.0 Safari/537.36",
                          referer = "https://lnb.fr/", origin = "https://lnb.fr"),
              times = 4, pause_base = 2, quiet = TRUE)
  Sys.sleep(0.3)                                   # be a polite bot
  if (http_error(res)) return(NULL)
  fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

# Names: strip accents first, then uppercase ("Chalon/Saône" -> "CHALON/SAONE"):
clean_name = function(x) toupper(stri_trans_general(x, "latin-ascii"))

# Minutes come as "PT22M6S" -> 22.1
iso_minutes = function(x) {
  if (is.null(x)) return(NA_real_)
  h = as.numeric(str_match(x, "(\\d+)H")[, 2])
  m = as.numeric(str_match(x, "(\\d+)M")[, 2])
  s = as.numeric(str_match(x, "([0-9.]+)S")[, 2])
  if (all(is.na(c(h, m, s)))) NA_real_ else sum(c(h * 60, m, s / 60), na.rm = TRUE)
}

# Box-score stats (players and team totals use the same field names):
get_stats = function(s) tibble(
  PTS = s$points, `2PM` = s$pointsTwoMade, `2PA` = s$pointsTwoAttempted,
  `3PM` = s$pointsThreeMade, `3PA` = s$pointsThreeAttempted,
  FTM = s$freeThrowsMade, FTA = s$freeThrowsAttempted,
  DREB = s$reboundsDefensive, OREB = s$reboundsOffensive, REB = s$rebounds,
  AST = s$assists, STL = s$steals, BLK = s$blocks, TOV = s$turnovers, PF = s$foulsTotal)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *EXTRACT TEAM ID'S*

# Team pages need the widget "state": zlib-compressed, base64url-encoded JSON.
# z = "fixtures" opens the team's "Matchs" tab (its full season).
state = toJSON(list(s = season_id, z = "fixtures"), auto_unbox = TRUE) %>%
  as.character() %>% charToRaw() %>% memCompress("gzip") %>% base64_enc() %>%
  chartr("+/", "-_", .) %>% str_remove_all("[=\r\n]")

# One team from the upcoming-games list; its schedule includes every other team:
upcoming = get_json("fixtures", list(seasonId = season_id))
first_team = upcoming$data$fixtures[[1]]$competitors[[1]]$entityId
first_schedule = get_json("entity_detail", list(entityId = first_team, state = state))

team_ids = first_schedule$data$team$fixtures %>%
  map(~ map_chr(.x$competitors, ~ .x$entityId %||% NA_character_)) %>%
  unlist() %>% na.omit() %>% unique()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *EXTRACT MATCH ID'S* (every team's full season, finished games only)

FF = list()
for (i in seq_along(team_ids)) {
  team_json = get_json("entity_detail", list(entityId = team_ids[i], state = state))
  FF[[i]] = map_dfr(team_json$data$team$fixtures, ~ tibble(
    GAME_ID = .x$fixtureId, DATE = .x$startTimeUTC, STATUS = .x$status$value))
}

fixture_info = bind_rows(FF) %>%
  distinct(GAME_ID, .keep_all = TRUE) %>%
  filter(STATUS == "CONFIRMED", ymd_hms(DATE) < now("UTC")) %>%    # finished games
  arrange(DATE)

rm(list=setdiff(ls(),c("fixture_info","league","season","base_url",
                       "get_json","clean_name","iso_minutes","get_stats")))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*

PP = list()
TT = list()
for (i in seq_len(nrow(fixture_info))) {
  
  raw_json = get_json("fixture_detail", list(fixtureId = fixture_info$GAME_ID[i]))
  if (is.null(raw_json)) next
  
  # League games only, and only once the box score is published:
  banner = raw_json$data$banner
  if (!str_detect(clean_name(banner$competition$name), "BETCLIC ELITE")) next
  base = raw_json$data$statistics$data$base
  if (is.null(base$home) || is.null(base$away)) next
  
  # Teams and matchup string ("YYYY-MM-DD, HOME vs AWAY"):
  home = keep(banner$fixture$competitors, ~ isTRUE(.x$isHome))[[1]]
  away = discard(banner$fixture$competitors, ~ isTRUE(.x$isHome))[[1]]
  MATCHUP = paste0(substr(banner$fixture$startDateTime, 1, 10), ", ", home$code, " vs ", away$code)
  
  for (side in c("home", "away")) {
    team = if (side == "home") home else away
    
    # Player Stats (players who did not play are dropped):
    players = base[[side]]$persons[[1]]$rows %>%
      keep(~ isTRUE(.x$participated) && !is.null(.x$statistics)) %>%
      map_dfr(~ tibble(PLAYER = .x$personName, MIN = iso_minutes(.x$statistics$minutes)) %>%
                bind_cols(get_stats(.x$statistics)))
    
    if (nrow(players) > 0) {
      PP[[length(PP) + 1]] = players %>%
        mutate(GAME_ID = fixture_info$GAME_ID[i], SEASON = season, LEAGUE = league,
               PLAYER = clean_name(PLAYER), TEAM = clean_name(team$name),
               MATCHUP = MATCHUP, MIN = round(MIN)) %>%
        filter(!is.na(MIN)) %>%
        select(GAME_ID, SEASON, LEAGUE, PLAYER, TEAM, MATCHUP, MIN, everything())
    }
    
    # Team Stats:
    TT[[length(TT) + 1]] = tibble(TEAM = clean_name(team$name), CODE = team$code, MATCHUP = MATCHUP) %>%
      bind_cols(get_stats(base[[side]]$entity))
  }
}
rm(list=setdiff(ls(),c("PP","TT")))

# beepr::beep()
# write files in .csv format
write_csv(bind_rows(PP),"bball-stats/data/FR-players.csv")

write_csv(bind_rows(TT),"bball-stats/data/FR-teams.csv")
