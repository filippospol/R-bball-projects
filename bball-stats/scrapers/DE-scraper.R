# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script extracts box-score data for Germany's BBL basketball league.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOAD LIBRARIES*
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(stringi)
library(httr)
library(rvest)
library(jsonlite)
library(glue)
library(janitor)
library(lubridate)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *EXTRACT MATCH ID'S*

# Set API headers:
headers = c(
  accept = "application/json, text/plain, */*",
  `accept-language` = "en-GB,en-US;q=0.9,en;q=0.8",
  origin = "https://www.easycredit-bbl.de",
  priority = "u=1, i",
  referer = "https://www.easycredit-bbl.de/",
  `sec-ch-ua` = '"Not:A-Brand";v="99", "Google Chrome";v="145", "Chromium";v="145"',
  `sec-ch-ua-mobile` = "?0",
  `sec-ch-ua-platform` = '"Windows"',
  `sec-fetch-dest` = "empty",
  `sec-fetch-mode` = "cors",
  `sec-fetch-site` = "cross-site",
  `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/145.0.0.0 Safari/537.36",
  `x-api-key` = "publicWebUser",
  `x-api-secret` = "b735b3b6266025671fe81a4605e992e2898fb1ab4afb9dd8db74619ddba7613c"
)

# Helper: pull every page of one gameType ("finished" / "scheduled") into a tibble.
# Safe when the API returns zero games (e.g. before the season starts).
get_fixtures = function(game_type) {
  
  fetch_page = function(page) {
    GET(url = "https://api.basketball-bundesliga.de/games",
        add_headers(.headers = headers),
        query = list(currentPage = page, pageSize = "9", gameType = game_type))
  }
  
  # Empty template so downstream bind_rows() always has the right columns:
  empty = tibble(ID = character(), GAME_DATE = character(),
                 HOME_TEAM = character(), AWAY_TEAM = character())
  
  res = fetch_page(1)
  if (res$status_code != 200) return(empty)
  raw_json = fromJSON(content(res, "text", encoding = "UTF-8"))
  
  # totalPages may be 0 or NULL when there are no games yet:
  n_pages = raw_json$totalPages
  if (is.null(n_pages) || is.na(n_pages) || n_pages < 1) return(empty)
  
  FF = list()
  options(warn = -1)
  for (i in seq_len(n_pages)) {        # seq_len(0) is empty -> loop never runs
    if (i > 1) {
      res = fetch_page(i)
      if (res$status_code != 200) break
      raw_json = fromJSON(content(res, "text", encoding = "UTF-8"))
    }
    if (NROW(raw_json$items) == 0) break   # no games on this page -> stop
    
    items = raw_json$items %>% as_tibble()
    
    # Keep league games only (drops Netto BBL Pokal / cup games).
    # The list items carry the same `competition` field as initialGameData.
    if ("competition" %in% names(items)) {
      items = items %>% filter(competition == "BBL")
    }
    if (nrow(items) == 0) next   # page was all cup games
    
    FF[[i]] = items %>%
      select(ID = id, GAME_DATE = scheduledTime, HOME = homeTeam, AWAY = guestTeam) %>%
      unnest() %>%
      select(ID, GAME_DATE, HOME_TEAM = name, AWAY_TEAM = name1) %>%
      mutate(ID = as.character(ID))
  }
  options(warn = 1)
  
  if (length(FF) == 0) return(empty)
  bind_rows(FF)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Finished games IDs:
fixture_finished = get_fixtures("finished")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Scheduled games IDs:
fixture_scheduled = get_fixtures("scheduled")

# bind all game information together:
fixture_info = bind_rows(fixture_finished, fixture_scheduled) %>%
  mutate(GAME_DATE = as_date(ymd_hms(GAME_DATE))) %>%
  arrange(GAME_DATE)

# Clear environment:
rm(list = setdiff(ls(), c("headers", "fixture_info")))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *GET DAILY URL KEY*
# (This changes every day so we have to make this step!)

# Use html to get the raw website data:
base_url = "https://www.easycredit-bbl.de/saison/aktuelle-spiele"
fixture_page = read_html(base_url)

# Locate the JSON tag that contains the key:
next_data_json = fixture_page %>%
  html_node("#__NEXT_DATA__") %>%
  html_text() %>%
  fromJSON()

# Extract the specific ID
daily_key = next_data_json$buildId
rm(base_url, fixture_page, next_data_json)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*

season = "2026-27" ; league = "BBL"

PP = list()
TT = list()

# seq_len(nrow(...)) is empty when there are no fixtures -> loop is skipped
for (i in seq_len(nrow(fixture_info))) {
  
  if (ymd(fixture_info$GAME_DATE[i]) >= today()) break
  
  res = GET(url = glue("https://www.easycredit-bbl.de/_next/data/{daily_key}/de-DE/spiele/{fixture_info$ID[i]}.json?id={fixture_info$ID[i]}"))
  if (res$status_code != 200) next
  raw_json = fromJSON(content(res, "text", encoding = "UTF-8"))
  
  # Safety net: keep league games only. initialGameData$competition is
  # populated even before tip-off (initialGameStats is NULL until then).
  comp = raw_json$pageProps$initialGameData$competition
  if (!isTRUE(comp == "BBL")) next
  
  # Skip games with no box score published yet:
  if (NROW(raw_json$pageProps$initialGameStats$homeTeam$playerStats) == 0 ||
      NROW(raw_json$pageProps$initialGameStats$guestTeam$playerStats) == 0) next
  
  # Get home and away teams boxscores:
  homeBox = raw_json$pageProps$initialGameStats$homeTeam$playerStats %>%
    as_tibble() %>%
    unnest() %>%
    suppressWarnings() %>%
    mutate_all(as.character)
  homeBox$TEAM = fixture_info$HOME_TEAM[i] ; homeBox$CODE = homeBox$tlc
  
  awayBox = raw_json$pageProps$initialGameStats$guestTeam$playerStats %>%
    as_tibble() %>%
    unnest() %>%
    suppressWarnings() %>%
    mutate_all(as.character)
  awayBox$TEAM = fixture_info$AWAY_TEAM[i] ; awayBox$CODE = awayBox$tlc
  
  # Matchup column:
  MATCHUP = paste0(fixture_info$GAME_DATE[i], ", ",
                   homeBox$CODE[1], " vs ", awayBox$CODE[1])
  
  # Players Boxscore:
  PP[[i]] = bind_rows(homeBox, awayBox) %>%
    clean_names("all_caps") %>%
    mutate(GAME_ID = fixture_info$ID[i], SEASON = season, LEAGUE = league,
           MATCHUP = MATCHUP,
           MIN = round(as.numeric(SECONDS_PLAYED) / 60, 1),
           PLAYER = stri_trans_general(
             toupper(paste0(FIRST_NAME, " ", LAST_NAME)), "latin-ascii")) %>%
    select(GAME_ID, SEASON, LEAGUE, PLAYER, TEAM, MATCHUP, MIN, PTS = POINTS,
           `2PM` = TWO_POINT_SHOTS_MADE, `2PA` = TWO_POINT_SHOTS_ATTEMPTED,
           `3PM` = THREE_POINT_SHOTS_MADE, `3PA` = THREE_POINT_SHOTS_ATTEMPTED,
           FTM = FREE_THROWS_MADE, FTA = FREE_THROWS_ATTEMPTED,
           DREB = DEFENSIVE_REBOUNDS, OREB = OFFENSIVE_REBOUNDS,
           REB = TOTAL_REBOUNDS, AST = ASSISTS, STL = STEALS, BLK = BLOCKS,
           TOV = TURNOVERS, PF = FOULS_COMMITTED) %>%
    mutate(MIN = round(MIN)) %>%
    # if minutes is NA, player DNP so remove that row altogether
    filter(!is.na(MIN)) %>%
    mutate_at(7:22, as.numeric) %>%
    mutate(across(any_of(c("TEAM", "PLAYER")),
                  ~ stri_trans_general(.x, "latin-ascii")))
  
  homeTeam = homeBox$TEAM[1] ; homeCode = homeBox$CODE[1]
  awayTeam = awayBox$TEAM[1] ; awayCode = awayBox$CODE[1]
  rm(homeBox, awayBox)
  
  # Teams Boxscore:
  TT[[i]] = bind_rows(
    raw_json$pageProps$initialGameStats$homeTeam$gameStat %>%
      as_tibble() %>%
      head(1) %>%
      clean_names("all_caps") %>%
      mutate(TEAM = homeTeam, CODE = homeCode, MATCHUP = MATCHUP) %>%
      select(TEAM, CODE, MATCHUP, PTS = POINTS,
             `2PM` = TWO_POINT_SHOTS_MADE, `2PA` = TWO_POINT_SHOTS_ATTEMPTED,
             `3PM` = THREE_POINT_SHOTS_MADE, `3PA` = THREE_POINT_SHOTS_ATTEMPTED,
             FTM = FREE_THROWS_MADE, FTA = FREE_THROWS_ATTEMPTED,
             DREB = DEFENSIVE_REBOUNDS, OREB = OFFENSIVE_REBOUNDS,
             REB = TOTAL_REBOUNDS, AST = ASSISTS, STL = STEALS, BLK = BLOCKS,
             TOV = TURNOVERS, PF = FOULS_COMMITTED),
    raw_json$pageProps$initialGameStats$guestTeam$gameStat %>%
      as_tibble() %>%
      head(1) %>%
      clean_names("all_caps") %>%
      mutate(TEAM = awayTeam, CODE = awayCode, MATCHUP = MATCHUP) %>%
      select(TEAM, CODE, MATCHUP, PTS = POINTS,
             `2PM` = TWO_POINT_SHOTS_MADE, `2PA` = TWO_POINT_SHOTS_ATTEMPTED,
             `3PM` = THREE_POINT_SHOTS_MADE, `3PA` = THREE_POINT_SHOTS_ATTEMPTED,
             FTM = FREE_THROWS_MADE, FTA = FREE_THROWS_ATTEMPTED,
             DREB = DEFENSIVE_REBOUNDS, OREB = OFFENSIVE_REBOUNDS,
             REB = TOTAL_REBOUNDS, AST = ASSISTS, STL = STEALS, BLK = BLOCKS,
             TOV = TURNOVERS, PF = FOULS_COMMITTED)
  ) %>%
    mutate(across(any_of(c("TEAM")),
                  ~ stri_trans_general(.x, "latin-ascii")))
  
  # print(i)
}

rm(list = setdiff(ls(), c("PP", "TT")))
# beepr::beep()

# write files in .csv format
write_csv(bind_rows(PP) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/DE-players.csv")
write_csv(bind_rows(TT) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/DE-teams.csv")
