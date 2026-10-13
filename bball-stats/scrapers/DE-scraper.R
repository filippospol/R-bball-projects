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
library(readr)

season = "2026-27" ; league = "BBL"
season_id = 2026    # the website's seasonId (2026 = 2026-27), update every season
seed_ids = c()      # optional: any game id of the season, e.g. c(2006925)

# Names: strip accents first, then uppercase ("Würzburg" -> "WURZBURG"):
clean_name = function(x) toupper(stri_trans_general(x, "latin-ascii"))

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

# Game ID's shown on the page (recent results and next games):
ticker_ids = fixture_page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_match("/spiele/(\\d{6,})") %>%
  .[, 2] %>% na.omit() %>% as.numeric()

rm(base_url, fixture_page, next_data_json)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *EXTRACT MATCH ID'S*

# All game ID's of one season sit in one block of numbers (e.g. 2006865-2006950).
# Starting from the ID's we know, check every ID around them until 30 ID's in a row
# are not games of this season.

# Helper: game information for one ID (NULL if it is not a game of this season):
get_game_info = function(id) {
  res = GET(url = glue("https://www.easycredit-bbl.de/_next/data/{daily_key}/de-DE/spiele/{id}.json?id={id}"))
  Sys.sleep(0.2)
  if (res$status_code != 200) return(NULL)
  game = fromJSON(content(res, "text", encoding = "UTF-8"))$pageProps$initialGameData
  if (!isTRUE(game$seasonId == season_id)) return(NULL)
  
  tibble(ID = as.character(id), GAME_DATE = game$scheduledTime %||% NA,
         COMPETITION = game$competition %||% NA,
         HOME_TEAM = game$homeTeam$name %||% NA, AWAY_TEAM = game$guestTeam$name %||% NA)
}

# ID's we already know (website + games saved in the last run):
saved_ids = c()
if (file.exists("bball-stats/data/DE-players.csv")) {
  saved_ids = read_csv("bball-stats/data/DE-players.csv", show_col_types = FALSE) %>%
    filter(SEASON == season) %>% pull(GAME_ID) %>% as.numeric()
}
known = sort(unique(c(seed_ids, ticker_ids, saved_ids)))

# Known ID's can sit in separate blocks (e.g. regular season vs playoffs):
blocks = split(known, cumsum(c(1, diff(known) > 100)))

FF = list()
checked = c()
for (b in blocks) {
  
  # every ID between the lowest and the highest known ID of the block:
  for (id in setdiff(min(b):max(b), checked)) {
    checked = c(checked, id)
    FF[[length(FF) + 1]] = get_game_info(id)
  }
  
  # then walk outwards (down, then up) until 30 ID's in a row are not games of this season:
  for (step in c(-1, 1)) {
    id = if (step < 0) min(b) else max(b) ; misses = 0
    while (misses < 30 && length(checked) < 1500) {
      id = id + step
      if (id %in% checked) next
      checked = c(checked, id)
      info = get_game_info(id)
      misses = if (is.null(info)) misses + 1 else 0
      FF[[length(FF) + 1]] = info
    }
  }
}

# bind all game information together (league games only, no Pokal):
# (empty template so the columns exist even when no games were found)
empty = tibble(ID = character(), GAME_DATE = character(), COMPETITION = character(),
               HOME_TEAM = character(), AWAY_TEAM = character())
fixture_info = bind_rows(empty, FF) %>%
  filter(COMPETITION == "BBL") %>%
  distinct(ID, .keep_all = TRUE) %>%
  mutate(GAME_DATE = as_date(ymd_hms(GAME_DATE))) %>%
  arrange(GAME_DATE)

# Clear environment:
rm(list = setdiff(ls(), c("fixture_info", "daily_key", "season", "league", "clean_name")))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*

PP = list()
TT = list()

# seq_len(nrow(...)) is empty when there are no fixtures -> loop is skipped
for (i in seq_len(nrow(fixture_info))) {
  
  if (is.na(fixture_info$GAME_DATE[i]) || fixture_info$GAME_DATE[i] >= today("UTC")) break
  
  res = GET(url = glue("https://www.easycredit-bbl.de/_next/data/{daily_key}/de-DE/spiele/{fixture_info$ID[i]}.json?id={fixture_info$ID[i]}"))
  if (res$status_code != 200) next
  raw_json = fromJSON(content(res, "text", encoding = "UTF-8"))
  
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
           PLAYER = clean_name(paste0(FIRST_NAME, " ", LAST_NAME)),
           TEAM = clean_name(TEAM)) %>%
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
    mutate_at(7:22, as.numeric)
  
  homeTeam = clean_name(homeBox$TEAM[1]) ; homeCode = homeBox$CODE[1]
  awayTeam = clean_name(awayBox$TEAM[1]) ; awayCode = awayBox$CODE[1]
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
  )
  
  # print(i)
}

rm(list = setdiff(ls(), c("PP", "TT")))
# beepr::beep()

# write files in .csv format (only if we got games, so a bad run keeps the old files)
if (length(PP) > 0) {
  write_csv(bind_rows(PP), "bball-stats/data/DE-players.csv")
  
  write_csv(bind_rows(TT), "bball-stats/data/DE-teams.csv")
}
