# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# NBA and WNBA scraper script.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOAD LIBRARIES*
library(dplyr)
library(stringr)
library(stringi)
library(readr)
library(hoopR)
library(wehoop)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *CONFIG*
league      = "NBA"
season_year = most_recent_nba_season()                 # e.g. 2026 for the 2025-26 season
season_lbl  = paste0(season_year - 1, "-", substr(as.character(season_year), 3, 4))  # "2025-26"

# stat categories, in the app's column order (used for the team averages)
stat_cols = c("PTS","2PM","2PA","3PM","3PA","FTM","FTA",
              "DREB","OREB","REB","AST","STL","BLK","TOV","PF")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *PULL PLAYER BOX SCORES*
pbox = load_nba_player_box(seasons = season_year)
# keep regular season (2) + playoffs (3) only; drops preseason (1) and All-Star (4/5)
pbox = pbox %>% filter(season_type %in% c(2, 3))

# ALLOW-LIST the 30 real franchises (UPPERCASE). Anything else (All-Star squads like
# "TEAM STARS"/"TEAM STRIPES", Rising Stars, etc.) is dropped automatically - no yearly upkeep.
# Matched against the uppercased team name, since ESPN stores them in mixed case.
nba_teams = c(
  "ATLANTA HAWKS","BOSTON CELTICS","BROOKLYN NETS","CHARLOTTE HORNETS","CHICAGO BULLS",
  "CLEVELAND CAVALIERS","DALLAS MAVERICKS","DENVER NUGGETS","DETROIT PISTONS",
  "GOLDEN STATE WARRIORS","HOUSTON ROCKETS","INDIANA PACERS","LA CLIPPERS","LOS ANGELES LAKERS",
  "MEMPHIS GRIZZLIES","MIAMI HEAT","MILWAUKEE BUCKS","MINNESOTA TIMBERWOLVES",
  "NEW ORLEANS PELICANS","NEW YORK KNICKS","OKLAHOMA CITY THUNDER","ORLANDO MAGIC",
  "PHILADELPHIA 76ERS","PHOENIX SUNS","PORTLAND TRAIL BLAZERS","SACRAMENTO KINGS",
  "SAN ANTONIO SPURS","TORONTO RAPTORS","UTAH JAZZ","WASHINGTON WIZARDS")

pbox = pbox %>% filter(toupper(team_display_name) %in% nba_teams)
if (nrow(pbox) == 0) stop("No NBA player box rows for season ", season_year, " yet.")

#' *PULL TEAM BOX SCORES* (official team game logs -> source for the team averages)
tbox = load_nba_team_box(seasons = season_year)
tbox = tbox %>%
  filter(season_type %in% c(2, 3)) %>%
  filter(toupper(team_display_name) %in% nba_teams)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *ONE MATCHUP STRING PER GAME* ("YYYY-MM-DD, HOME vs AWAY", shared by both teams)
if ("home_away" %in% names(pbox)) {
  matchup_map = pbox %>%
    distinct(game_id, game_date, team_abbreviation, home_away) %>%
    group_by(game_id) %>%
    summarise(DATE = first(game_date),
              home = first(team_abbreviation[home_away == "home"]),
              away = first(team_abbreviation[home_away == "away"]),
              .groups = "drop") %>%
    mutate(MATCHUP = paste0(format(as.Date(DATE), "%Y-%m-%d"), ", ", home, " vs ", away)) %>%
    select(GAME_ID = game_id, MATCHUP)
} else {
  # fallback: no home/away flag -> order the two codes alphabetically (still one shared string)
  matchup_map = pbox %>%
    distinct(game_id, game_date, team_abbreviation) %>%
    group_by(game_id) %>%
    arrange(team_abbreviation, .by_group = TRUE) %>%
    summarise(DATE = first(game_date),
              codes = paste(team_abbreviation, collapse = " vs "),
              .groups = "drop") %>%
    mutate(MATCHUP = paste0(format(as.Date(DATE), "%Y-%m-%d"), ", ", codes)) %>%
    select(GAME_ID = game_id, MATCHUP)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *PLAYER-LEVEL, ENRICHED TO THE APP SCHEMA*
# ESPN gives total FG and 3PT; derive two-pointers as (FG - 3PT).
box = pbox %>%
  transmute(
    GAME_ID = game_id,
    PLAYER  = athlete_display_name,
    TEAM    = team_display_name,          # full name -> TEAM column (uppercased below)
    MIN     = round(as.numeric(minutes)),
    PTS     = as.numeric(points),
    `2PM`   = as.numeric(field_goals_made) - as.numeric(three_point_field_goals_made),
    `2PA`   = as.numeric(field_goals_attempted) - as.numeric(three_point_field_goals_attempted),
    `3PM`   = as.numeric(three_point_field_goals_made),
    `3PA`   = as.numeric(three_point_field_goals_attempted),
    FTM     = as.numeric(free_throws_made),
    FTA     = as.numeric(free_throws_attempted),
    DREB    = as.numeric(defensive_rebounds),
    OREB    = as.numeric(offensive_rebounds),
    REB     = as.numeric(rebounds),
    AST     = as.numeric(assists),
    STL     = as.numeric(steals),
    BLK     = as.numeric(blocks),
    TOV     = as.numeric(turnovers),
    PF      = as.numeric(fouls)
  )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *PLAYERS FILE* (exact same schema/order as the other scrapers; drop DNPs)
players = box %>%
  left_join(matchup_map, by = "GAME_ID") %>%
  filter(!is.na(PLAYER), !is.na(MIN), MIN > 0) %>%
  mutate(SEASON = season_lbl, LEAGUE = league,
         PLAYER = stri_trans_general(toupper(PLAYER), "latin-ascii"),
         TEAM   = stri_trans_general(toupper(TEAM),   "latin-ascii")) %>%
  transmute(GAME_ID, SEASON, LEAGUE, PLAYER, TEAM, MATCHUP,
            MIN, PTS, `2PM`, `2PA`, `3PM`, `3PA`, FTM, FTA,
            DREB, OREB, REB, AST, STL, BLK, TOV, PF)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *TEAMS FILE* (per-team SEASON AVERAGE in every category, from TEAM GAME LOGS)
# One row per team per game comes straight from the official team box; then average over games.
# Two-pointers derived as (FG - 3PT), same as the player file. Team points = team_score.
teams = tbox %>%
  transmute(
    TEAM = team_display_name,
    PTS  = as.numeric(team_score),
    `2PM` = as.numeric(field_goals_made) - as.numeric(three_point_field_goals_made),
    `2PA` = as.numeric(field_goals_attempted) - as.numeric(three_point_field_goals_attempted),
    `3PM` = as.numeric(three_point_field_goals_made),
    `3PA` = as.numeric(three_point_field_goals_attempted),
    FTM  = as.numeric(free_throws_made),
    FTA  = as.numeric(free_throws_attempted),
    DREB = as.numeric(defensive_rebounds),
    OREB = as.numeric(offensive_rebounds),
    REB  = as.numeric(total_rebounds),
    AST  = as.numeric(assists),
    STL  = as.numeric(steals),
    BLK  = as.numeric(blocks),
    TOV  = as.numeric(total_turnovers),
    PF   = as.numeric(fouls)
  ) %>%
  group_by(TEAM) %>%
  summarise(GP = n(),
            across(all_of(stat_cols), ~ round(mean(.x, na.rm = TRUE), 1)),
            .groups = "drop") %>%
  mutate(TEAM = stri_trans_general(toupper(TEAM), "latin-ascii")) %>%
  arrange(desc(PTS))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list=setdiff(ls(),c("players","teams")))
write_csv(players, "bball-stats/data/NBA-players.csv")
write_csv(teams,   "bball-stats/data/NBA-teams.csv")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *CONFIG*
league      = "WNBA"
season_year = most_recent_wnba_season()               # WNBA seasons are single-year, e.g. 2026
season_lbl  = as.character(season_year)

stat_cols = c("PTS","2PM","2PA","3PM","3PA","FTM","FTA",
              "DREB","OREB","REB","AST","STL","BLK","TOV","PF")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *PULL PLAYER BOX SCORES*
pbox = load_wnba_player_box(seasons = season_year)
pbox = pbox %>% filter(season_type %in% c(2, 3))       # regular season + playoffs only

# ALLOW-LIST the real WNBA franchises (UPPERCASE). Anything else (All-Star squads like
# TEAM USA/TEAM WNBA, Team Stewart/Team Wilson, etc.) is dropped automatically - no yearly upkeep.
# Matched against the uppercased team name, since ESPN stores them in mixed case.
wnba_teams = c(
  "ATLANTA DREAM","CHICAGO SKY","CONNECTICUT SUN","DALLAS WINGS","GOLDEN STATE VALKYRIES",
  "INDIANA FEVER","LAS VEGAS ACES","LOS ANGELES SPARKS","MINNESOTA LYNX","NEW YORK LIBERTY",
  "PHOENIX MERCURY","PORTLAND FIRE","SEATTLE STORM","TORONTO TEMPO","WASHINGTON MYSTICS")

pbox = pbox %>% filter(toupper(team_display_name) %in% wnba_teams)
if (nrow(pbox) == 0) stop("No WNBA player box rows for season ", season_year, " yet.")

#' *PULL TEAM BOX SCORES* (official team game logs -> source for the team averages)
tbox = load_wnba_team_box(seasons = season_year)
tbox = tbox %>%
  filter(season_type %in% c(2, 3)) %>%
  filter(toupper(team_display_name) %in% wnba_teams)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *ONE MATCHUP STRING PER GAME* ("YYYY-MM-DD, HOME vs AWAY", shared by both teams)
if ("home_away" %in% names(pbox)) {
  matchup_map = pbox %>%
    distinct(game_id, game_date, team_abbreviation, home_away) %>%
    group_by(game_id) %>%
    summarise(DATE = first(game_date),
              home = first(team_abbreviation[home_away == "home"]),
              away = first(team_abbreviation[home_away == "away"]),
              .groups = "drop") %>%
    mutate(MATCHUP = paste0(format(as.Date(DATE), "%Y-%m-%d"), ", ", home, " vs ", away)) %>%
    select(GAME_ID = game_id, MATCHUP)
} else {
  matchup_map = pbox %>%
    distinct(game_id, game_date, team_abbreviation) %>%
    group_by(game_id) %>%
    arrange(team_abbreviation, .by_group = TRUE) %>%
    summarise(DATE = first(game_date),
              codes = paste(team_abbreviation, collapse = " vs "),
              .groups = "drop") %>%
    mutate(MATCHUP = paste0(format(as.Date(DATE), "%Y-%m-%d"), ", ", codes)) %>%
    select(GAME_ID = game_id, MATCHUP)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *PLAYER-LEVEL, ENRICHED TO THE APP SCHEMA* (2PM/2PA derived as FG - 3PT)
box = pbox %>%
  transmute(
    GAME_ID = game_id,
    PLAYER  = athlete_display_name,
    TEAM    = team_display_name,
    MIN     = round(as.numeric(minutes)),
    PTS     = as.numeric(points),
    `2PM`   = as.numeric(field_goals_made) - as.numeric(three_point_field_goals_made),
    `2PA`   = as.numeric(field_goals_attempted) - as.numeric(three_point_field_goals_attempted),
    `3PM`   = as.numeric(three_point_field_goals_made),
    `3PA`   = as.numeric(three_point_field_goals_attempted),
    FTM     = as.numeric(free_throws_made),
    FTA     = as.numeric(free_throws_attempted),
    DREB    = as.numeric(defensive_rebounds),
    OREB    = as.numeric(offensive_rebounds),
    REB     = as.numeric(rebounds),
    AST     = as.numeric(assists),
    STL     = as.numeric(steals),
    BLK     = as.numeric(blocks),
    TOV     = as.numeric(turnovers),
    PF      = as.numeric(fouls)
  )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *PLAYERS FILE* (exact same schema/order as the other scrapers; drop DNPs)
players = box %>%
  left_join(matchup_map, by = "GAME_ID") %>%
  filter(!is.na(PLAYER), !is.na(MIN), MIN > 0) %>%
  mutate(SEASON = season_lbl, LEAGUE = league,
         PLAYER = stri_trans_general(toupper(PLAYER), "latin-ascii"),
         TEAM   = stri_trans_general(toupper(TEAM),   "latin-ascii")) %>%
  transmute(GAME_ID, SEASON, LEAGUE, PLAYER, TEAM, MATCHUP,
            MIN, PTS, `2PM`, `2PA`, `3PM`, `3PA`, FTM, FTA,
            DREB, OREB, REB, AST, STL, BLK, TOV, PF)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *TEAMS FILE* (per-team SEASON AVERAGE in every category, from TEAM GAME LOGS)
teams = tbox %>%
  transmute(
    TEAM = team_display_name,
    PTS  = as.numeric(team_score),
    `2PM` = as.numeric(field_goals_made) - as.numeric(three_point_field_goals_made),
    `2PA` = as.numeric(field_goals_attempted) - as.numeric(three_point_field_goals_attempted),
    `3PM` = as.numeric(three_point_field_goals_made),
    `3PA` = as.numeric(three_point_field_goals_attempted),
    FTM  = as.numeric(free_throws_made),
    FTA  = as.numeric(free_throws_attempted),
    DREB = as.numeric(defensive_rebounds),
    OREB = as.numeric(offensive_rebounds),
    REB  = as.numeric(total_rebounds),
    AST  = as.numeric(assists),
    STL  = as.numeric(steals),
    BLK  = as.numeric(blocks),
    TOV  = as.numeric(total_turnovers),
    PF   = as.numeric(fouls)
  ) %>%
  group_by(TEAM) %>%
  summarise(GP = n(),
            across(all_of(stat_cols), ~ round(mean(.x, na.rm = TRUE), 1)),
            .groups = "drop") %>%
  mutate(TEAM = stri_trans_general(toupper(TEAM), "latin-ascii")) %>%
  arrange(desc(PTS))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list=setdiff(ls(),c("players","teams")))
write_csv(players, "bball-stats/data/WNBA-players.csv")
write_csv(teams,   "bball-stats/data/WNBA-teams.csv")
