# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Builds bball-stats/data/app-data.rds : a single precomputed bundle of every
# object the Shiny app builds at startup, so the app loads one file instead of
# fetching ~20 CSVs and re-running all the transforms on every cold start.
#
# Run after the scrapers, from the repo root:  Rscript bball-stats/bball-stats-app-build-data.R
# Reads the CSVs that are already on disk (local paths, not the GitHub URL).
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(lubridate)
})

# Quiet reader (mirrors the app's wrapper):
vroom = function(...) suppressWarnings(suppressMessages(vroom::vroom(...)))

# Where the CSVs live locally (repo root is the working dir in CI):
data_path = "bball-stats/data/"
out_path  = paste0(data_path, "bball-stats-app-data.rds")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Canonical schemas. any_of() ignores CODE for the files that don't have it:
stat_cols    = c("MIN","PTS","2PM","2PA","3PM","3PA","FTM","FTA",
                 "DREB","OREB","REB","AST","STL","BLK","TOV","PF")
tstat_cols   = setdiff(stat_cols, "MIN")
players_cols = c("GAME_ID","SEASON","LEAGUE","PLAYER","TEAM","MATCHUP", stat_cols) # CODE,
teams_cols   = c("TEAM","CODE","MATCHUP", tstat_cols)

# File prefixes -> LEAGUE labels used by the Trend/Leaders filters:
leagues = c(EL="Euroleague", EC="Eurocup", ES="ACB", GR="GBL", IT="Serie A",
            FR="Pro A", DE="BBL", TR="TBSL", BR="NBB", NBA="NBA", WNBA="WNBA")
teamlog = leagues[!names(leagues) %in% c("NBA","WNBA")]   # NBA/WNBA team files are season averages

# A correctly-typed empty table, so a missing/failed CSV degrades to an empty
# league instead of crashing the whole build (character keys, numeric stats).
make_empty = function(cols) {
  char_cols = intersect(cols, c("GAME_ID","SEASON","LEAGUE","PLAYER","TEAM","MATCHUP","CODE"))
  num_cols  = setdiff(cols, char_cols)
  as_tibble(c(setNames(lapply(char_cols, function(x) character(0)), char_cols),
              setNames(lapply(num_cols,  function(x) numeric(0)),   num_cols)))[cols]
}

# One reader for every file: fixes column order/set and id-column types:
read_stats = function(file, cols) {
  path = paste0(data_path, file)
  if (!file.exists(path)) {
    message("  MISSING ", file, " -> empty table")
    return(make_empty(cols))
  }
  vroom(path) %>%
    mutate(across(any_of(c("GAME_ID","LEAGUE")), as.character),
           across(any_of(stat_cols), as.numeric)) %>%
    select(any_of(cols))
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

message("Reading player files ...")
players = imap(leagues, ~ read_stats(paste0(.y,"-players.csv"), players_cols) %>%
                 mutate(LEAGUE = .x,
                        # WNBA writes SEASON as a bare year:
                        SEASON = if (is.numeric(SEASON)) paste0(SEASON-1,"-",substr(SEASON,3,4))
                        else as.character(SEASON)) %>%
                 distinct(PLAYER, MATCHUP, .keep_all = TRUE))

playersTab = map(players, ~ .x %>%
                   select(-any_of(c("GAME_ID","SEASON","LEAGUE"))) %>%
                   arrange(PLAYER, desc(MATCHUP)))

playersAll = bind_rows(players) %>%
  arrange(PLAYER, LEAGUE, desc(MATCHUP)) %>%
  mutate(PR=PTS+REB, PA=PTS+AST, PRA=PTS+REB+AST, RA=REB+AST) %>%
  select(-any_of(c("GAME_ID","CODE")))

message("Reading team files ...")
teams = map(set_names(names(teamlog)),
            ~ read_stats(paste0(.x,"-teams.csv"), teams_cols) %>%
              distinct(TEAM, MATCHUP, .keep_all = TRUE))

teamsNBA  = read_stats("NBA-teams.csv",  c("TEAM","GP",tstat_cols)) %>% arrange(TEAM)
teamsWNBA = read_stats("WNBA-teams.csv", c("TEAM","GP",tstat_cols)) %>% arrange(TEAM)

# Team game logs bound together, tagged with the LEAGUE names used in the player
# files. OPP_ columns = the other team's line in the same game.
teamGamesFA = imap(teams, ~ .x %>% mutate(LEAGUE = teamlog[[.y]])) %>%
  bind_rows() %>%
  select(-any_of("CODE")) %>%
  distinct(LEAGUE, TEAM, MATCHUP, .keep_all = TRUE) %>%
  group_by(LEAGUE, MATCHUP) %>%
  filter(n() == 2) %>%                                                  # both teams present
  mutate(across(any_of(tstat_cols), ~ rev(.), .names = "OPP_{.col}")) %>%
  ungroup() %>%
  arrange(TEAM, LEAGUE, desc(MATCHUP))

# Stat Leaders tab (kept verbatim; NB the season/date cut-offs are hard-coded):
topMost = playersAll %>%
  filter(LEAGUE=="NBA" & SEASON=="2025-26") %>%
  mutate(DATE=lubridate::as_date(substr(MATCHUP,1,10))) %>%
  filter(DATE<"2026-04-14") %>%
  select(PLAYER,TEAM,PTS,REB,AST,`3PM`,STL,BLK,TOV) %>%
  group_by(PLAYER,TEAM) %>%
  reframe(GAMES=n(), across(PTS:TOV, ~ sum(.,na.rm=T))) %>%
  distinct(PLAYER,.keep_all = T) %>%
  arrange(-PTS) %>% ungroup()

elLeaders = playersAll %>%
  filter(LEAGUE=="Euroleague" & SEASON=="2025-26") %>%
  mutate(DATE=lubridate::as_date(substr(MATCHUP,1,10))) %>%
  filter(DATE<"2026-04-21") %>%
  select(PLAYER,TEAM,PTS,REB,AST,`3PM`,STL,BLK,TOV) %>%
  group_by(PLAYER,TEAM) %>%
  reframe(GAMES=n(), across(PTS:TOV, ~ sum(.,na.rm=T))) %>%
  distinct(PLAYER,.keep_all = T) %>%
  arrange(-PTS) %>% ungroup()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Bundle everything into one named list, plus a small meta block so the app can
# show "data as of ..." and detect a stale/short build.
app_data = list(
  playersTab  = playersTab,
  playersAll  = playersAll,
  teams       = teams,
  teamsNBA    = teamsNBA,
  teamsWNBA   = teamsWNBA,
  teamGamesFA = teamGamesFA,
  topMost     = topMost,
  elLeaders   = elLeaders,
  meta        = list(
    built_at   = Sys.time(),
    player_rows = map_int(playersTab, nrow),
    team_rows   = map_int(teams, nrow),
    total_player_rows = nrow(playersAll)
  )
)

saveRDS(app_data, out_path)   # default gzip; use compress="xz" for a smaller blob

message("Wrote ", out_path)
message("  players (per league): ", paste(names(app_data$meta$player_rows),
                                          app_data$meta$player_rows, sep="=", collapse="  "))
message("  playersAll rows: ", app_data$meta$total_player_rows)