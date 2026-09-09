# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script extracts box-score data for the Italian Serie A basketball league.
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
library(jsonlite)
library(glue)
library(janitor)
library(lubridate)
library(rvest)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

league = "Serie A" ; season = "2026-27"
start_year = as.integer(str_sub(season, 1, 4))

# Fetch one calendar page (championship id + game day). NULL on any failure.
get_calendar = function(id, d = 1) {
  res = GET(glue("https://www.legabasket.it/api/championships/get-championships-calendar-by-id?id={id}&d={d}"))
  if (status_code(res) != 200) return(NULL)
  txt = content(res, "text", encoding = "UTF-8")
  if (!str_starts(str_trim(txt), "\\{")) return(NULL)
  fromJSON(txt)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *FIND THIS SEASON'S CHAMPIONSHIP ID(S)*
# Championship ids are sequential (596 = Regular Season 2025/26). Scan upwards from
# there and keep every Serie A championship whose year matches the season, except the
# Supercoppa. This picks up Regular Season plus Play-In / Playoffs once they exist.
champs = list()
misses = 0
for (id in 597:800) {
  cj = get_calendar(id)
  if (is.null(cj$competition)) { misses = misses + 1; if (misses >= 30) break; next }
  misses = 0
  cmp = cj$competition
  if (isTRUE(cmp$year == start_year) && isTRUE(cmp$serie_code == "A1") &&
      !str_detect(cmp$full_name, regex("supercoppa", ignore_case = TRUE))) {
    champs[[length(champs) + 1]] = tibble(ID = cmp$id, NAME = cmp$full_name,
                                          N_DAYS = NROW(cj$filters$days))
  }
}
champs = bind_rows(champs)
if (nrow(champs) == 0) stop("No Serie A championship found for ", season, " - not published yet?")
print(champs)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *EXTRACT MATCH ID'S*  (full schedule: played + scheduled)
fixture_info = map_df(seq_len(nrow(champs)), function(k) {
  map_df(seq_len(champs$N_DAYS[k]), function(MD) {
    cj = get_calendar(champs$ID[k], MD)
    if (is.null(cj$matches) || NROW(cj$matches) == 0) return(NULL)
    cj$matches %>%
      as_tibble() %>%
      clean_names("all_caps") %>%
      select(ID, MATCH_DATETIME, H_TEAM_NAME, H_CLUB_CODE, V_TEAM_NAME, V_CLUB_CODE)
  })
}) %>%
  distinct(ID, .keep_all = TRUE) %>%
  mutate(GAME_DATE = as_date(ymd_hms(MATCH_DATETIME, quiet = TRUE)),
         MATCHUP   = paste0(GAME_DATE, ", ", H_CLUB_CODE, " vs ", V_CLUB_CODE),
         LEAGUE = league, SEASON = season) %>%
  select(GAME_ID = ID, SEASON, LEAGUE, GAME_DATE, MATCHUP, H_TEAM_NAME:V_CLUB_CODE) %>%
  arrange(is.na(GAME_DATE), GAME_DATE)

rm(list = setdiff(ls(), c("fixture_info", "league", "season")))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *DYNAMICALLY FETCH TODAY'S NEXT.JS BUILD ID*
main_page = read_html("https://www.legabasket.it")
build_id  = fromJSON(main_page %>% html_node("#__NEXT_DATA__") %>% html_text())$buildId

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *PARSERS*  (one function per table, applied to home and away)
stat_cols = c(PTS = "PUN", `2PM` = "T2_R", `2PA` = "T2_T", `3PM` = "T3_R", `3PA` = "T3_T",
              FTM = "TL_R", FTA = "TL_T", DREB = "RIMBALZI_D", OREB = "RIMBALZI_O",
              REB = "RIMBALZI_T", AST = "ASS", STL = "PALLE_R", TOV = "PALLE_P",
              BLK = "STOPPATE_DAT", PF = "FALLI_C")

parse_players = function(rows, team_name, fx) {
  rows %>% as_tibble() %>% clean_names("all_caps") %>%
    mutate(GAME_ID = fx$GAME_ID, SEASON = fx$SEASON, LEAGUE = fx$LEAGUE,
           PLAYER  = stri_trans_general(toupper(paste(PLAYER_NAME, PLAYER_SURNAME)), "latin-ascii"),
           TEAM    = stri_trans_general(toupper(team_name), "latin-ascii"),
           MATCHUP = fx$MATCHUP,
           MIN     = round(MIN)) %>%
    select(GAME_ID, SEASON, LEAGUE, PLAYER, TEAM, MATCHUP, MIN, all_of(stat_cols)) %>%
    filter(!is.na(MIN))
}

parse_team = function(totals, team_name, code, fx) {
  totals %>% as_tibble() %>% clean_names("all_caps") %>%
    mutate(TEAM = stri_trans_general(toupper(team_name), "latin-ascii"),
           CODE = code, MATCHUP = fx$MATCHUP) %>%
    select(TEAM, CODE, MATCHUP, all_of(stat_cols))
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*
PP = list()
TT = list()

for (i in seq_len(nrow(fixture_info))) {
  fx = fixture_info[i, ]
  if (is.na(fx$GAME_DATE) || fx$GAME_DATE >= today()) break
  
  Sys.sleep(0.5)                                          # be a polite bot
  
  res = GET(glue("https://www.legabasket.it/_next/data/{build_id}/game/{fx$GAME_ID}/tabellini.json?id={fx$GAME_ID}&detail=tabellini"))
  if (status_code(res) != 200) { print(paste("Skipping game", fx$GAME_ID, "- API Error:", status_code(res))); next }
  
  raw_text = content(res, "text", encoding = "UTF-8")
  if (!str_starts(str_trim(raw_text), "\\{")) { print(paste("Skipping game", fx$GAME_ID, "- HTML instead of JSON.")); next }
  
  sc = fromJSON(raw_text)$pageProps$game$scores
  if (is.null(sc$ht$rows) || is.null(sc$vt$rows)) next     # box score not published yet
  
  PP[[i]] = bind_rows(parse_players(sc$ht$rows, fx$H_TEAM_NAME, fx),
                      parse_players(sc$vt$rows, fx$V_TEAM_NAME, fx))
  TT[[i]] = bind_rows(parse_team(sc$ht$totals, fx$H_TEAM_NAME, fx$H_CLUB_CODE, fx),
                      parse_team(sc$vt$totals, fx$V_TEAM_NAME, fx$V_CLUB_CODE, fx))
}
rm(list = setdiff(ls(), c("PP", "TT")))

# beepr::beep()
# write files in .csv format
write_csv(bind_rows(PP) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/IT-players.csv")

write_csv(bind_rows(TT) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/IT-teams.csv")
