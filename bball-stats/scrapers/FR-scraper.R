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

#' *EXTRACT MATCH ID'S*

league = "Pro A" ; season = "2026-27"

# API headers:
headers = c(
  accept = "application/json, text/plain, */*",
  `accept-language` = "en-GB,en-US;q=0.9,en;q=0.8",
  `content-type` = "application/json",
  language_code = "fr",
  origin = "https://lnb.fr",
  priority = "u=1, i",
  referer = "https://lnb.fr/fr/calendar",
  `sec-ch-ua` = '"Not(A:Brand";v="8", "Chromium";v="144", "Google Chrome";v="144"',
  `sec-ch-ua-mobile` = "?0",
  `sec-ch-ua-platform` = '"Windows"',
  `sec-fetch-dest` = "empty",
  `sec-fetch-mode` = "cors",
  `sec-fetch-site` = "same-site",
  `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36"
)

# Date parameters, derived from `season`: September of the start year through June of the next.
start_year   = as.integer(str_sub(season, 1, 4))
month_starts = seq(ymd(paste0(start_year, "-09-01")),
                   ymd(paste0(start_year + 1, "-06-01")),
                   by = "month")
competition_start_dates = format(month_starts, "%Y-%m-%d")
competition_end_dates   = format(ceiling_date(month_starts, "month") - days(1), "%Y-%m-%d")

# 302 = Regular Season, 308 = Playoffs
competition_ids = c(0)

FF = list()

for (comp_id in competition_ids) {
  for (i in seq_along(competition_start_dates)) {
    
    data = paste0('{"competition_external_id":', comp_id, ',"start_date":"',
                  competition_start_dates[i],
                  '","end_date":"',
                  competition_end_dates[i],
                  '"}')
    
    res = httr::POST(url = "https://api-prod.lnb.fr/match/getCalendar", httr::add_headers(.headers=headers)
                     , body = data
    )
    
    raw_calendar = fromJSON(content(res, "text", encoding = "UTF-8"))
    
    if (length(raw_calendar$data) > 0) {
      FF[[length(FF) + 1]] = suppressWarnings(
        raw_calendar$data %>%
          data.frame() %>%
          as_tibble() %>%
          unnest()
      )
    }
  }
}

# Full schedule (played + scheduled), Pro A only, one row per match:
fixture_info = bind_rows(FF) %>%
  filter(str_detect(competition_abbrev, "PROA")) %>%
  distinct(match_id, .keep_all = TRUE)

# table(fixture_info$competition_abbrev) add new values to str_detect

rm(list=setdiff(ls(),c("fixture_info","league","season")))

#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*

# run time around 5 minutes
PP = list()
TT = list()
for (i in seq_along(fixture_info$match_id)) {
  # URL (JSON):
  fixture_url = GET(
    url=glue(
      "https://embed-api.eui.connect.sportradar.com/v1/embed/12/fixture_detail?fixtureId={fixture_info$match_id[i]}"
    )
  )
  
  # API data:
  raw_json = suppressMessages(
    fromJSON(content(fixture_url, "text"))
  )
  
  if (
    raw_json$data$banner$competition$name %>% pluck(1) %>% stri_trans_general("latin-ascii") %>% str_detect("Betclic ELITE") == FALSE
  ) next
  
  # Fixture info:
  fixture_id = raw_json$data$banner$fixture$id
  fixture_teamcodes = raw_json$data$banner$fixture$competitors$code
  fixture_teamnames = raw_json$data$banner$fixture$competitors$name
  fixture_date = raw_json$data$banner$fixture$startDateTime %>% substr(1,10)
  fixture_matchup = paste0(fixture_date,", ",
                           fixture_teamcodes[1]," vs ",fixture_teamcodes[2])
  
  if (ymd(fixture_date)>=today()) break
  
  # Player Stats:
  PP[[i]] = bind_rows(
    suppressWarnings(
      raw_json$data$statistics$data$base$home$persons$rows %>%
        data.frame() %>%
        as_tibble() %>%
        unnest()) %>%
      clean_names("all_caps") %>%
      mutate(TEAM=fixture_teamnames[1],MATCHUP=fixture_matchup,
             GAME_ID=fixture_id,SEASON=season,LEAGUE=league) %>%
      select(GAME_ID,SEASON,LEAGUE,PLAYER=PERSON_NAME,TEAM,MATCHUP,
             MIN=MINUTES,PTS=POINTS,`2PM`=POINTS_TWO_MADE,`2PA`=POINTS_TWO_ATTEMPTED,
             `3PM`=POINTS_THREE_MADE,`3PA`=POINTS_THREE_ATTEMPTED,
             FTA=FREE_THROWS_ATTEMPTED,FTM=FREE_THROWS_MADE,DREB=REBOUNDS_DEFENSIVE,
             OREB=REBOUNDS_OFFENSIVE,REB=REBOUNDS,AST=ASSISTS,STL=STEALS,BLK=BLOCKS,
             TOV=TURNOVERS,PF=FOULS_TOTAL) %>%
      mutate(MIN=gsub("S","",gsub("M",":",gsub("PT","",MIN)))) %>%
      separate(MIN,c("MINS","SEC"),sep=":") %>%
      mutate(MIN=as.numeric(MINS)+if_else(is.na(as.numeric(SEC)),0,as.numeric(SEC)/60)) %>%
      select(1:6,24,9:23),
    suppressWarnings(
      raw_json$data$statistics$data$base$away$persons$rows %>%
        data.frame() %>%
        as_tibble() %>%
        unnest()) %>%
      clean_names("all_caps") %>%
      mutate(TEAM=fixture_teamnames[2],MATCHUP=fixture_matchup,
             GAME_ID=fixture_id,SEASON=season,LEAGUE=league) %>%
      select(GAME_ID,SEASON,LEAGUE,PLAYER=PERSON_NAME,TEAM,MATCHUP,
             MIN=MINUTES,PTS=POINTS,`2PM`=POINTS_TWO_MADE,`2PA`=POINTS_TWO_ATTEMPTED,
             `3PM`=POINTS_THREE_MADE,`3PA`=POINTS_THREE_ATTEMPTED,
             FTA=FREE_THROWS_ATTEMPTED,FTM=FREE_THROWS_MADE,DREB=REBOUNDS_DEFENSIVE,
             OREB=REBOUNDS_OFFENSIVE,REB=REBOUNDS,AST=ASSISTS,STL=STEALS,BLK=BLOCKS,
             TOV=TURNOVERS,PF=FOULS_TOTAL) %>%
      mutate(MIN=gsub("S","",gsub("M",":",gsub("PT","",MIN)))) %>%
      separate(MIN,c("MINS","SEC"),sep=":") %>%
      mutate(MIN=as.numeric(MINS)+if_else(is.na(as.numeric(SEC)),0,as.numeric(SEC)/60)) %>%
      select(1:6,24,9:23)
  ) %>%
    mutate(PLAYER = toupper(as.character(PLAYER)),
           PLAYER = stri_trans_general(PLAYER, "latin-ascii")) %>%
    mutate(MIN=round(MIN)) %>%
    # if minutes is NA, player DNP so remove that row altogether?
    filter(!is.na(MIN)) %>%
    mutate(TEAM = stri_trans_general(toupper(as.character(TEAM)),"latin-ascii"))
  
  # Team Stats:
  TT[[i]] = bind_rows(
    raw_json$data$statistics$data$base$home$entity %>%
      modify_if(is.null, ~ NA) %>%
      as_tibble() %>%
      clean_names("all_caps") %>%
      mutate(TEAM=fixture_teamnames[1],CODE=fixture_teamcodes[1],MATCHUP=fixture_matchup) %>%
      select(TEAM,CODE,MATCHUP,
             PTS=POINTS,`2PM`=POINTS_TWO_MADE,`2PA`=POINTS_TWO_ATTEMPTED,
             `3PM`=POINTS_THREE_MADE,`3PA`=POINTS_THREE_ATTEMPTED,
             FTA=FREE_THROWS_ATTEMPTED,FTM=FREE_THROWS_MADE,DREB=REBOUNDS_DEFENSIVE,
             OREB=REBOUNDS_OFFENSIVE,REB=REBOUNDS,AST=ASSISTS,STL=STEALS,BLK=BLOCKS,
             TOV=TURNOVERS,PF=FOULS_TOTAL),
    raw_json$data$statistics$data$base$away$entity %>%
      modify_if(is.null, ~ NA) %>%
      as_tibble() %>%
      clean_names("all_caps") %>%
      mutate(TEAM=fixture_teamnames[2],CODE=fixture_teamcodes[2],MATCHUP=fixture_matchup) %>%
      select(TEAM,CODE,MATCHUP,
             PTS=POINTS,`2PM`=POINTS_TWO_MADE,`2PA`=POINTS_TWO_ATTEMPTED,
             `3PM`=POINTS_THREE_MADE,`3PA`=POINTS_THREE_ATTEMPTED,
             FTA=FREE_THROWS_ATTEMPTED,FTM=FREE_THROWS_MADE,DREB=REBOUNDS_DEFENSIVE,
             OREB=REBOUNDS_OFFENSIVE,REB=REBOUNDS,AST=ASSISTS,STL=STEALS,BLK=BLOCKS,
             TOV=TURNOVERS,PF=FOULS_TOTAL)
  ) %>%
    mutate(TEAM = stri_trans_general(toupper(as.character(TEAM)),"latin-ascii"))
}
rm(list=setdiff(ls(),c("PP","TT")))

# beepr::beep()
# write files in .csv format
write_csv(bind_rows(PP) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/FR-players.csv")

write_csv(bind_rows(TT) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/FR-teams.csv")
