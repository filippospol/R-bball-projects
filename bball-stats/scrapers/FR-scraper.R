#' *LOAD LIBRARIES*
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
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

# Date and competition parameters:
competition_start_dates = c(
  "2025-09-01","2025-10-01","2025-11-01","2025-12-01","2026-01-01",
  "2026-02-01","2026-03-01","2026-04-01","2026-05-01","2026-06-01"
)
competition_end_dates = c(
  "2025-09-30","2025-10-31","2025-11-30","2025-12-31","2026-01-31",
  "2026-02-28","2026-03-31","2026-04-30","2026-05-31","2026-06-30"
)

# 302 = Regular Season, 308 = Playoffs
competition_ids = c(302, 308)

url_list = c(NA)

for (comp_id in competition_ids) {
  for (i in 1:10) {

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
      url_list = c(url_list,
                   suppressWarnings(
                     raw_calendar$data %>% 
                       data.frame() %>% 
                       as_tibble() %>% 
                       unnest() %>% 
                       pull(match_id) %>% 
                       unique()
                   )
      )
    }
  }
}

# Keep only the match ids and ensure no accidental duplicates:
url_list = url_list[-1]
url_list = unique(url_list)

#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*

# run time around 5 minutes
league="Pro A" ; season="2025-26"
PP = list()
TT = list()
for (i in 1:length(url_list)) {
  # URL (JSON):
  fixture_url = GET(
    url=glue(
      "https://embed-api.eui.connect.sportradar.com/v1/embed/12/fixture_detail?fixtureId={url_list[i]}"
    )
  )
  
  # API data:
  raw_json = suppressMessages(
    fromJSON(content(fixture_url, "text"))
  )
  
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
