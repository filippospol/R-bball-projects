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

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Finished games IDs:

# Initial parameters:
params = list(
  currentPage = 1,
  pageSize = "9",
  gameType = "finished"
)
# Extract JSON:
res = GET(url = "https://api.basketball-bundesliga.de/games",
          add_headers(.headers=headers),
          query = params)
raw_json = fromJSON(content(res, "text", encoding = "UTF-8"))

# Number of pages in website:
fixture_finished_pages = raw_json$totalPages
rm(params,res)

FF = list()
options(warn = -1)
for (i in 1:fixture_finished_pages) {
  
  # Set parameters:
  params = list(
    currentPage = i,
    pageSize = "9",
    gameType = "finished"
  )
  # Extract JSON:
  res = GET(url = "https://api.basketball-bundesliga.de/games",
            add_headers(.headers=headers),
            query = params)
  
  if (res$status_code != 200) break
  
  raw_json = fromJSON(content(res, "text", encoding = "UTF-8"))
  
  FF[[i]] = raw_json$items %>% 
    as_tibble() %>% 
    select(ID=id, GAME_DATE=scheduledTime,HOME=homeTeam,AWAY=guestTeam) %>% 
    unnest() %>% 
    select(ID,GAME_DATE,HOME_TEAM=name,AWAY_TEAM=name1)
  
}
options(warn = 1)
fixture_finished = bind_rows(FF)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Scheduled games IDs:

# Initial parameters:
params = list(
  currentPage = 1,
  pageSize = "9",
  gameType = "scheduled"
)
# Extract JSON:
res = GET(url = "https://api.basketball-bundesliga.de/games",
          add_headers(.headers=headers),
          query = params)
raw_json = fromJSON(content(res, "text", encoding = "UTF-8"))

# Number of pages in website:
fixture_scheduled_pages = raw_json$totalPages
rm(params,res)

FF = list()
options(warn = -1)
for (i in 1:fixture_scheduled_pages) {
  
  # Set parameters:
  params = list(
    currentPage = i,
    pageSize = "9",
    gameType = "scheduled"
  )
  # Extract JSON:
  res = GET(url = "https://api.basketball-bundesliga.de/games",
            add_headers(.headers=headers),
            query = params)
  
  raw_json = fromJSON(content(res, "text", encoding = "UTF-8"))
  
  if (length(raw_json$items) == 0) break
  
  FF[[i]] = raw_json$items %>% 
    as_tibble() %>% 
    select(ID=id, GAME_DATE=scheduledTime,HOME=homeTeam,AWAY=guestTeam) %>% 
    unnest() %>% 
    select(ID,GAME_DATE,HOME_TEAM=name,AWAY_TEAM=name1)
  
}
options(warn = 1)
fixture_scheduled = bind_rows(FF)

# bind all game information togetherL
fixture_info = bind_rows(fixture_finished,fixture_scheduled) %>% 
  mutate(GAME_DATE=as_date(ymd_hms(GAME_DATE))) %>% 
  arrange(GAME_DATE)

# Clear environment:
rm(list=setdiff(ls(),c("headers","fixture_info")))

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
rm(base_url,fixture_page,next_data_json)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*

season="2025-26" ; league="BBL"
PP = list()
TT = list()
for (i in 1:dim(fixture_info)[1]) {
  
  if (ymd(fixture_info$GAME_DATE[i])>today()) break
  
  res = GET(url = glue("https://www.easycredit-bbl.de/_next/data/{daily_key}/de-DE/spiele/{fixture_info$ID[i]}.json?id={fixture_info$ID[i]}"))
  
  raw_json = fromJSON(content(res, "text", encoding = "UTF-8"))
  
  if (raw_json$pageProps$initialGameStats$homeTeam$gameStat$competition[1] == "BBL_CUP") next
  
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
  MATCHUP =  paste0(fixture_info$GAME_DATE[i],", ",
                    homeBox$CODE[1]," vs ",awayBox$CODE[1])
  
  # Players Boxscore:
  PP[[i]] = bind_rows(homeBox,awayBox) %>% 
    clean_names("all_caps") %>% 
    mutate(GAME_ID=fixture_info$ID[i],SEASON=season,LEAGUE=league,
           MATCHUP=MATCHUP,
           MIN=round(as.numeric(SECONDS_PLAYED)/60,1),
           PLAYER=stri_trans_general(
             toupper(paste0(FIRST_NAME," ",LAST_NAME)), "latin-ascii")) %>% 
    select(GAME_ID,SEASON,LEAGUE,PLAYER,TEAM,MATCHUP,MIN,PTS=POINTS,
           `2PM`=TWO_POINT_SHOTS_MADE,`2PA`=TWO_POINT_SHOTS_ATTEMPTED,
           `3PM`=THREE_POINT_SHOTS_MADE,`3PA`=THREE_POINT_SHOTS_ATTEMPTED,
           FTM=FREE_THROWS_MADE,FTA=FREE_THROWS_ATTEMPTED,
           DREB=DEFENSIVE_REBOUNDS,OREB=OFFENSIVE_REBOUNDS,
           REB=TOTAL_REBOUNDS,AST=ASSISTS,STL=STEALS,BLK=BLOCKS,
           TOV=TURNOVERS,PF=FOULS_COMMITTED) %>% 
    mutate(MIN=round(MIN)) %>% 
    # if minutes is NA, player DNP so remove that row altogether?
    filter(!is.na(MIN)) %>% 
    mutate_at(7:22, as.numeric)
  # homeTeam = homeBox$TEAM %>% unique() ; homeCode = homeBox$CODE %>% unique()
  # awayTeam = awayBox$TEAM %>% unique() ; awayCode = awayBox$CODE %>% unique()
  homeTeam = homeBox$TEAM[1] ; homeCode = homeBox$CODE[1]
  awayTeam = awayBox$TEAM[1] ; awayCode = awayBox$CODE[1]
  rm(homeBox,awayBox)
  
  # Teamms Boxscore:
  TT[[i]] = bind_rows(
    raw_json$pageProps$initialGameStats$homeTeam$gameStat %>% 
      as_tibble() %>% 
      head(1) %>% 
      clean_names("all_caps") %>% 
      mutate(TEAM=homeTeam,CODE=homeCode,MATCHUP=MATCHUP) %>% 
      select(TEAM,CODE,MATCHUP,PTS=POINTS,
             `2PM`=TWO_POINT_SHOTS_MADE,`2PA`=TWO_POINT_SHOTS_ATTEMPTED,
             `3PM`=THREE_POINT_SHOTS_MADE,`3PA`=THREE_POINT_SHOTS_ATTEMPTED,
             FTM=FREE_THROWS_MADE,FTA=FREE_THROWS_ATTEMPTED,
             DREB=DEFENSIVE_REBOUNDS,OREB=OFFENSIVE_REBOUNDS,
             REB=TOTAL_REBOUNDS,AST=ASSISTS,STL=STEALS,BLK=BLOCKS,
             TOV=TURNOVERS,PF=FOULS_COMMITTED),
    raw_json$pageProps$initialGameStats$guestTeam$gameStat %>% 
      as_tibble() %>% 
      head(1) %>% 
      clean_names("all_caps") %>% 
      mutate(TEAM=awayTeam,CODE=awayCode,MATCHUP=MATCHUP) %>% 
      select(TEAM,CODE,MATCHUP,PTS=POINTS,
             `2PM`=TWO_POINT_SHOTS_MADE,`2PA`=TWO_POINT_SHOTS_ATTEMPTED,
             `3PM`=THREE_POINT_SHOTS_MADE,`3PA`=THREE_POINT_SHOTS_ATTEMPTED,
             FTM=FREE_THROWS_MADE,FTA=FREE_THROWS_ATTEMPTED,
             DREB=DEFENSIVE_REBOUNDS,OREB=OFFENSIVE_REBOUNDS,
             REB=TOTAL_REBOUNDS,AST=ASSISTS,STL=STEALS,BLK=BLOCKS,
             TOV=TURNOVERS,PF=FOULS_COMMITTED)
  )  
  print(i)
}
rm(list=setdiff(ls(),c("PP","TT")))

# beepr::beep()
# write files in .csv format
write.csv(bind_rows(PP) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/DE-players.csv")
write.csv(bind_rows(TT) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/DE-teams.csv")