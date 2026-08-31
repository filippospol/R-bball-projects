# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script extracts box-score data for the Euroleague. 
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

#' *EXTRACT MATCH ID'S AND INFORMATION*

# season code:
# 20 teams, they play each other twice, so 19*2=38 rounds and 38*10=380 RS games
# 3 play-in games
# playoffs: best of 5 series
# final four: semi finals and first place game
league="Euroleague" ; season = "2025-26" ; scode = "E2025" ; games_n = (38*10)+26

games_url = "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/{substr(scode,1,1)}/seasons/{scode}/games" %>% 
  glue()
res  = GET(games_url, query = list(limit = 600))
game_list = fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

raw_fixtures = game_list$data %>% 
  as_tibble() %>% 
  select(where(~!is.list(.)))

fixture_info = raw_fixtures %>% 
  clean_names("all_caps") %>% 
  filter(STATUS == "result") %>% 
  mutate(SEASON=season,LEAGUE=league,
         GAME_DATE=as.Date(DATE),
         HOME_TEAM=HOME_ABBREVIATED_NAME,AWAY_TEAM=AWAY_ABBREVIATED_NAME,
         MATCHUP=paste0(GAME_DATE,", ",HOME_CODE," vs ",AWAY_CODE)) %>% 
  select(GAME_ID=CODE,SEASON,LEAGUE,MATCHUP,HOME_TEAM,HOME_CODE,AWAY_TEAM,AWAY_CODE) %>% 
  arrange(GAME_ID)

# Clear environment:
rm(list=setdiff(ls(),c("fixture_info","scode")))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*
PP = list()
TT = list()
for(i in 1:dim(fixture_info)[1]) {
  
  Sys.sleep(1)
  boxscore_url = glue("https://live.euroleague.net/api/Boxscore?gamecode={fixture_info$GAME_ID[i]}&seasoncode={scode}")
  res = GET(boxscore_url)
  message(paste0("Game id ",fixture_info$GAME_ID[i]),": status code ",res$status_code)
  raw_json = fromJSON(content(res, "text", encoding = "UTF-8"))
  
  PP[[i]] = suppressWarnings(
    raw_json %>% 
      pluck("Stats") %>% 
      as_tibble() %>% 
      # team code, so the cleanup at the end has a stable key to group on.
      # same home-then-away assumption the TT block below already makes
      mutate(CODE = c(fixture_info$HOME_CODE[i],fixture_info$AWAY_CODE[i]),
             TEAM2 = c(fixture_info$HOME_TEAM[i],fixture_info$AWAY_TEAM[i])) %>% 
      unnest() %>% 
      clean_names("all_caps") %>% 
      mutate(GAME_ID=fixture_info$GAME_ID[i],SEASON=fixture_info$SEASON %>% unique(),
             LEAGUE=fixture_info$LEAGUE %>% unique(),MATCHUP=fixture_info$MATCHUP[i],
             PLAYER = str_replace(PLAYER, "^(.*),\\s*(.*)$", "\\2 \\1"),
             MINUTES = round(period_to_seconds(ms(MINUTES)) / 60)) %>% 
      select(GAME_ID,SEASON,LEAGUE,PLAYER,TEAM=TEAM2,CODE,MATCHUP,MIN=MINUTES,PTS=POINTS,`2PM`=FIELD_GOALS_MADE2,
             `2PA`=FIELD_GOALS_ATTEMPTED2,`3PM`=FIELD_GOALS_MADE3,`3PA`=FIELD_GOALS_ATTEMPTED3,FTM=FREE_THROWS_MADE,
             FTA=FREE_THROWS_ATTEMPTED,DREB=DEFENSIVE_REBOUNDS,OREB=OFFENSIVE_REBOUNDS,REB=TOTAL_REBOUNDS,AST=ASSISTANCES,STL=STEALS,
             TOV=TURNOVERS,BLK=BLOCKS_FAVOUR,PF=FOULS_COMMITED) %>% 
      filter(!is.na(MIN))
  )
  
  TT[[i]] = suppressWarnings(
    raw_json$Stats$totr %>% 
      as_tibble() %>% 
      mutate(TEAM=c(fixture_info$HOME_TEAM[i],fixture_info$AWAY_TEAM[i]),
             CODE=c(fixture_info$HOME_CODE[i],fixture_info$AWAY_CODE[i]),
             MATCHUP=fixture_info$MATCHUP[i]) %>% 
      clean_names("all_caps") %>% 
      select(TEAM,CODE,MATCHUP,PTS=POINTS,`2PM`=FIELD_GOALS_MADE2,
             `2PA`=FIELD_GOALS_ATTEMPTED2,`3PM`=FIELD_GOALS_MADE3,`3PA`=FIELD_GOALS_ATTEMPTED3,FTM=FREE_THROWS_MADE,
             FTA=FREE_THROWS_ATTEMPTED,DREB=DEFENSIVE_REBOUNDS,OREB=OFFENSIVE_REBOUNDS,REB=TOTAL_REBOUNDS,AST=ASSISTANCES,STL=STEALS,
             TOV=TURNOVERS,BLK=BLOCKS_FAVOUR,PF=FOULS_COMMITED)
  )
}
rm(list=setdiff(ls(),c("PP","TT","fixture_info")))

players = bind_rows(PP)
teams   = bind_rows(TT)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# beepr::beep()
# write files in .csv format
write.csv(players,"bball-stats/data/EL-players.csv")
write.csv(teams,"bball-stats/data/EL-teams.csv")
