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
league="Euroleague" ; season = "2025-26" ; scode = "E2025" ; games_n = (38*10)+7

GG = list()
for (i in 1:games_n) {

  # Overview tab:
  # Date, Team names and codes from here:
  overview_url = glue("https://live.euroleague.net/api/Header?gamecode={i}&seasoncode={scode}")
  res = GET(overview_url)
  if (content(res, "text", encoding = "UTF-8")=="") next
  
  raw_json = fromJSON(content(res, "text", encoding = "UTF-8"))
  GG[[i]] = raw_json %>% as_tibble() %>% mutate(Game_id=i, .before=1)
  if (i==170) Sys.sleep(5)
}
rm(overview_url,res,raw_json,i)
fixture_info = bind_rows(GG) %>% 
  mutate(SEASON=season,LEAGUE=league, .before=2) %>% 
  mutate(GAME_DATE=dmy(Date),
         HOME_CODE=TVCodeA,AWAY_CODE=TVCodeB,HOME_TEAM=TeamA,AWAY_TEAM=TeamB,
         MATCHUP=paste0(GAME_DATE,", ",HOME_CODE," vs ",AWAY_CODE)) %>% 
  select(GAME_ID=Game_id,SEASON,LEAGUE,MATCHUP,HOME_TEAM,HOME_CODE,AWAY_TEAM,AWAY_CODE)

# Clear environment:
rm(list=setdiff(ls(),c("fixture_info","scode")))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*
PP = list()
TT = list()
for(i in 1:dim(fixture_info)[1]) {
  
  boxscore_url = glue("https://live.euroleague.net/api/Boxscore?gamecode={fixture_info$GAME_ID[i]}&seasoncode={scode}")
  res = GET(boxscore_url)
  raw_json = fromJSON(content(res, "text", encoding = "UTF-8"))
  
  PP[[i]] = suppressWarnings(
    raw_json %>% 
      pluck("Stats") %>% 
      as_tibble() %>% 
      unnest() %>% 
      clean_names("all_caps") %>% 
      mutate(GAME_ID=fixture_info$GAME_ID[i],SEASON=fixture_info$SEASON %>% unique(),
             LEAGUE=fixture_info$LEAGUE %>% unique(),MATCHUP=fixture_info$MATCHUP[i],
             PLAYER = str_replace(PLAYER, "^(.*),\\s*(.*)$", "\\2 \\1"),
             MINUTES = round(period_to_seconds(ms(MINUTES)) / 60)) %>% 
      select(GAME_ID,SEASON,LEAGUE,PLAYER,TEAM,MATCHUP,MIN=MINUTES,PTS=POINTS,`2PM`=FIELD_GOALS_MADE2,
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
rm(list=setdiff(ls(),c("PP","TT")))

# beepr::beep()
# write files in .csv format
write.csv(bind_rows(PP) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/EL-players.csv")


write.csv(bind_rows(TT) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/EL-teams.csv")


