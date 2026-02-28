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

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *EXTRACT MATCH ID'S*

# max number of vector indicates number of game days in season:
fixture_info = map_df(1:30, function(MD) {
  res = GET("https://www.legabasket.it/api/championships/get-championships-calendar-by-id?id=596&d={MD}" %>% 
              glue())
  
  raw_json = suppressMessages(
    fromJSON(content(res, "text"))
  )
  
  raw_json$matches %>% 
    as_tibble() %>%
    clean_names("all_caps") %>% 
    select(ID,WEBSOCKET_MATCH_ID,MATCH_DATETIME,
           H_TEAM_NAME,H_CLUB_CODE,V_TEAM_NAME,V_CLUB_CODE)
})
# Get relevant info (columns for next step)
fixture_info = fixture_info %>% 
  mutate(GAME_DATE=as_date(ymd_hms(MATCH_DATETIME)),
         MATCHUP = paste0(GAME_DATE,", ",H_CLUB_CODE," vs ",V_CLUB_CODE)) %>% 
  select(ID,GAME_DATE,MATCHUP,H_TEAM_NAME:V_CLUB_CODE)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*

PP = list()
TT = list()
for (i in 1:dim(fixture_info)[1]) {
  
  if (ymd(fixture_info$GAME_DATE[i])>today()) break
  
  game_id = fixture_info[i,1]
  # URL (JSON):
  fixture_url = "https://www.legabasket.it/_next/data/LJ_cpxPspBbJ6PZKqa8gR/game/{game_id}/tabellini.json?id={game_id}&detail=tabellini" %>% 
    glue()
  
  # API data:
  raw_json = suppressMessages(
    fromJSON(content(GET(fixture_url), "text"))
  )
  
  # Player Stats:
  PP[[i]] = bind_rows(
    raw_json$pageProps$game$scores$ht$rows %>% 
      as_tibble() %>% 
      clean_names("all_caps") %>% 
      rename(MIN2=MIN) %>% 
      mutate(PLAYER=paste0(PLAYER_NAME," ",PLAYER_SURNAME),
             TEAM=fixture_info$H_TEAM_NAME[i],MATCHUP=fixture_info$MATCHUP[i],
             MIN=MIN2,PTS=PUN,`2PM`=T2_R,`2PA`=T2_T,`3PM`=T3_R,`3PA`=T3_T,
             FTM=TL_R,FTA=TL_T,DREB=RIMBALZI_D,OREB=RIMBALZI_O,REB=RIMBALZI_T,
             AST=ASS,STL=PALLE_R,BLK=STOPPATE_DAT,TOV=PALLE_P,PF=FALLI_C),
    raw_json$pageProps$game$scores$vt$rows %>% 
      as_tibble() %>% 
      clean_names("all_caps") %>% 
      rename(MIN2=MIN) %>% 
      mutate(PLAYER=paste0(PLAYER_NAME," ",PLAYER_SURNAME),
             TEAM=fixture_info$V_TEAM_NAME[i],MATCHUP=fixture_info$MATCHUP[i],
             MIN=MIN2,PTS=PUN,`2PM`=T2_R,`2PA`=T2_T,`3PM`=T3_R,`3PA`=T3_T,
             FTM=TL_R,FTA=TL_T,DREB=RIMBALZI_D,OREB=RIMBALZI_O,REB=RIMBALZI_T,
             AST=ASS,STL=PALLE_R,BLK=STOPPATE_DAT,TOV=PALLE_P,PF=FALLI_C)
  ) %>% 
    select(33:51) %>% 
    mutate(PLAYER=toupper(as.character(PLAYER)),
           PLAYER = stri_trans_general(PLAYER, "latin-ascii"))
  
  # Team Stats:
  TT[[i]] = bind_rows(
    raw_json$pageProps$game$scores$ht$totals %>% 
      as_tibble() %>% 
      clean_names("all_caps") %>% 
      mutate(TEAM=fixture_info$H_TEAM_NAME[i],CODE=fixture_info$H_CLUB_CODE[i],
             MATCHUP=fixture_info$MATCHUP[i]) %>% 
      select(TEAM,CODE,MATCHUP,
             PTS=PUN,`2PM`=T2_R,`2PA`=T2_T,`3PM`=T3_R,`3PA`=T3_T,
             FTM=TL_R,FTA=TL_T,DREB=RIMBALZI_D,OREB=RIMBALZI_O,REB=RIMBALZI_T,
             AST=ASS,STL=PALLE_R,BLK=STOPPATE_DAT,TOV=PALLE_P,PF=FALLI_C),
    raw_json$pageProps$game$scores$vt$totals %>% 
      as_tibble() %>% 
      clean_names("all_caps") %>% 
      mutate(TEAM=fixture_info$V_TEAM_NAME[i],CODE=fixture_info$V_CLUB_CODE[i],
             MATCHUP=fixture_info$MATCHUP[i]) %>% 
      select(TEAM,CODE,MATCHUP,
             PTS=PUN,`2PM`=T2_R,`2PA`=T2_T,`3PM`=T3_R,`3PA`=T3_T,
             FTM=TL_R,FTA=TL_T,DREB=RIMBALZI_D,OREB=RIMBALZI_O,REB=RIMBALZI_T,
             AST=ASS,STL=PALLE_R,BLK=STOPPATE_DAT,TOV=PALLE_P,PF=FALLI_C)
  )
  
}
beepr::beep()
rm(list=setdiff(ls(),c("PP","TT")))

# write files in .csv format
write.csv(bind_rows(PP),"IT-players.csv")
write.csv(bind_rows(TT),"IT-teams.csv")