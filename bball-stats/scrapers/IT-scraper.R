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

#' *EXTRACT MATCH ID'S*

# max number of vector indicates number of game days in season:
league="Serie A" ; season = "2025-26"
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
         MATCHUP = paste0(GAME_DATE,", ",H_CLUB_CODE," vs ",V_CLUB_CODE),
         LEAGUE=league,SEASON=season) %>% 
  select(GAME_ID=ID,SEASON,LEAGUE,GAME_DATE,MATCHUP,H_TEAM_NAME:V_CLUB_CODE)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*

#' *DYNAMICALLY FETCH TODAY'S NEXT.JS BUILD ID*
main_page = read_html("https://www.legabasket.it")
next_data_json = main_page %>% html_node("#__NEXT_DATA__") %>% html_text()
build_id = fromJSON(next_data_json)$buildId


#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*
PP = list()
TT = list()

for (i in 1:nrow(fixture_info)) {
  
  # Be a polite bot
  Sys.sleep(0.5)
  
  if (ymd(fixture_info$GAME_DATE[i]) >= today()) break
  
  # FIX 1: Properly extract the game ID as a single character/numeric value
  game_id = fixture_info$GAME_ID[i]
  
  # FIX 2: Inject the dynamic build_id into the URL
  fixture_url = glue("https://www.legabasket.it/_next/data/{build_id}/game/{game_id}/tabellini.json?id={game_id}&detail=tabellini")
  
  # Fetch data with a safety net (so one broken game doesn't ruin the whole loop)
  res = GET(fixture_url)
  
  if (status_code(res) == 200) {
    
    # Only parse if we confirm it is actual JSON text
    raw_text <- content(res, "text", encoding = "UTF-8")
    
    if (str_starts(str_trim(raw_text), "\\{")) {
      raw_json <- suppressMessages(fromJSON(raw_text))
      
      # Player Stats:
      PP[[i]] = bind_rows(
        raw_json$pageProps$game$scores$ht$rows %>% as_tibble() %>% clean_names("all_caps") %>%
          rename(MIN2=MIN) %>% mutate(PLAYER=paste0(PLAYER_NAME," ",PLAYER_SURNAME),
                                      TEAM=stri_trans_general(toupper(fixture_info$H_TEAM_NAME[i]),"latin-ascii"),
                                      MATCHUP=fixture_info$MATCHUP[i],
                                      MIN=MIN2,PTS=PUN,`2PM`=T2_R,`2PA`=T2_T,`3PM`=T3_R,`3PA`=T3_T,
                                      FTM=TL_R,FTA=TL_T,DREB=RIMBALZI_D,OREB=RIMBALZI_O,REB=RIMBALZI_T,
                                      AST=ASS,STL=PALLE_R,BLK=STOPPATE_DAT,TOV=PALLE_P,PF=FALLI_C,
                                      SEASON=fixture_info$SEASON[i],LEAGUE=fixture_info$LEAGUE[i],
                                      GAME_ID=fixture_info$GAME_ID[i]),
        raw_json$pageProps$game$scores$vt$rows %>% as_tibble() %>% clean_names("all_caps") %>%
          rename(MIN2=MIN) %>% mutate(PLAYER=paste0(PLAYER_NAME," ",PLAYER_SURNAME),
                                      TEAM=stri_trans_general(toupper(fixture_info$V_TEAM_NAME[i]),"latin-ascii"),,
                                      MATCHUP=fixture_info$MATCHUP[i],
                                      MIN=MIN2,PTS=PUN,`2PM`=T2_R,`2PA`=T2_T,`3PM`=T3_R,`3PA`=T3_T,
                                      FTM=TL_R,FTA=TL_T,DREB=RIMBALZI_D,OREB=RIMBALZI_O,REB=RIMBALZI_T,
                                      AST=ASS,STL=PALLE_R,BLK=STOPPATE_DAT,TOV=PALLE_P,PF=FALLI_C,
                                      SEASON=fixture_info$SEASON[i],LEAGUE=fixture_info$LEAGUE[i],
                                      GAME_ID=fixture_info$GAME_ID[i])
      ) %>%
        select(GAME_ID,SEASON,LEAGUE,PLAYER,TEAM,MATCHUP,MIN,PTS,
               `2PM`,`2PA`,`3PM`,`3PA`,FTM,FTA,DREB,OREB,REB,AST,STL,TOV,BLK,PF) %>%
        mutate(PLAYER=toupper(as.character(PLAYER)),
               PLAYER = stri_trans_general(PLAYER, "latin-ascii")) %>%
        mutate(MIN=round(MIN)) %>%
        filter(!is.na(MIN))
      
      # Team Stats:
      TT[[i]] = bind_rows(
        raw_json$pageProps$game$scores$ht$totals %>% as_tibble() %>% clean_names("all_caps") %>%
          mutate(TEAM=stri_trans_general(toupper(fixture_info$H_TEAM_NAME[i]),"latin-ascii"),,
                 CODE=fixture_info$H_CLUB_CODE[i], MATCHUP=fixture_info$MATCHUP[i]) %>%
          select(TEAM,CODE,MATCHUP, PTS=PUN,`2PM`=T2_R,`2PA`=T2_T,`3PM`=T3_R,`3PA`=T3_T,
                 FTM=TL_R,FTA=TL_T,DREB=RIMBALZI_D,OREB=RIMBALZI_O,REB=RIMBALZI_T,
                 AST=ASS,STL=PALLE_R,BLK=STOPPATE_DAT,TOV=PALLE_P,PF=FALLI_C),
        raw_json$pageProps$game$scores$vt$totals %>% as_tibble() %>% clean_names("all_caps") %>%
          mutate(TEAM=stri_trans_general(toupper(fixture_info$V_TEAM_NAME[i]),"latin-ascii"),,
                 CODE=fixture_info$V_CLUB_CODE[i], MATCHUP=fixture_info$MATCHUP[i]) %>%
          select(TEAM,CODE,MATCHUP, PTS=PUN,`2PM`=T2_R,`2PA`=T2_T,`3PM`=T3_R,`3PA`=T3_T,
                 FTM=TL_R,FTA=TL_T,DREB=RIMBALZI_D,OREB=RIMBALZI_O,REB=RIMBALZI_T,
                 AST=ASS,STL=PALLE_R,BLK=STOPPATE_DAT,TOV=PALLE_P,PF=FALLI_C)
      )
      
    } else {
      print(paste("Skipping game", game_id, "- Server returned HTML instead of JSON."))
    }
  } else {
    print(paste("Skipping game", game_id, "- API Error:", status_code(res)))
  }
}
rm(list=setdiff(ls(),c("PP","TT")))

# beepr::beep()
# write files in .csv format
write.csv(bind_rows(PP) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/IT-players.csv")

write.csv(bind_rows(TT) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/IT-teams.csv")
