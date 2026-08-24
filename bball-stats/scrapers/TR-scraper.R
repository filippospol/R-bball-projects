# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script extracts box-score data for the Turkish BSL.
# Match ids/dates/teams come from tbsl-fixtures.json (saved once per season via the browser
# console snippet tbsl-fixtures.js). Boxscores come from FIBA LiveStats by geniusId.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOAD LIBRARIES*
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(stringr)
library(stringi)
library(httr)
library(jsonlite)
library(glue)
library(janitor)
library(lubridate)
library(vroom)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *BEFORE STARTING*
# To get the fixture list, at the start of every season (and before the playoffs),
# use the tbsl-fixtures-generator.js file as follows:
# https://www.tbf.org.tr/ligler/bsl-2026-2027 > DevTools, Console Tab >
# > allow pasting > paste the contents of the file > press Enter
# The browser will generate a file named `tbsl-fixtures.json`
# Replace the new file with the one already found on Github.

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *EXTRACT MATCH ID'S*
# From tbsl-fixtures.json (geniusId = FIBA LiveStats id, date, team names), sorted by date:
fixtures = fromJSON("https://raw.githubusercontent.com/filippospol/R-bball-projects/refs/heads/main/bball-stats/scrapers/tbsl-fixtures.json") %>%
  as_tibble() %>%
  filter(!is.na(geniusId) & geniusId != "") %>%
  arrange(date)

url_list = fixtures$geniusId

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*

league="TBSL" ; season="2025-26"
PP = list()
TT = list()
for (i in 1:length(url_list)) {
  # URL (JSON):
  fixture_url = GET(
    url=glue(
      "https://fibalivestats.dcd.shared.geniussports.com/data/{url_list[i]}/data.json"
    )
  )
  
  # API data:
  raw_json = suppressMessages(
    tryCatch(fromJSON(content(fixture_url, "text", encoding="UTF-8")), error=function(e) NULL)
  )
  
  # if the game hasn't been played yet there are no player stats -> stop:
  if (is.null(raw_json) || length(raw_json$tm$`1`$pl)==0) break
  
  # Fixture info (dates + team names from the fixtures file):
  fixture_id = url_list[i]
  fixture_teamnames = c(fixtures$home[i],fixtures$away[i])
  fixture_date = fixtures$date[i]
  fixture_teamcodes = c(raw_json$tm$`1`$code, raw_json$tm$`2`$code)
  fixture_matchup = paste0(fixture_date,", ",
                           fixture_teamcodes[1]," vs ",fixture_teamcodes[2])
  
  # Player Stats (FLS fields: sMinutes as MM:SS, twoPointers = made, etc.):
  PP[[i]] = bind_rows(
    raw_json$tm$`1`$pl %>%
      bind_rows() %>%
      mutate(TEAM=fixture_teamnames[1],MATCHUP=fixture_matchup,
             GAME_ID=fixture_id,SEASON=season,LEAGUE=league,
             PLAYER=paste(firstName,familyName)) %>%
      select(GAME_ID,SEASON,LEAGUE,PLAYER,TEAM,MATCHUP,
             MIN=sMinutes,PTS=sPoints,`2PM`=sTwoPointersMade,`2PA`=sTwoPointersAttempted,
             `3PM`=sThreePointersMade,`3PA`=sThreePointersAttempted,
             FTA=sFreeThrowsAttempted,FTM=sFreeThrowsMade,DREB=sReboundsDefensive,
             OREB=sReboundsOffensive,REB=sReboundsTotal,AST=sAssists,STL=sSteals,BLK=sBlocks,
             TOV=sTurnovers,PF=sFoulsPersonal) %>%
      separate(MIN,c("MINS","SEC"),sep=":") %>%
      mutate(MIN=as.numeric(MINS)+if_else(is.na(as.numeric(SEC)),0,as.numeric(SEC)/60)) %>%
      select(1:6,24,9:23),
    raw_json$tm$`2`$pl %>%
      bind_rows() %>%
      mutate(TEAM=fixture_teamnames[2],MATCHUP=fixture_matchup,
             GAME_ID=fixture_id,SEASON=season,LEAGUE=league,
             PLAYER=paste(firstName,familyName)) %>%
      select(GAME_ID,SEASON,LEAGUE,PLAYER,TEAM,MATCHUP,
             MIN=sMinutes,PTS=sPoints,`2PM`=sTwoPointersMade,`2PA`=sTwoPointersAttempted,
             `3PM`=sThreePointersMade,`3PA`=sThreePointersAttempted,
             FTA=sFreeThrowsAttempted,FTM=sFreeThrowsMade,DREB=sReboundsDefensive,
             OREB=sReboundsOffensive,REB=sReboundsTotal,AST=sAssists,STL=sSteals,BLK=sBlocks,
             TOV=sTurnovers,PF=sFoulsPersonal) %>%
      separate(MIN,c("MINS","SEC"),sep=":") %>%
      mutate(MIN=as.numeric(MINS)+if_else(is.na(as.numeric(SEC)),0,as.numeric(SEC)/60)) %>%
      select(1:6,24,9:23)
  ) %>%
    mutate(PLAYER = toupper(as.character(PLAYER)),
           PLAYER = stri_trans_general(PLAYER, "latin-ascii")) %>%
    mutate(MIN=round(MIN)) %>%
    # if minutes is 0, player DNP so remove that row altogether:
    filter(MIN>0) %>%
    mutate(TEAM = stri_trans_general(toupper(as.character(TEAM)),"latin-ascii"))
  
  # Team Stats (tot_-prefixed totals on each side):
  TT[[i]] = bind_rows(
    raw_json$tm$`1` %>%
      `[`(c("tot_sPoints","tot_sTwoPointersMade","tot_sTwoPointersAttempted",
            "tot_sThreePointersMade","tot_sThreePointersAttempted","tot_sFreeThrowsAttempted",
            "tot_sFreeThrowsMade","tot_sReboundsDefensive","tot_sReboundsOffensive",
            "tot_sReboundsTotal","tot_sAssists","tot_sSteals","tot_sBlocks",
            "tot_sTurnovers","tot_sFoulsPersonal")) %>%
      as_tibble() %>%
      mutate(TEAM=fixture_teamnames[1],MATCHUP=fixture_matchup) %>%
      select(TEAM,MATCHUP,
             PTS=tot_sPoints,`2PM`=tot_sTwoPointersMade,`2PA`=tot_sTwoPointersAttempted,
             `3PM`=tot_sThreePointersMade,`3PA`=tot_sThreePointersAttempted,
             FTA=tot_sFreeThrowsAttempted,FTM=tot_sFreeThrowsMade,DREB=tot_sReboundsDefensive,
             OREB=tot_sReboundsOffensive,REB=tot_sReboundsTotal,AST=tot_sAssists,STL=tot_sSteals,
             BLK=tot_sBlocks,TOV=tot_sTurnovers,PF=tot_sFoulsPersonal),
    raw_json$tm$`2` %>%
      `[`(c("tot_sPoints","tot_sTwoPointersMade","tot_sTwoPointersAttempted",
            "tot_sThreePointersMade","tot_sThreePointersAttempted","tot_sFreeThrowsAttempted",
            "tot_sFreeThrowsMade","tot_sReboundsDefensive","tot_sReboundsOffensive",
            "tot_sReboundsTotal","tot_sAssists","tot_sSteals","tot_sBlocks",
            "tot_sTurnovers","tot_sFoulsPersonal")) %>%
      as_tibble() %>%
      mutate(TEAM=fixture_teamnames[2],MATCHUP=fixture_matchup) %>%
      select(TEAM,MATCHUP,
             PTS=tot_sPoints,`2PM`=tot_sTwoPointersMade,`2PA`=tot_sTwoPointersAttempted,
             `3PM`=tot_sThreePointersMade,`3PA`=tot_sThreePointersAttempted,
             FTA=tot_sFreeThrowsAttempted,FTM=tot_sFreeThrowsMade,DREB=tot_sReboundsDefensive,
             OREB=tot_sReboundsOffensive,REB=tot_sReboundsTotal,AST=tot_sAssists,STL=tot_sSteals,
             BLK=tot_sBlocks,TOV=tot_sTurnovers,PF=tot_sFoulsPersonal)
  ) %>%
    mutate(TEAM = stri_trans_general(toupper(as.character(TEAM)),"latin-ascii"))
  
  Sys.sleep(0.5)
}
rm(list=setdiff(ls(),c("PP","TT")))

# write files in .csv format
vroom_write(bind_rows(PP) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/TR-players.csv")
vroom_write(bind_rows(TT) %>% mutate(TEAM=toupper(TEAM)),"bball-stats/data/TR-teams.csv")
