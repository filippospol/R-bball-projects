# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes advanced stats from Basketball Reference.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

bbref_advanced = function(season) {
  # pacman::p_load(tidyverse,rvest,janitor,glue)
  library(dplyr)
  library(rvest)
  library(rvest)
  library(janitor)
  library(purrr)
  
  suppressWarnings(
    "https://www.basketball-reference.com/leagues/NBA_{paste0(20,substr(season,6,7))}_advanced.html" %>% 
      glue() %>% 
      read_html() %>% 
      html_elements("table") %>% 
      html_table() %>% 
      pluck(1) %>% 
      clean_names("all_caps") %>% 
      mutate(TEAM = case_when(
        TEAM=="BRK" ~ "BKN",
        TEAM=="CHO" ~ "CHA",
        TEAM=="PHO" ~ "PHX",
        TRUE ~ TEAM
      )) %>% 
      group_by(PLAYER) %>% 
      mutate(TEAM=gsub("2TM|3TM|4TM","",paste0(TEAM,collapse="/")),
             TEAM=gsub("^/","",TEAM)) %>% 
      filter(as.numeric(MP)==max(as.numeric(MP))) %>%
      ungroup() %>% 
      arrange(PLAYER)
  ) %>% 
    mutate(SEASON=season) %>% 
    select(SEASON,PLAYER_NAME=PLAYER,TEAM,POS,AGE,GP=G,MIN=MP,PER,TS_PCT=TS_PERCENT,FG3_RATE=X3P_AR,FTA_RATE=F_TR,OREB_PCT=ORB_PERCENT,DREB_PCT=DRB_PERCENT,
           AST_PCT=AST_PERCENT,STL_PCT=STL_PERCENT,BLK_PCT=BLK_PERCENT,TOV_PCT=TOV_PERCENT,USG_PCT=USG_PERCENT,PER,OWS:WS_48,OBPM:VORP) %>% 
    return()
}


