# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes play-by-play stats from Basketball Reference.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

bbref_play_by_play = function(season) {
  # pacman::p_load(tidyverse,rvest,janitor,glue)
  library(dplyr)
  library(rvest)
  library(rvest)
  library(janitor)
  library(purrr)
  
  suppressWarnings(
    "https://www.basketball-reference.com/leagues/NBA_{paste0(20,substr(season,6,7))}_play-by-play.html" %>% 
      glue() %>% 
      read_html() %>% 
      html_elements("table") %>% 
      html_table() %>% 
      pluck(1) %>% 
      row_to_names(row_number=1) %>% 
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
      arrange(PLAYER) %>% 
      mutate(SEASON=season)
  ) %>% 
    select(SEASON,PLAYER_NAME=PLAYER,POS,PG_PERCENT:C_PERCENT,TOV_BAD_PASS=BAD_PASS,TOV_LOST_BALL=LOST_BALL,
           SHOOT_PF=SHOOT,OFF_PF=OFF,SHOOT_PFD=SHOOT_2,OFF_PFD=OFF_2,AST_PTS=PGA,AND1,BLKD_FGA=BLKD) %>% 
    return()
}


