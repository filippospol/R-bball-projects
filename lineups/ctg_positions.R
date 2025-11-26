# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script gives player positions in `Cleaning the Glass` format for a given season
# Point - Combo - Wing - Forward - Big
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ctg_positions = function(season) {
  # Load necessary functions and libraries:
  pacman::p_load(tidyverse,rvest,hoopR,janitor,glue)
  source("https://raw.githubusercontent.com/filippospol/R-bball-projects/refs/heads/main/mergeStats.R")
  source("https://raw.githubusercontent.com/filippospol/R-bball-projects/refs/heads/main/scraper%20functions/bbref/bbref_play_by_play.R")
  
  # Scrape necessary data and merge into one table:
  playersNBA = nba_leaguedashplayerstats(season=season, measure_type="Base") %>% 
    pluck(1) %>% 
    select(PLAYER_ID,PLAYER_NAME,TEAM_NBA=TEAM_ABBREVIATION)
  
  playersBBRef = bbref_play_by_play(season=season) %>% 
    select(PLAYER_NAME,SEASON,POS,PG_PERCENT:C_PERCENT)
  
  playersAll = mergeStats(playersNBA,playersBBRef)
  rm(list=setdiff(ls(),"playersAll"))
  
  # Estimate positions:
  playersAll %>% 
    mutate_at(6:10, ~round(as.numeric(.)/100,3)) %>% 
    mutate(POSITION = case_when(
      PG_PERCENT>=0.80 ~ "Point",
      PG_PERCENT>SG_PERCENT | (PG_PERCENT>=0.5 | SG_PERCENT>=0.5) & PG_PERCENT+SG_PERCENT>0.8 ~ "Combo",
      PG_PERCENT+SG_PERCENT+SF_PERCENT>=0.85| SG_PERCENT>=0.85 | SF_PERCENT>=0.85 | (SG_PERCENT+SF_PERCENT>=0.7) | SG_PERCENT>SF_PERCENT ~ "Wing",
      C_PERCENT<0.1 & SF_PERCENT>=0.65 | PF_PERCENT>=0.65 | SF_PERCENT+PF_PERCENT>=0.75 ~ "Forward",
      TRUE ~ "Big"
    )) %>% 
    select(SEASON,PLAYER_ID,PLAYER_NAME,TEAM=TEAM_NBA,CTG_POS=POSITION) %>% 
    return()

  }
