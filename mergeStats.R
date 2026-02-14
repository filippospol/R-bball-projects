# pacman::p_load(tidyverse,hoopR,rvest,glue,janitor)
library(dplyr)
library(rvest)
library(hoopR)
library(janitor)
library(purrr)
library(glue)

mergeStats = function(nbacom,bbref) {
  # Make sure objects are correct.
  # We need the first one to be from NBA.com and the second one from Basketball Reference:
  if (!"PLAYER_ID" %in% names(nbacom)) {
    stop("Error: the `nbacom` argument MUST be from NBA Stats! Try again.")
  }
  if (!"POS" %in% names(bbref)) {
    stop("Error: the `bbref` argument MUST be from Basketball Reference! Try again.")
  }
  
  # Use edit distance on the player name columns to match the rows
  # names(nbacom) ; names(bbref)
  edd <- adist(bbref$PLAYER_NAME, nbacom$PLAYER_NAME)
  ind <- rep(NA,ncol(edd))
  for (i in 1:nrow(nbacom)) {
    ind[i] <- which.min(edd[,i])
  }
  bbref = bbref[ind,] # %>% na.omit()
  merged = suppressMessages(
    bind_cols(nbacom,bbref %>% select(-c(PLAYER_NAME)))
  )
  rm(edd,ind,i,bbref,nbacom) ; return(merged)
}

# # NOT RUN
# # Example:
# pacman::p_load(tidyverse,hoopR,rvest,glue,janitor)
# source("https://raw.githubusercontent.com/filippospol/R-bball-projects/refs/heads/main/scraper%20functions/bbref/bbref_advanced.R")
# 
# # Get the data:
# season="2021-22"
# nbacom = nba_leaguedashplayerstats(season=season,per_mode="PerGame") %>% 
#   pluck(1)
# bbref = bbref_advanced(season=season)
# # Merge them together:
# mydf = mergeStats(nbacom,bbref)


