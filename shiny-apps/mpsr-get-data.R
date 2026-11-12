# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes and cleans historical data from Basketball Reference for the MPSR app:
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *SETUP ENVIRONMENT*
pacman::p_load(hoopR,tidyverse,janitor,rvest,glue,readr)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *PLAYOFF SERIES DATA*

# Series-by-series raw data:
series = suppressWarnings(
  "https://www.basketball-reference.com/playoffs/series.html" %>% 
    read_html() %>% 
    html_elements("table") %>% 
    html_table() %>% 
    pluck(1) %>% 
    row_to_names(row_number=1) %>% 
    clean_names("all_caps")
)

# Series-by-series clean data:
series_adv = series %>%  
  suppressWarnings() %>% 
  select(SEASON=YR,ROUND=SERIES,TEAM_NAME1=TEAM,W1=W,
         TEAM_NAME2=TEAM_2,W2=W_2) %>% 
  mutate_at(c(3,5), ~ str_replace(.," \\s*\\([^\\)]+\\)","")) %>% 
  mutate_at(c(3,5), ~ if_else(.=="Los Angeles Clippers","LA Clippers",.)) %>% 
  filter(W1!="W") %>%  
  mutate(SEASON=paste0(as.numeric(SEASON)-1,"-",substr(SEASON,3,4)),
         SERIES=paste(TEAM_NAME1,TEAM_NAME2,sep=" vs "))

# Final dataset:
# Columns: Season - Round - Team - Wins - Matchup
# Every series has two rows, one for each team
series_final = bind_rows(
  series_adv %>% 
    select(SEASON,ROUND,TEAM_NAME=TEAM_NAME1,W=W1,SERIES),
  series_adv %>% 
    select(SEASON,ROUND,TEAM_NAME=TEAM_NAME2,W=W2,SERIES)
) %>% filter(SEASON!="NA-")
rm(list=setdiff(ls(),"series_final"))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *SCRAPER FUNCTION*

get_raw_stats = function(season,rounds) {
  
  #' [PART 1: TEAM STATISTICS]
  
  # Regular Season Advanced Stats:
  teams_rs = nba_leaguedashteamstats(season=season,measure_type="Advanced",per_mode="Totals") %>% 
    pluck(1) %>% 
    select(TEAM_ID,TEAM_NAME,
           OPP_OFF_RATING_RS=OFF_RATING,
           OPP_DEF_RATING_RS=DEF_RATING,
           TM_TS_PCT_RS=TS_PCT) %>% 
    mutate(TEAM_NAME=gsub("/Oklahoma City","",TEAM_NAME),
           TEAM_NAME=if_else(TEAM_NAME=="Los Angeles Clippers","LA Clippers",TEAM_NAME))
  
  # League Average Offensive and Defensive Ratings for the Regular Season:
  z = mean(as.numeric(teams_rs$OPP_DEF_RATING_RS))   # league DRTG
  y = mean(as.numeric(teams_rs$OPP_OFF_RATING_RS))   # league ORTG
  
  # Playoffs Advanced Stats per round:
  teams_po = map_df(c(rounds), function(x) {
    nba_leaguedashteamstats(season=season,season_type="Playoffs",per_mode="Totals",
                            po_round=x,measure_type="Advanced") %>% 
      pluck(1) %>% 
      select(TEAM_ID,
             OPP_OFF_RATING_ROUND=OFF_RATING,
             OPP_DEF_RATING_ROUND=DEF_RATING) %>% 
      mutate(PO_ROUND=x)
  })
  
  # Merge them together:
  teams = left_join(teams_po,teams_rs,by="TEAM_ID") %>% 
    select(TEAM_ID,TEAM_NAME,PO_ROUND,
           OPP_OFF_RATING_ROUND,OPP_DEF_RATING_ROUND,
           OPP_OFF_RATING_RS,OPP_DEF_RATING_RS,TM_TS_PCT_RS) ; rm(teams_po,teams_rs)
  
  #' [PART 2: PLAYER STATISTICS]
  
  # Regular Season Player TS% (NBA.com):
  nbacom = nba_leaguedashplayerstats(season=season,season_type="Regular Season",
                                     measure_type="Advanced") %>% 
    pluck(1) %>% 
    select(PLAYER_ID,PLAYER_NAME,TS_PCT_RS=TS_PCT)
  
  # Player Bio - Age and Position (Basketball Reference:
  bbref = suppressWarnings(
    "https://www.basketball-reference.com/leagues/NBA_{paste0(20,substr(season,6,7))}_advanced.html" %>% 
      glue() %>% 
      read_html() %>% 
      html_elements("table") %>% 
      html_table() %>% 
      pluck(1) %>% 
      clean_names("all_caps") %>% 
      group_by(PLAYER) %>% 
      filter(as.numeric(MP)==max(as.numeric(MP))) %>%  
      ungroup() %>% 
      arrange(PLAYER)
  ) %>% 
    select(PLAYER_NAME=PLAYER,POS,AGE)
  
  # Merge data from different sources with edit distance:
  edd = adist(bbref$PLAYER_NAME, nbacom$PLAYER_NAME)
  ind = rep(NA,ncol(edd))
  for (i in 1:nrow(nbacom)) {
    ind[i] = which.min(edd[,i])
  }
  bbref =  bbref[ind,] %>% na.omit()
  
  # Final - Player Regular Season Information:
  players_rs = bind_cols(nbacom,bbref[,-1]) %>% select(1,4,5,3)
  rm(i,edd,ind,nbacom,bbref)
  
  # Player Playoffs Stats by Round:
  players_po = map_df(rounds, function(x) {
    ## TS%, Number of Possessions and Usage Rate:
    x1 = nba_leaguedashplayerstats(season=season,season_type="Playoffs",per_mode="Totals",
                                   po_round=x,measure_type="Advanced") %>% 
      pluck(1) %>% 
      select(PLAYER_ID,PLAYER_NAME,TEAM_ID,GP,MIN,TS_PCT,POSS,USG_PCT) %>% 
      mutate(SEASON=season,PO_ROUND=x)
    ## Player Total Points, Field Goal Attempts and Free Throw Attempts:
    x2 = nba_leaguedashplayerstats(season=season,season_type="Playoffs",per_mode="Totals",
                                   po_round=x,measure_type="Base") %>% 
      pluck(1) %>% 
      select(PLAYER_ID,PTS,FGA,FTA)
    Sys.sleep(0.5)
    return(
      left_join(x1,x2,by="PLAYER_ID")
    )
  })
  
  # Final - Player Playoffs Information:
  players_po = left_join(
    players_po,
    left_join(
      nba_leaguedashplayerstats(season=season,season_type="Playoffs",
                                per_mode="Per100Possessions") %>% 
        pluck(1) %>% 
        mutate(POST_PTS_75=as.numeric(PTS)*0.75) %>% 
        select(PLAYER_ID,POST_PTS_75),
      nba_leaguedashplayerstats(season=season,season_type="Playoffs",
                                measure_type="Advanced") %>% 
        pluck(1) %>% 
        select(PLAYER_ID,POST_TS_PCT=TS_PCT),
      by="PLAYER_ID"
    ),
    by="PLAYER_ID"
  )
  
  #' [PART 3: BUILD TABLE]
  
  ## X1 -> Keep relevant data for teams info by round:
  ## Season - Series - Team - Opponent - Round - Off/Def efficiency RS/PO
  x1 = left_join(
    series_final %>% 
      filter(SEASON==season) %>% 
      rowwise() %>% 
      mutate(OPPONENT=gsub(" vs ","",gsub(TEAM_NAME,"",SERIES)),
             PO_ROUND=case_when(
               str_detect(ROUND,"First") ~ 1,
               str_detect(ROUND,"Semifinals") ~ 2,
               str_detect(ROUND,"Conf Finals") ~ 3,
               TRUE ~ 4
             )) %>% ungroup(),
    teams %>% 
      rename(OPPONENT=TEAM_NAME) %>% 
      select(OPPONENT,PO_ROUND,OPP_OFF_RATING_ROUND,OPP_DEF_RATING_ROUND,OPP_OFF_RATING_RS,OPP_DEF_RATING_RS),
    by=c("OPPONENT","PO_ROUND")
  ) %>% 
    select(SEASON,SERIES,TEAM_NAME,PO_ROUND,
           OPPONENT:OPP_DEF_RATING_RS) %>% 
    left_join(.,teams %>% select(TEAM_NAME,TEAM_ID,PO_ROUND), 
              by=c("TEAM_NAME","PO_ROUND"))
  
  ## X2 -V Keep relevant data for players info by round:
  x2 = left_join(players_po,players_rs,by="PLAYER_ID")
  
  # Merge all the data together:
  left_join(x2,x1, by=c("SEASON","TEAM_ID","PO_ROUND")) %>% 
  select(SEASON,PLAYER_ID,PLAYER_NAME,POS,AGE,TEAM_NAME,PO_ROUND,OPPONENT,SERIES,
         GP,MIN,POSS,PTS,USG_PCT,FGA,FTA,TS_PCT,TS_PCT_RS,
         OPP_OFF_RATING_RS,OPP_DEF_RATING_RS,
         OPP_OFF_RATING_ROUND,OPP_DEF_RATING_ROUND,
         POST_PTS_75,POST_TS_PCT) %>% 
    mutate(LA_DEF_RATING=z,LA_OFF_RATING=y) %>% 
  return()
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *GET THE DATA*

#' [MULTIPLE SEASONS]
seasons = year_to_season(1999:2025)
# Repeat 1:4 as many times as the number of post seasons:
rounds  = rep(list(1:4), length(seasons))

mpsrdb = map2(seasons, rounds, function(s, r) {
  res = get_raw_stats(s, r)
  Sys.sleep(runif(1, 3, 5))
  res
}, .progress = TRUE) %>% 
  set_names(seasons) %>% 
  list_rbind()

beepr::beep(10)
rm(list=setdiff(ls(),c("mpsrdb","get_raw_stats","series_final")))
write_csv(mpsrdb,"mpsr-raw.csv") #' *UPLOAD THIS ON GITHUB*


#' #' [SINGLE SEASON - during playofs]
#' season = "2026-27"
#' rounds = c(1:1)
#' mprsrdb = bind_rows(
#'   read_csv("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/shiny-apps/mpsr-raw.csv"),
#'   get_raw_stats(season,rounds)
#' ) %>% 
#'   distinct(SEASON,PLAYER_ID,ROUND, .keep_all=T)