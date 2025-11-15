# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script replicates the Lineup Continuity chart (h/t Owen Phillips).
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pacman::p_load(tidyverse,rvest,ggimage,glue,janitor,hoopR)
# Load bbref functions to get lineup and preseason odds data:
source("https://raw.githubusercontent.com/filippospol/R-bball-projects/refs/heads/main/scraper%20functions/bbref/bbref_lineups.R")
source("https://raw.githubusercontent.com/filippospol/R-bball-projects/refs/heads/main/scraper%20functions/bbref/bbref_odds.R")

# Specify season:
season="2024-25"

# Scrape the data for the given season:
lineups = bbref_lineups(season=season) %>% 
  mutate(TEAM_NAME=if_else(TEAM_NAME=="Los Angeles Clippers",
                           "LA Clippers",TEAM_NAME))
odds = bbref_odds(season=season)

# Teams wins and losses:
teams = nba_leaguedashteamstats(season=season) %>% 
  pluck(1) %>% 
  select(TEAM_ID,W,L) %>% 
  mutate_at(-1, as.numeric)

# Merge tables to get necessary columns:
continuity = left_join(
  lineups,odds,
  by=c("SEASON","TEAM_NAME")
) %>% 
  left_join(.,teams,by="TEAM_ID") %>% 
  select(SEASON,TEAM_ID,TEAM_ABBREVIATION,TEAM_NAME,PROJECTED_WINS,W,L,LINEUPS) %>% 
  mutate_at(5:8, as.numeric)
rm(list=setdiff(ls(),"continuity"))

# Add columns for the chart:
continuity = continuity %>% 
  mutate(GP=W+L,
         PWIN=PROJECTED_WINS/82,
         TWIN=W/GP,
         DELTA=TWIN-PWIN,
         CONTINUITY=LINEUPS/GP,
         LOGO=glue("https://raw.githubusercontent.com/filippospol/R-bball-projects/refs/heads/main/misc/nba-team-logos-png/{TEAM_ID}.png"))

# Create chart:
continuity %>% 
  ggplot(aes(x=CONTINUITY,y=DELTA,image=LOGO)) +
  scale_y_continuous(labels=scales::percent,
                     limits=c(-max(abs(continuity$DELTA)),
                              max(abs(continuity$DELTA)))) +
  scale_x_continuous(limits=c(0,max(continuity$CONTINUITY))) +
  geom_hline(yintercept=0,linetype=2,alpha=0.4) +
  geom_image(aes(image=LOGO),size=0.06) +
  labs(title=paste('Continuity Chart:',season),
       subtitle=paste('Source: Basketball Reference | As of',format(Sys.Date(), "%m/%d/%y")),
       x='# of Distinct Starting Lineups / Games Played',
       y='Current W% - Projected W%',

       caption="Inspired by Owen Phillips")
