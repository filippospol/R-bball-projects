# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# One function for every player-stats mode on Basketball Reference.
# Replaces the old bbref_advanced / bbref_shooting / bbref_play_by_play.
#
#   bbref_player_stats("2026-27", mode = "advanced")
#
# season : nba.com format, e.g. "2026-27".
# mode   : one of
#            "totals", "per_game", "per36", "per100",
#            "advanced", "play_by_play", "shooting", "adj_shooting"
# Returns the FULL table for that mode, cleanly named, with SEASON first, nba.com team
# codes, and traded players as one row per team plus a combined "DAL/LAL"-style row.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".bbref_read_defined"))
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/scraper%20functions/bbref/bbref_helpers.R")

bbref_player_stats = function(season, mode = "per_game") {

  # mode -> bbref url slug (aliases accepted)
  slug = dplyr::recode(
    tolower(gsub("[^a-z0-9]", "_", tolower(mode))),
    totals        = "totals",
    per_game      = "per_game",   pergame = "per_game",
    per36         = "per_minute", per_36  = "per_minute", per_minute = "per_minute",
    per100        = "per_poss",   per_100 = "per_poss",   per_poss   = "per_poss",
    advanced      = "advanced",   adv = "advanced",
    play_by_play  = "play-by-play", pbp = "play-by-play", playbyplay = "play-by-play",
    shooting      = "shooting",
    adj_shooting  = "adj_shooting", adjusted_shooting = "adj_shooting", adjshooting = "adj_shooting",
    .default = NA_character_
  )
  if (is.na(slug))
    stop("Unknown mode: \"", mode, "\". Use one of: totals, per_game, per36, per100, ",
         "advanced, play_by_play, shooting, adj_shooting.")

  yr  = .bbref_year(season)
  url = glue("https://www.basketball-reference.com/leagues/NBA_{yr}_{slug}.html")

  tbl = .bbref_player_table(bbref_read(url))

  df = .bbref_extract(tbl) %>%
    mutate(team_name_abbr = ifelse(str_detect(team_name_abbr, "^[0-9]+TM$"),
                                   team_name_abbr,                       # keep aggregate marker
                                   .bbref_nba_code(team_name_abbr))) %>% # nba.com codes
    .bbref_clean_player() %>%
    mutate(SEASON = season, .before = 1) %>%
    .bbref_regroup_traded() %>%                                          # per-team + combined row
    relocate(any_of(c("SEASON","PLAYER_NAME","TEAM","POS","AGE","GP","GS","MIN"))) %>%
    .bbref_numify(keep_chr = c("SEASON", "PLAYER_NAME", "TEAM", "POS")) %>%
    arrange(PLAYER_NAME)

  df
}
