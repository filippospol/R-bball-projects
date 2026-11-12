# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Number of distinct starting lineups each team has used in a season, from Basketball Reference.
#
#   bbref_lineups("2024-25")
#
# season : nba.com format, e.g. "2024-25".
# Hits ~31 pages (team list + one page per team); the shared throttle keeps the whole
# run under bbref's ~20 req/min cap, so no blind Sys.sleep() is needed. The old NBA-Stats
# (hoopR) join was dropped - team identity now comes straight from bbref as nba.com codes.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".bbref_read_defined"))
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/bbref/bbref_helpers.R")

bbref_lineups = function(season) {
  yr = .bbref_year(season)

  # bbref team codes (keep bbref spelling for the per-team URL; map to nba.com for output).
  pg   = bbref_read(glue("https://www.basketball-reference.com/leagues/NBA_{yr}_per_game.html"))
  tbl  = .bbref_player_table(pg)
  bb_codes = tbl %>% html_elements("tbody > tr") %>%
    html_element("[data-stat='team_name_abbr']") %>% html_text2() %>%
    discard(~ is.na(.x) || .x == "" || str_detect(.x, "^[0-9]+TM$")) %>%
    unique() %>% sort()

  count_team = function(bb) {
    url = glue("https://www.basketball-reference.com/teams/{bb}/{yr}_start.html")
    tbl = .bbref_table(bbref_read(url), "starting_lineups_po0")      # po0 = regular season
    if (is.null(tbl)) return(tibble(TEAM = .bbref_nba_code(bb), LINEUPS = NA_integer_))
    # each game's five starters as player-ids; a lineup is the sorted set of ids.
    games = tbl %>% html_elements("tbody > tr")
    games = games[!str_detect(tidyr::replace_na(html_attr(games, "class"), ""), "thead")]
    lus = map_chr(games, function(g) {
      ids = g %>% html_elements("[data-stat='game_starters'] a") %>% html_attr("href") %>%
        str_match("/players/./([a-z0-9]+)\\.html") %>% .[, 2]
      paste(sort(ids), collapse = "_")
    })
    tibble(TEAM = .bbref_nba_code(bb), LINEUPS = n_distinct(lus[lus != ""]))
  }

  map_dfr(bb_codes, count_team) %>%
    mutate(SEASON = season, .before = 1) %>%
    arrange(desc(LINEUPS))
}
