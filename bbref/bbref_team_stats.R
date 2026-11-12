# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Team (or opponent) stats by mode, from the Basketball Reference season summary page.
# Replaces the old bbref_teams_shooting / bbref_opponents_shooting.
#
#   bbref_team_stats("2026-27", mode = "shooting", side = "opponent")
#
# season : nba.com format, e.g. "2026-27".
# mode   : "per_game", "totals", "per100", "advanced", "shooting".
# side   : "team" (what the team did) or "opponent" (what it allowed). "advanced" is
#          team-only on bbref.
# Returns one row per team (nba.com code + full name), cleanly named, SEASON first.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".bbref_read_defined"))
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/bbref/bbref_helpers.R")

bbref_team_stats = function(season, mode = "per_game", side = "team") {

  slug = dplyr::recode(
    tolower(gsub("[^a-z0-9]", "_", tolower(mode))),
    per_game = "per_game", pergame = "per_game",
    totals   = "totals",
    per100   = "per_poss", per_100 = "per_poss", per_poss = "per_poss",
    advanced = "advanced", adv = "advanced",
    shooting = "shooting",
    .default = NA_character_
  )
  if (is.na(slug))
    stop("Unknown mode: \"", mode, "\". Use: per_game, totals, per100, advanced, shooting.")
  side = match.arg(tolower(side), c("team", "opponent"))
  if (slug == "advanced" && side == "opponent")
    stop("bbref has no opponent table for mode = \"advanced\" (team only).")

  yr  = .bbref_year(season)
  id  = paste0(slug, "-", side)
  doc = bbref_read(glue("https://www.basketball-reference.com/leagues/NBA_{yr}.html"))
  tbl = .bbref_table(doc, id)
  if (is.null(tbl)) stop("Could not find table \"", id, "\" on the ", season, " summary page.")

  # nba.com team code from the /teams/XXX/ link, using the SAME row filtering as the
  # extractor so the vectors stay aligned; rows without a link (League Average) drop out.
  data_rows = tbl %>% html_elements("tbody > tr")
  data_rows = data_rows[!str_detect(tidyr::replace_na(html_attr(data_rows, "class"), ""), "thead")]
  codes = data_rows %>% html_element("[data-stat='team'] a") %>% html_attr("href") %>%
    .bbref_code_from_href()

  df = .bbref_extract(tbl)
  df$TEAM_NAME = df$team
  df$team = NULL
  df = df %>% rename_with(toupper) %>% select(-any_of("RANKER"))
  df$TEAM = codes[seq_len(nrow(df))]
  df = df %>%
    filter(!is.na(TEAM)) %>%
    mutate(SEASON = season, .before = 1) %>%
    relocate(TEAM, TEAM_NAME, .after = SEASON) %>%
    .bbref_numify(keep_chr = c("SEASON", "TEAM", "TEAM_NAME"))

  df
}
