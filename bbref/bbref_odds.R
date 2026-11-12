# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Preseason over/under win totals (and championship futures) by team, from Basketball Reference.
#
#   bbref_odds("2025-26")
#
# season : nba.com format, e.g. "2025-26".
# Note: bbref's win-total column already carries the .5 line (e.g. 62.5), so unlike the
#       old version we do NOT add 0.5.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".bbref_read_defined"))
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/scraper%20functions/bbref/bbref_helpers.R")

bbref_odds = function(season) {
  yr  = .bbref_year(season)
  doc = bbref_read(glue("https://www.basketball-reference.com/leagues/NBA_{yr}_preseason_odds.html"))
  tbl = .bbref_table(doc, "NBA_preseason_odds")
  if (is.null(tbl)) stop("Could not find the preseason odds table for ", season, ".")

  rows = tbl %>% html_elements("tbody > tr")
  rows = rows[!str_detect(tidyr::replace_na(html_attr(rows, "class"), ""), "thead")]
  grab = function(stat) rows %>% html_element(sprintf("[data-stat='%s']", stat)) %>% html_text2()
  code = rows %>% html_element("[data-stat='team'] a") %>% html_attr("href") %>% .bbref_code_from_href()

  tibble(
    SEASON             = season,
    TEAM               = code,
    TEAM_NAME          = grab("team"),
    PROJECTED_WINS     = suppressWarnings(as.numeric(grab("wins_ou"))),   # already the .5 line
    CHAMPIONSHIP_ODDS  = grab("odds_fmt"),
    REAL_WINS          = suppressWarnings(as.numeric(str_extract(grab("result"), "^[0-9]+"))),
    RESULT             = grab("result")
  ) %>% filter(!is.na(TEAM))
}
