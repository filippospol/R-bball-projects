# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# A season's schedule & results from Basketball Reference (every month combined).
#
#   bbref_schedule("2024-25")
#
# season : nba.com format, e.g. "2024-25".
# Returns one row per game: date, visitor/home (nba.com codes + names), points, OT,
# attendance and the box-score id (feed it to bbref_game_log()).
# NOTE: this is the one function I could not verify against a saved page in this pass -
#       the logic follows bbref's current /leagues/NBA_YYYY_games*.html markup (table id
#       "schedule", data-stat columns), but give it a quick eyeball on first run.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".bbref_read_defined"))
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/bbref/bbref_helpers.R")

bbref_schedule = function(season) {
  yr   = .bbref_year(season)
  base = glue("https://www.basketball-reference.com/leagues/NBA_{yr}_games.html")
  doc0 = bbref_read(base)

  # Month sub-pages (NBA_YYYY_games-january.html, ...). Fall back to the base page alone.
  months = doc0 %>% html_elements("div.filter a") %>% html_attr("href") %>%
    str_subset(glue("NBA_{yr}_games-")) %>% unique()
  urls = if (length(months)) paste0("https://www.basketball-reference.com", months) else base

  one_page = function(url) {
    doc = if (identical(url, base)) doc0 else bbref_read(url)
    tbl = .bbref_table(doc, "schedule")
    if (is.null(tbl)) return(NULL)
    rows = tbl %>% html_elements("tbody > tr")
    rows = rows[!str_detect(tidyr::replace_na(html_attr(rows, "class"), ""), "thead")]
    grab = function(stat) rows %>% html_element(sprintf("[data-stat='%s']", stat)) %>% html_text2()
    code = function(stat) rows %>% html_element(sprintf("[data-stat='%s'] a", stat)) %>%
      html_attr("href") %>% .bbref_code_from_href()
    box  = rows %>% html_element("[data-stat='box_score_text'] a") %>% html_attr("href") %>%
      str_match("/boxscores/([0-9]{9}[A-Z]{3})\\.html") %>% .[, 2]
    tibble(
      DATE          = suppressWarnings(as.Date(grab("date_game"), format = "%a, %b %d, %Y")),
      VISITOR       = code("visitor_team_name"), VISITOR_NAME = grab("visitor_team_name"),
      VISITOR_PTS   = suppressWarnings(as.numeric(grab("visitor_pts"))),
      HOME          = code("home_team_name"),    HOME_NAME    = grab("home_team_name"),
      HOME_PTS      = suppressWarnings(as.numeric(grab("home_pts"))),
      OT            = grab("overtimes"),
      ATTENDANCE    = suppressWarnings(as.numeric(str_remove_all(grab("attendance"), ","))),
      GAME_ID       = box
    ) %>% filter(!is.na(VISITOR) | !is.na(HOME))
  }

  map_dfr(urls, one_page) %>%
    mutate(SEASON = season, .before = 1) %>%
    arrange(DATE)
}
