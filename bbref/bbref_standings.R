# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Conference standings from Basketball Reference (East + West in one table).
#
#   bbref_standings("2026-27")
#
# season : nba.com format, e.g. "2026-27".
# Returns W/L, win%, games back, PPG/opp-PPG and SRS per team, with nba.com codes.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".bbref_read_defined"))
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/bbref/bbref_helpers.R")

bbref_standings = function(season) {
  yr  = .bbref_year(season)
  doc = bbref_read(glue("https://www.basketball-reference.com/leagues/NBA_{yr}.html"))

  one_conf = function(id, conf) {
    tbl = .bbref_table(doc, id)
    if (is.null(tbl)) stop("Could not find standings table \"", id, "\".")
    rows  = tbl %>% html_elements("tbody > tr")
    rows  = rows[!str_detect(tidyr::replace_na(html_attr(rows, "class"), ""), "thead")]
    code  = rows %>% html_element("[data-stat='team_name'] a") %>% html_attr("href") %>%
      .bbref_code_from_href()
    name  = rows %>% html_element("[data-stat='team_name']") %>% html_text2() %>%
      str_remove("\\*$") %>% str_trim()
    grab  = function(stat) rows %>% html_element(sprintf("[data-stat='%s']", stat)) %>% html_text2()
    tibble(
      SEASON = season, CONFERENCE = conf, TEAM = code, TEAM_NAME = name,
      W = grab("wins"), L = grab("losses"), WIN_PCT = grab("win_loss_pct"),
      GB = grab("gb"), PTS_PER_G = grab("pts_per_g"),
      OPP_PTS_PER_G = grab("opp_pts_per_g"), SRS = grab("srs")
    ) %>% filter(!is.na(TEAM))
  }

  bind_rows(one_conf("confs_standings_E", "East"),
            one_conf("confs_standings_W", "West")) %>%
    mutate(GB = ifelse(GB == "—", "0", GB)) %>%   # bbref shows the leader's GB as a dash
    .bbref_numify(keep_chr = c("SEASON", "CONFERENCE", "TEAM", "TEAM_NAME")) %>%
    arrange(CONFERENCE, desc(WIN_PCT))
}
