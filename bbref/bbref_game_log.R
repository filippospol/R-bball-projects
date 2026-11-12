# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# One game's box score from Basketball Reference, including who was Inactive / DNP.
#
#   ids = bbref_gameid_date(2025, 4, 13)
#   log = bbref_game_log(ids[1])
#   # or many:  logs = purrr::map_dfr(ids, bbref_game_log)
#
# gameID : a bbref box-score id, e.g. "202504130BOS".
# Returns one row per player (both teams), with IS_STARTER, STATUS (Active / Inactive /
# the DNP reason) and the basic box line. Team codes are nba.com.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".bbref_read_defined"))
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/scraper%20functions/bbref/bbref_helpers.R")

bbref_game_log = function(gameID) {
  doc = bbref_read(glue("https://www.basketball-reference.com/boxscores/{gameID}.html"))

  # Team codes from the two basic box ids (away listed first, home second on bbref).
  box_ids = doc %>% html_elements("table") %>% html_attr("id") %>%
    str_subset("^box-[A-Z]{3}-game-basic$")
  bb      = str_match(box_ids, "^box-([A-Z]{3})-game-basic$")[, 2]
  away_bb = bb[1]; home_bb = bb[2]

  # Matchup + date from the H1 ("X at Y Box Score, April 13, 2025").
  h1    = doc %>% html_element("h1") %>% html_text2()
  parts = str_split(h1, " Box Score, ", simplify = TRUE)
  date  = suppressWarnings(as.Date(str_trim(parts[1, 2]), format = "%B %d, %Y"))
  matchup = paste(.bbref_nba_code(away_bb), "@", .bbref_nba_code(home_bb))

  # Inactive players, grouped by team code (walk the "Inactive:" block in document order).
  inactive = list()
  istrong = doc %>% html_elements("strong") %>% keep(~ str_trim(html_text2(.x)) == "Inactive:")
  if (length(istrong)) {
    blk = xml2::xml_parent(istrong[[1]])
    cur = NA_character_
    for (nd in xml2::xml_find_all(blk, ".//strong|.//a")) {
      nm = xml2::xml_name(nd); tx = str_trim(xml2::xml_text(nd))
      if (nm == "strong" && str_detect(tx, "^[A-Z]{3}$")) { cur = tx; inactive[[cur]] = character(0) }
      else if (nm == "a" && !is.na(cur))                   inactive[[cur]] = c(inactive[[cur]], tx)
    }
  }

  stat_cols = c("fg","fga","fg_pct","fg3","fg3a","fg3_pct","ft","fta","ft_pct",
                "orb","drb","trb","ast","stl","blk","tov","pf","pts","game_score","plus_minus")

  one_team = function(bb) {
    tbl  = .bbref_table(doc, paste0("box-", bb, "-game-basic"))
    rows = tbl %>% html_elements("tbody > tr")
    starter = TRUE; acc = list()
    for (tr in rows) {
      cls = tidyr::replace_na(html_attr(tr, "class"), "")
      if (str_detect(cls, "thead")) { starter = FALSE; next }     # the "Reserves" divider
      player = tr %>% html_element("[data-stat='player']") %>% html_text2()
      if (is.na(player) || player %in% c("", "Team Totals")) next
      mp = tr %>% html_element("[data-stat='mp']") %>% html_text2()
      active = !is.na(mp) && str_detect(mp, "[0-9]") && str_detect(mp, ":")
      stats  = set_names(map_chr(stat_cols, function(s)
                 tr %>% html_element(sprintf("[data-stat='%s']", s)) %>% html_text2()), toupper(stat_cols))
      acc[[length(acc) + 1]] = tibble::tibble(
        TEAM = .bbref_nba_code(bb), PLAYER_NAME = player,
        IS_STARTER = if (active) starter else NA,
        STATUS = if (active) "Active" else (if (is.na(mp) || mp == "") "Did Not Play" else mp),
        MP = if (active) { ms = as.numeric(str_split(mp, ":", simplify = TRUE));
                           round(ms[1] + ms[2] / 60, 1) } else NA_real_,
        !!!as.list(if (active) stats else set_names(rep(NA_character_, length(stat_cols)), toupper(stat_cols)))
      )
    }
    dressed = bind_rows(acc)
    inact_tbl = if (!is.null(inactive[[bb]]) && length(inactive[[bb]]))
      tibble::tibble(TEAM = .bbref_nba_code(bb), PLAYER_NAME = inactive[[bb]],
                     IS_STARTER = NA, STATUS = "Inactive", MP = NA_real_) else NULL
    bind_rows(dressed, inact_tbl)
  }

  bind_rows(one_team(away_bb), one_team(home_bb)) %>%
    mutate(GAME_ID = gameID, MATCHUP = matchup, DATE = date, .before = 1) %>%
    mutate(across(all_of(toupper(stat_cols)), ~ suppressWarnings(as.numeric(.x))))
}
