# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script extracts box-score data for the Greek GBL basketball league.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOAD LIBRARIES*
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(httr)
library(rvest)
library(xml2)
library(readr)
library(lubridate)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *CONFIG*
BASE_URL     = "https://www.esake.gr"
USER_AGENT   = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
POLITE_DELAY = 1.0

league = "GBL"
season = "2025-26"

# ESAKE identifiers
idchampionship = "44B80BEB"           # <-- FIX: was missing; results_url() needs it
idseason = c("00000001", "00000002")
series   = c("402",      "402")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *LOW-LEVEL FETCH (direct from site, retrying, browser UA)*
fetch_html = function(url, max_tries = 3, delay = POLITE_DELAY) {
  for (attempt in seq_len(max_tries)) {
    resp = tryCatch(GET(url, user_agent(USER_AGENT), timeout(30)),
                    error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200) {
      txt = content(resp, as = "text", encoding = "UTF-8")
      if (nchar(txt) > 0) {
        Sys.sleep(delay)
        return(read_html(txt, encoding = "UTF-8"))
      }
    }
    Sys.sleep(delay * attempt)
  }
  stop("Failed to fetch: ", url)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *SMALL HELPERS*
`%||%` = function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

split_made = function(x) suppressWarnings(as.integer(str_match(x %||% "", "(\\d+)\\s*-\\s*(\\d+)")[, 2]))
split_att  = function(x) suppressWarnings(as.integer(str_match(x %||% "", "(\\d+)\\s*-\\s*(\\d+)")[, 3]))

clean_int = function(x) {
  x = str_trim(x %||% "")
  x[x %in% c("-", "", "--", "---")] = NA
  suppressWarnings(as.integer(x))
}

time_to_min = function(x) {
  x = str_trim(x %||% "")
  if (!str_detect(x, "\\d+:\\d+")) return(NA_real_)
  p = as.integer(str_split(x, ":", simplify = TRUE))
  secs = if (length(p) == 3) p[1]*3600 + p[2]*60 + p[3]
  else if (length(p) == 2) p[1]*60 + p[2] else NA
  round(secs / 60)
}

extract_id = function(x, key) str_match(x %||% "", paste0(key, "=([0-9A-Fa-f]+)"))[, 2]

# 4-letter uppercase Greek code from the team name's first word.
team_code = function(name) {
  first = str_squish(str_split(name %||% "", "\\s+", simplify = TRUE)[1])
  toupper(str_sub(first, 1, 4))
}

# "Sat 4 Oct 16:00" + season -> Date (season year inferred from the month)
parse_game_date = function(dt_raw, season) {
  if (is.na(dt_raw)) return(as.Date(NA))
  m = str_match(dt_raw, "(\\d{1,2})\\s+([A-Za-z]{3})")
  if (is.na(m[1, 1])) return(as.Date(NA))
  day = as.integer(m[1, 2]); mon_abbr = m[1, 3]
  mon = match(tolower(mon_abbr),
              tolower(c("Jan","Feb","Mar","Apr","May","Jun",
                        "Jul","Aug","Sep","Oct","Nov","Dec")))
  yrs   = as.integer(str_match(season, "(\\d{4})-(\\d{2})")[, 2:3])
  start = yrs[1]; end = start %/% 100 * 100 + yrs[2]
  year  = if (!is.na(mon) && mon >= 8) start else end
  make_date(year, mon, day)
}

game_url = function(idgame, mode = 3)
  paste0(BASE_URL, "/en/action/EsakegameView?idgame=", idgame, "&mode=", mode)

results_url = function(idseason, series, mode = 2)
  paste0(BASE_URL, "/en/action/EsakeResults?idchampionship=", idchampionship,
         "&idseason=", idseason, "&series=", series, "&mode=", mode)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *EXTRACT MATCH IDs*  (loop the idseason/series phases, union all game ids)
esake_game_ids = function() {
  ids = character(0)
  for (i in seq_along(idseason)) {
    page  = fetch_html(results_url(idseason[i], series[i], 2))
    hrefs = page %>% html_elements("a") %>% html_attr("href")
    got   = extract_id(hrefs, "idgame")
    ids   = c(ids, got[!is.na(got)])
  }
  unique(ids)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *PARSE ONE BOXSCORE PAGE*  (pure function - takes a parsed page)
# Table column order after PLAYER:
#  P 2PM-A 3PM-A FTM-A REBS D.REBS O.REBS AST BLK BLK-A FOULS-F FOULS-M STL TO MIN RANK
parse_boxscore_page = function(page, idgame) {
  
  all_tables = html_elements(page, "table")
  if (length(all_tables) == 0) return(NULL)
  tbl_text = vapply(all_tables, html_text2, character(1))
  stat_idx = which(str_detect(tbl_text, "2PM-A"))
  if (length(stat_idx) < 2) return(NULL)                # game not played yet
  stat_tables = all_tables[stat_idx[1:2]]
  
  ptext  = html_text2(page)
  dt_raw = str_match(ptext,
                     "(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)\\s+\\d{1,2}\\s+[A-Za-z]{3,}\\s+\\d{1,2}:\\d{2}")[, 1]
  gdate  = parse_game_date(dt_raw, season)
  
  parse_one = function(tnode, side) {
    prev = xml_find_all(tnode, "preceding::a[contains(@href,'EsaketeamView')]")
    tlink     = if (length(prev)) prev[[length(prev)]] else NULL
    team_name = if (is.null(tlink)) NA_character_ else str_squish(html_text2(tlink))
    
    M = html_table(tnode, header = FALSE, fill = TRUE)
    M = as.data.frame(lapply(M, as.character), stringsAsFactors = FALSE)
    if (ncol(M) < 17) return(NULL)
    labels    = str_squish(M[[1]])
    is_total  = str_detect(toupper(labels), "^TOTAL$")
    is_bench  = str_detect(toupper(labels), "BENCH")
    is_head   = labels %in% c("", "PLAYER") | str_detect(M[[3]], "2PM-A")
    is_player = !is_total & !is_bench & !is_head & labels != ""
    
    mk = function(idx) {
      tibble(
        TEAM   = toupper(team_name),
        PLAYER = str_squish(str_replace(labels[idx], "^#+\\s*\\d*\\s*", "")),
        PTS  = clean_int(M[[2]][idx]),
        `2PM` = split_made(M[[3]][idx]),  `2PA` = split_att(M[[3]][idx]),
        `3PM` = split_made(M[[4]][idx]),  `3PA` = split_att(M[[4]][idx]),
        FTM  = split_made(M[[5]][idx]),  FTA  = split_att(M[[5]][idx]),
        REB  = clean_int(M[[6]][idx]), DREB = clean_int(M[[7]][idx]), OREB = clean_int(M[[8]][idx]),
        AST  = clean_int(M[[9]][idx]), BLK  = clean_int(M[[10]][idx]),
        PF   = clean_int(M[[13]][idx]),                    # FOULS-M = fouls committed
        STL  = clean_int(M[[14]][idx]), TOV = clean_int(M[[15]][idx]),
        MIN  = vapply(M[[16]][idx], time_to_min, numeric(1))
      )
    }
    
    players = mk(which(is_player))
    total   = mk(which(is_total)) %>% mutate(PLAYER = "TOTAL")
    list(players = players, total = total, team_name = team_name)
  }
  
  home = parse_one(stat_tables[[1]], "home")
  away = parse_one(stat_tables[[2]], "away")
  if (is.null(home) || is.null(away)) return(NULL)
  
  code_home = team_code(home$team_name); code_away = team_code(away$team_name)
  matchup   = paste0(gdate, ", ", code_home, " vs ", code_away)
  
  players = bind_rows(home$players, away$players) %>%
    mutate(GAME_ID = idgame, SEASON = season, LEAGUE = league, MATCHUP = matchup)
  
  teams = bind_rows(
    home$total %>% mutate(CODE = code_home),
    away$total %>% mutate(CODE = code_away)
  ) %>%
    mutate(MATCHUP = matchup)
  
  list(players = players, teams = teams, game_date = gdate)
}

esake_boxscore = function(idgame) {
  parse_boxscore_page(fetch_html(game_url(idgame, 3)), idgame)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *FINAL COLUMN SETS*  (Euroleague layout, minus BLKD)
PLAYER_COLS = c("GAME_ID","SEASON","LEAGUE","PLAYER","TEAM","MATCHUP","MIN","PTS",
                "2PM","2PA","3PM","3PA","FTM","FTA","OREB","DREB","REB",
                "AST","STL","TOV","BLK","PF")
TEAM_COLS   = c("TEAM","CODE","MATCHUP","PTS","2PM","2PA","3PM","3PA","FTM","FTA",
                "OREB","DREB","REB","AST","STL","TOV","BLK","PF")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *MAIN*
if (sys.nframe() == 0) {
  
  game_ids = esake_game_ids()
  
  PP = list(); TT = list()
  for (i in seq_along(game_ids)) {
    res = tryCatch(esake_boxscore(game_ids[i]), error = function(e) NULL)
    if (is.null(res)) next
    if (!is.na(res$game_date) && res$game_date > today()) next   # played games only
    PP[[i]] = res$players
    TT[[i]] = res$teams
  }
  
  players_df = bind_rows(PP) %>% arrange(MATCHUP) %>% select(all_of(PLAYER_COLS))
  teams_df   = bind_rows(TT) %>% arrange(MATCHUP) %>% select(all_of(TEAM_COLS))
  
  # write files in .csv format (create the folder first so the write can't fail)
  write_excel_csv(players_df, "bball-stats/data/GR-players.csv")
  write_excel_csv(teams_df,   "bball-stats/data/GR-teams.csv")
}
