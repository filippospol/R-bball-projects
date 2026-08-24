# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script extracts box-score data for the Brazilian NBB (Novo Basquete Brasil).
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOAD LIBRARIES*
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(stringi)
library(rvest)
library(httr)
library(lubridate)
library(readr)
library(vroom)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *CONFIG*
league    = "NBB"
season    = "2025-26"
season_id = 97   # NBB 2025-26 on lnb.com.br (bump each season: check the site's season filter)
# The bare season URL (no phase, no wherePlaying) returns EVERY game - regular season AND the
# full playoffs (Oitavas -> Quartas -> Semi -> Final, games 381-439). Adding phase[]= or
# wherePlaying= makes the site truncate to the regular season only, so keep this URL as-is.
schedule_url = paste0("https://lnb.com.br/nbb/tabela-de-jogos/?season%5B%5D=", season_id)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *BUILD FIXTURE LIST* (one fetch of the schedule page - all games, all phases)
# Each schedule row carries the game's date and its "VER RELATORIO" report link. We pair them
# per-row so nothing depends on positional alignment. Two ways to tell a game is finished, both
# WITHOUT fetching the report page:
#   (a) the report slug contains the score, e.g. "...-vasco-78-x-88-fortaleza..."  -> played
#   (b) the game date is on/before today
# We only fetch report pages for games flagged played, and later also skip any page that
# doesn't actually contain the two box-score tables (a further safety check).

# Fetch the schedule with a timeout + a few retries and a real User-Agent. A bare read_html()
# here is the single point of failure: if that one request blips on the CI runner, the whole
# run dies with "cannot open the connection". Retrying makes transient failures non-fatal.
fetch_html = function(url, tries = 4, pause = 3) {
  for (k in seq_len(tries)) {
    res = tryCatch(
      read_html(GET(url,
                    user_agent("Mozilla/5.0 (compatible; bball-stats-scraper/1.0)"),
                    timeout(45))),
      error = function(e) NULL
    )
    if (!is.null(res)) return(res)
    message("schedule fetch attempt ", k, "/", tries, " failed; retrying in ", pause, "s...")
    Sys.sleep(pause)
  }
  stop("could not fetch schedule after ", tries, " attempts: ", url)
}

sched_page = fetch_html(schedule_url)

fixtures = sched_page %>%
  html_elements("tr") %>%
  map_dfr(function(r) {
    # Report links: regular-season slugs are "/noticias/nbb-caixa-...", but PLAYOFF games use
    # "/noticias/playoffs-nbb-caixa-..." and the finals use "/noticias/finais-nbb-caixa-...".
    # So grab every /noticias/ link in the row and keep the first one mentioning "nbb-caixa".
    hrefs = r %>% html_elements("a[href*='/noticias/']") %>% html_attr("href")
    hrefs = hrefs[!is.na(hrefs) & str_detect(hrefs, "nbb-caixa")]
    if (length(hrefs) == 0) return(NULL)
    href = hrefs[1]
    row_txt  = html_text2(r)
    date_raw = str_match(row_txt, "(\\d{2}/\\d{2}/\\d{4})")[, 2]
    # the two 3-letter scoreboard codes flank the second "NN X NN VER RELATORIO":
    codes = str_match(row_txt, "([A-Z]{3})\\s+\\d+\\s*X\\s*\\d+\\s*VER RELAT\\S*RIO\\s+([A-Z]{3})")
    tibble(report_url = href, date_raw = date_raw,
           home_code = codes[, 2], away_code = codes[, 3])
  }) %>%
  mutate(report_url = if_else(str_detect(report_url, "^http"),
                              report_url, paste0("https://lnb.com.br", report_url))) %>%
  distinct(report_url, .keep_all = TRUE) %>%
  mutate(
    DATE      = suppressWarnings(as.Date(date_raw, "%d/%m/%Y")),
    has_score = str_detect(report_url, "-\\d+-x-\\d+-"),
    played    = has_score | (!is.na(DATE) & DATE <= Sys.Date()),
    MATCHUP   = paste0(format(DATE, "%Y-%m-%d"), ", ", home_code, " vs ", away_code)
  ) %>%
  arrange(DATE)

fixtures_played = fixtures %>% filter(played)

url_list     = fixtures_played$report_url
date_list    = fixtures_played$DATE
matchup_list = fixtures_played$MATCHUP
homecode_list = fixtures_played$home_code
awaycode_list = fixtures_played$away_code

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *PARSING HELPERS* (Brazilian cell formats verified against a real report page)

# "20/35 (57)" -> 20  (points scored = number before the slash)
num_before_slash = function(x) as.numeric(str_extract(x, "^\\d+"))
# "4/8 (50)" -> made 4, attempted 8
made = function(x) as.numeric(str_match(x, "^(\\d+)/")[, 2])
att  = function(x) as.numeric(str_match(x, "^\\d+/(\\d+)")[, 2])
# "4+1 5" -> DREB 4 (defensive first), OREB 1, REB 5
reb_d = function(x) as.numeric(str_match(x, "^(\\d+)\\+")[, 2])
reb_o = function(x) as.numeric(str_match(x, "\\+(\\d+)")[, 2])
reb_t = function(x) as.numeric(str_match(x, "\\s(\\d+)\\s*$")[, 2])

# turn one report table (tables 1 or 6) + its team name into player rows (+ team totals row):
parse_box = function(tbl, team_name, team_code, matchup, game_id) {
  tbl %>%
    as_tibble(.name_repair = "minimal") %>%
    filter(!is.na(Jogador), Jogador != "") %>%
    transmute(
      GAME_ID = game_id, SEASON = season, LEAGUE = league,
      PLAYER  = Jogador, TEAM = team_name, CODE = team_code, MATCHUP = matchup,
      IS_TEAM = str_detect(Nr., regex("equipe", ignore_case = TRUE)) |
        str_detect(Jogador, regex("equipe", ignore_case = TRUE)),
      MIN  = round(as.numeric(Min)),
      PTS  = num_before_slash(Pts),
      `2PM` = made(`2P%`), `2PA` = att(`2P%`),
      `3PM` = made(`3P%`), `3PA` = att(`3P%`),
      FTM  = made(`LL%`),  FTA  = att(`LL%`),
      DREB = reb_d(`RD+RO RT`), OREB = reb_o(`RD+RO RT`), REB = reb_t(`RD+RO RT`),
      AST = as.numeric(AS),
      STL = as.numeric(BR),   # bolas recuperadas = steals
      BLK = as.numeric(TO),   # tocos = blocks
      TOV = as.numeric(ER),   # erros = turnovers
      PF  = as.numeric(FC)    # faltas cometidas = personal fouls
    )
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOOP OVER FINISHED GAMES AND GET BOXSCORES*

PP = list()
TT = list()
for (i in 1:length(url_list)) {
  
  # fetch with a timeout so a single slow/dead page can't stall the whole run:
  report = tryCatch(read_html(GET(url_list[i], timeout(30))), error = function(e) NULL)
  if (is.null(report)) { message("skip (fetch failed): ", url_list[i]); next }

  tabs = report %>% html_elements("table")
  if (length(tabs) < 6) next   # no box score on this report page yet -> skip
  
  # if (i %% 25 == 0) message("...", i, "/", length(url_list), " games")
  
  # Team full names (for the TEAM column) from the page <title>. Regular-season titles read
  #   "NBB CAIXA 2025/26 | <home> NN x NN <away> - Liga Nacional de Basquete"
  # but PLAYOFF titles append phase/game info after the away name, e.g.
  #   "Playoffs NBB CAIXA 2025/26 | CAIXA/Brasilia 67 x 50 Sesi Franca | Semifinais | Jogo 2 - Liga..."
  #   "Finais NBB CAIXA 2025/26 | Pinheiros 94 x 92 Sesi Franca | Jogo 3 - Liga..."
  # so each name must stop at the first "|" (team names never contain "|"); the away name ends
  # at either that "|" or the "- Liga" tail.
  title = report %>% html_element("title") %>% html_text2()
  tm = str_match(title, "\\|\\s*([^|\u2013]+?)\\s+\\d+\\s*[xX]\\s*\\d+\\s+([^|\u2013]+?)\\s*(?:\\||[\u2013-]\\s*Liga)")
  if (is.na(tm[, 2])) next
  home = str_squish(tm[, 2]); away = str_squish(tm[, 3])
  
  # MATCHUP already built in the fixture list ("YYYY-MM-DD, CODE vs CODE"):
  matchup = matchup_list[i]
  
  box_home = parse_box(html_table(tabs[[1]]), home, homecode_list[i], matchup, i)
  box_away = parse_box(html_table(tabs[[6]]), away, awaycode_list[i], matchup, i)
  
  # Players: drop the team-totals row and DNPs (Min == 0):
  PP[[i]] = bind_rows(box_home, box_away) %>%
    filter(!IS_TEAM, MIN > 0) %>%
    select(-IS_TEAM) %>%
    mutate(PLAYER = stri_trans_general(toupper(as.character(PLAYER)), "latin-ascii"),
           TEAM   = stri_trans_general(toupper(as.character(TEAM)),   "latin-ascii")) %>%
    mutate(across(PTS:PF, as.numeric))
  
  # Team stats: keep only the totals row from each side:
  TT[[i]] = bind_rows(box_home, box_away) %>%
    filter(IS_TEAM) %>%
    transmute(TEAM = stri_trans_general(toupper(as.character(TEAM)), "latin-ascii"),
              CODE, MATCHUP, PTS, `2PM`, `2PA`, `3PM`, `3PA`, FTM, FTA,
              DREB, OREB, REB, AST, STL, BLK, TOV, PF) %>%
    mutate(across(PTS:PF, as.numeric))
  
  Sys.sleep(0.3)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=setdiff(ls(),c("PP","TT")))
players = bind_rows(PP) %>% mutate(TEAM = toupper(TEAM), PLAYER = str_squish(PLAYER))
teams   = bind_rows(TT) %>% mutate(TEAM = toupper(TEAM))

# CANONICAL TEAM NAMES: the 3-letter CODE is stable across the whole season, but the printed
# name can vary (e.g. "PAULISTANO" vs "PAULISTANO/CORPE", or leftover playoff text). Pick ONE
# canonical name per code - the most frequent spelling seen (regular season dominates, so this
# matches what the app already expects) - and apply it to BOTH files. Then drop CODE.
canon = bind_rows(players %>% select(CODE, TEAM),
                  teams   %>% select(CODE, TEAM)) %>%
  filter(!is.na(CODE), CODE != "") %>%
  count(CODE, TEAM, name = "n") %>%
  group_by(CODE) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(CODE, TEAM_CANON = TEAM)

players = players %>%
  left_join(canon, by = "CODE") %>%
  mutate(TEAM = coalesce(TEAM_CANON, TEAM)) %>%
  select(-CODE, -TEAM_CANON)
teams = teams %>%
  left_join(canon, by = "CODE") %>%
  mutate(TEAM = coalesce(TEAM_CANON, TEAM)) %>%
  select(-CODE, -TEAM_CANON)

# write files in .csv format
vroom_write(players, "bball-stats/data/BR-players.csv")
vroom_write(teams,   "bball-stats/data/BR-teams.csv")
