# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script extracts box-score data for the Brazilian NBB (Novo Basquete Brasil).
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *THIS SCRAPER RUNS LOCALLY ONLY!!!*

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

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *CONFIG*
league    = "NBB"
season    = "2026-27"

# season_id: leave as NA. The bare season URL already returns the CURRENT season, all phases
# (regular season + playoffs), which is exactly what we want. Hard-coding an id only creates a
# way for the whole run to silently scrape the wrong season. Set it only if you need an archive
# season, and read the id off the site's own season filter when you do.
season_id = NA

schedule_url = if (is.na(season_id)) {
  "https://lnb.com.br/nbb/tabela-de-jogos/"
} else {
  paste0("https://lnb.com.br/nbb/tabela-de-jogos/?season%5B%5D=", season_id)
}

polite = 0.3   # seconds between report fetches

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *FETCH* (timeout + retries + browser UA)
UA = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"

fetch_html = function(url, tries = 4, pause = 3) {
  for (k in seq_len(tries)) {
    res = tryCatch(GET(url, user_agent(UA), timeout(45)), error = function(e) NULL)
    if (!is.null(res) && status_code(res) == 200) {
      # Hand libxml2 the RAW bytes and let it sniff the encoding from the response headers
      # and the <meta charset> tag. Do NOT pre-convert with content(as = "text",
      # encoding = "UTF-8"): a page carrying even one byte that isn't valid UTF-8 (the odd
      # Latin-1 character does turn up in the match reports) gets truncated at that byte, and
      # read_html then returns a short document with ZERO <table> elements rather than an
      # error - which looks exactly like "this game has no box score".
      return(tryCatch(read_html(content(res, as = "raw")), error = function(e) NULL))
    }
    message("  fetch attempt ", k, "/", tries, " failed; retrying in ", pause, "s...")
    Sys.sleep(pause)
  }
  NULL
}

sched_page = fetch_html(schedule_url)
if (is.null(sched_page)) stop("could not fetch the schedule page: ", schedule_url)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *BUILD FIXTURE LIST* (one fetch of the schedule page - all games, all phases)
# Each schedule row carries the game's date and its "VER RELATORIO" report link. Only PLAYED
# games get a /noticias/ report link; upcoming games link to /partidas/ instead, so the presence
# of a /noticias/...nbb-caixa... link is itself the "this game is finished" signal.

fixtures = sched_page %>%
  html_elements("tr") %>%
  map_dfr(function(r) {
    # Regular-season slugs are "/noticias/nbb-caixa-...", playoffs use
    # "/noticias/playoffs-nbb-caixa-..." and the finals "/noticias/finais-nbb-caixa-...",
    # so grab every /noticias/ link in the row and keep the first mentioning "nbb-caixa".
    hrefs = r %>% html_elements("a[href*='/noticias/']") %>% html_attr("href")
    hrefs = hrefs[!is.na(hrefs) & str_detect(hrefs, "nbb-caixa")]
    if (length(hrefs) == 0) return(NULL)
    row_txt  = html_text2(r)
    date_raw = str_match(row_txt, "(\\d{2}/\\d{2}/\\d{4})")[, 2]
    # the two 3-letter scoreboard codes flank the second "NN X NN VER RELATORIO":
    codes = str_match(row_txt, "([A-Z]{3})\\s+\\d+\\s*X\\s*\\d+\\s*VER RELAT\\S*RIO\\s+([A-Z]{3})")
    tibble(report_url = hrefs[1], date_raw = date_raw,
           home_code = codes[, 2], away_code = codes[, 3])
  })

if (nrow(fixtures) == 0) {
  stop("no played fixtures found on ", schedule_url,
       " - the schedule page layout probably changed; inspect it before trusting any output.")
}

fixtures = fixtures %>%
  mutate(report_url = if_else(str_detect(report_url, "^http"),
                              report_url, paste0("https://lnb.com.br", report_url))) %>%
  distinct(report_url, .keep_all = TRUE) %>%
  mutate(
    DATE    = suppressWarnings(as.Date(date_raw, "%d/%m/%Y")),
    SLUG    = str_match(report_url, "/noticias/([^/]+)/?$")[, 2],
    MATCHUP = paste0(format(DATE, "%Y-%m-%d"), ", ", home_code, " vs ", away_code)
  ) %>%
  arrange(DATE)

message("fixtures with a report link: ", nrow(fixtures))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *SEASON SCHEDULE FILE* (for the completeness check in the GitHub Actions summary)
schedule = sched_page %>%
  html_elements("tr") %>%
  map_dfr(function(r) {
    row_txt  = html_text2(r)
    date_raw = str_match(row_txt, "(\\d{2}/\\d{2}/\\d{4})")[, 2]
    if (is.na(date_raw)) return(NULL)
    hrefs = r %>% html_elements("a") %>% html_attr("href")
    hrefs = hrefs[!is.na(hrefs) & str_detect(hrefs, "/partidas/|nbb-caixa")]
    if (length(hrefs) == 0) return(NULL)                 # not a game row
    tibble(KEY = hrefs[1], DATE = as.Date(date_raw, "%d/%m/%Y"))
  }) %>%
  distinct(KEY, .keep_all = TRUE) %>%
  arrange(DATE) %>%
  select(DATE)
write_csv(schedule, "BR-schedule.csv")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *PARSING HELPERS* (Brazilian cell formats verified against a real report page)

# "20/35 (57)" -> 20  (points scored = number before the slash)
num_before_slash = function(x) suppressWarnings(as.numeric(str_extract(x, "^\\d+")))
# "4/8 (50)" -> made 4, attempted 8
made = function(x) suppressWarnings(as.numeric(str_match(x, "^(\\d+)/")[, 2]))
att  = function(x) suppressWarnings(as.numeric(str_match(x, "^\\d+/(\\d+)")[, 2]))
# "4+1 5" -> first number, second number, total
reb_a = function(x) suppressWarnings(as.numeric(str_match(x, "^(\\d+)\\+")[, 2]))
reb_b = function(x) suppressWarnings(as.numeric(str_match(x, "\\+(\\d+)")[, 2]))
reb_t = function(x) suppressWarnings(as.numeric(str_match(x, "\\s(\\d+)\\s*$")[, 2]))
num   = function(x) suppressWarnings(as.numeric(gsub(",", ".", x)))

# Pick a column by REGEX on its header rather than by position or exact spelling. The site
# renders some headers with <br> / non-breaking spaces, and the exact wording has drifted
# before ("RD+RO RT" vs "RO+RD RT"), so exact-name matching is the fragile part.
pick = function(tb, pattern, what) {
  i = which(grepl(pattern, names(tb), perl = TRUE))
  if (length(i) == 0)
    stop("no column matching ", what, " - headers were: ", paste(names(tb), collapse = " | "))
  as.character(tb[[i[1]]])
}

clean_names = function(tb) {
  names(tb) = str_squish(gsub("\u00a0", " ", names(tb)))
  tb
}

# Does this table look like a player box score?
is_box_table = function(tb) {
  if (is.null(tb) || !is.data.frame(tb) || ncol(tb) == 0) return(FALSE)
  nm = tolower(names(tb))
  any(grepl("^jogador", nm)) && any(grepl("^min$", nm)) && any(grepl("^pts$", nm)) &&
    any(grepl("\\+", nm) & !grepl("/", nm))
}

# lnb.com.br serves its "not found" template with an HTTP 200 status and the correct <title>
# still filled in, so neither the status check nor the title tells you the article is gone.
# Detect it by content, otherwise a dead report link looks like a parsing failure.
is_404_page = function(doc) {
  txt = tryCatch(html_text2(doc), error = function(e) "")
  grepl("conte\u00fado solicitado n\u00e3o foi encontrado", txt, ignore.case = TRUE) ||
    length(html_elements(doc, "img[src*='404']")) > 0
}

# Read ONE <table> node into a data frame, trying twice.
# Some report pages mark the box-score header cells up as <td> instead of <th>. rvest's
# automatic header detection then treats that header as a data row, names the columns
# X1/X2/..., and the table stops looking like a box score even though it is one - which is
# what produced "no box-score tables on the page" on a couple of games. So if the first read
# doesn't look like a box score, read it again forcing row 1 to be the header.
read_box = function(node) {
  tb = tryCatch(clean_names(html_table(node)), error = function(e) NULL)
  if (is_box_table(tb)) return(tb)
  tb2 = tryCatch(clean_names(html_table(node, header = TRUE)), error = function(e) NULL)
  if (is_box_table(tb2)) return(tb2)
  tb   # keep the first read so the failure diagnostic can show its headers
}

# turn one box-score table + its team name into player rows (+ the team-totals row):
parse_box = function(tb, team_name, team_code, matchup, game_id) {
  tb = clean_names(as_tibble(tb, .name_repair = "minimal"))
  
  # Jogador is required; Nr. is not - some pages drop the shirt-number column entirely, and
  # it is only ever used as a second place to look for the "Equipe" totals row.
  jog = pick(tb, "^Jogador", "Jogador")
  nr  = tryCatch(pick(tb, "^Nr", "Nr."), error = function(e) rep(NA_character_, nrow(tb)))
  
  # rebounds: the header itself tells us which side comes first (RD+RO vs RO+RD)
  reb_i  = which(grepl("\\+", names(tb)) & !grepl("/", names(tb)))[1]
  reb_nm = names(tb)[reb_i]
  reb    = as.character(tb[[reb_i]])
  d_first = grepl("^RD", reb_nm, ignore.case = TRUE)
  
  out = tibble(
    GAME_ID = game_id, SEASON = season, LEAGUE = league,
    PLAYER  = jog, TEAM = team_name, CODE = team_code, MATCHUP = matchup,
    IS_TEAM = grepl("equipe", nr, ignore.case = TRUE) |
      grepl("equipe", jog, ignore.case = TRUE),
    MIN  = round(num(pick(tb, "^Min$", "Min"))),
    PTS  = num_before_slash(pick(tb, "^Pts$", "Pts")),
    `2PM` = made(pick(tb, "^2P", "2P%")), `2PA` = att(pick(tb, "^2P", "2P%")),
    `3PM` = made(pick(tb, "^3P", "3P%")), `3PA` = att(pick(tb, "^3P", "3P%")),
    FTM  = made(pick(tb, "^LL", "LL%")),  FTA  = att(pick(tb, "^LL", "LL%")),
    DREB = if (d_first) reb_a(reb) else reb_b(reb),
    OREB = if (d_first) reb_b(reb) else reb_a(reb),
    REB  = reb_t(reb),
    AST = num(pick(tb, "^AS$", "AS")),
    STL = num(pick(tb, "^BR$", "BR")),   # bolas recuperadas = steals
    BLK = num(pick(tb, "^TO$", "TO")),   # tocos = blocks
    TOV = num(pick(tb, "^ER$", "ER")),   # erros = turnovers
    PF  = num(pick(tb, "^FC$", "FC"))    # faltas cometidas = personal fouls
  )
  out %>% filter(!is.na(PLAYER), str_squish(PLAYER) != "")
}

# Team names come from the page <title>. Regular season reads
#   "NBB CAIXA 2026/27 | <home> NN x NN <away> - Liga Nacional de Basquete"
# playoffs/finals append phase info after the away name, e.g.
#   "Playoffs NBB CAIXA 2026/27 | Brasilia 67 x 50 Franca | Semifinais | Jogo 2 - Liga..."
# so strip the tail, strip everything up to the first "|", then split on the score.
teams_from_title = function(title) {
  if (is.na(title)) return(c(NA_character_, NA_character_))
  t = sub("\\s*[-\u2013\u2014]\\s*Liga Nacional de Basquete\\s*$", "", title)
  t = sub("^[^|]*\\|\\s*", "", t)   # drop the "NBB CAIXA 2026/27 |" prefix
  t = sub("\\|.*$", "", t)          # drop any "| Semifinais | Jogo 2" tail
  m = str_match(str_squish(t), "^(.+?)\\s+\\d+\\s*[xX]\\s*\\d+\\s+(.+)$")
  c(str_squish(m[, 2]), str_squish(m[, 3]))
}

# fallback: recover names from the slug if the title ever stops matching
teams_from_slug = function(slug) {
  m = str_match(slug, "^(?:playoffs-|finais-)?nbb-caixa-[0-9-]+-(.+?)-\\d+-x-\\d+-(.+?)$")
  nice = function(s) if (is.na(s)) NA_character_ else str_to_title(gsub("-", " ", s))
  c(nice(m[, 2]), nice(m[, 3]))
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *LOOP OVER FINISHED GAMES AND GET BOXSCORES*

PP = list(); TT = list(); problems = character(0)

for (i in seq_len(nrow(fixtures))) {
  
  fx  = fixtures[i, ]
  url = fx$report_url
  
  res = tryCatch({
    
    report = fetch_html(url)
    if (is.null(report)) stop("fetch failed")
    if (is_404_page(report))
      stop("DEAD LINK: lnb.com.br serves its 404 page here. The match report has been ",
           "unpublished or re-slugged on their side and the schedule still points at the ",
           "old URL. Nothing to fix locally - it will be picked up automatically once LNB ",
           "republishes, since the link is re-read from the schedule on every run.")
    
    nodes = report %>% html_elements("table")
    tabs  = map(nodes, read_box)
    box   = tabs[vapply(tabs, is_box_table, logical(1))]
    
    # Each team gets one full-game table plus one table per period, so the box tables come in
    # two equal blocks: home first, then away. Deriving the away index this way (instead of the
    # old hard-coded tabs[[6]]) is what makes OVERTIME games work - an extra period adds a
    # table to each block and the old index silently read the home side's 5th-period table
    # under the away team's name.
    if (length(box) < 2) {
      # Say WHAT was on the page, so a layout change is diagnosable from the log alone.
      # "chars" separates the two very different causes of an empty result: a few thousand
      # characters means we got a truncated or wrong page, a full-length one means the
      # stats really are missing or the headers moved.
      hdr = map_chr(head(tabs, 4), function(t)
        if (is.null(t) || ncol(t) == 0) "<empty>" else paste(names(t), collapse = ","))
      ptxt = tryCatch(html_text2(report), error = function(e) "")
      stop("no box-score tables (", length(nodes), " tables; ", nchar(ptxt), " chars; ",
           "'BOX SCORE' present: ", grepl("BOX SCORE", ptxt, fixed = TRUE), "; headers: ",
           paste(hdr, collapse = " || "), ")")
    }
    if (length(box) %% 2 != 0) stop("odd number of box tables (", length(box), ") - layout changed")
    half = length(box) / 2
    
    title = report %>% html_element("title") %>% html_text2()
    nm    = teams_from_title(title)
    if (any(is.na(nm))) nm = teams_from_slug(fx$SLUG)
    if (any(is.na(nm))) stop("could not read team names from title or slug")
    
    box_home = parse_box(box[[1]],        nm[1], fx$home_code, fx$MATCHUP, fx$SLUG)
    box_away = parse_box(box[[half + 1]], nm[2], fx$away_code, fx$MATCHUP, fx$SLUG)
    
    # sanity check against the score baked into the report slug - this is what catches a
    # home/away or full-game/period mix-up instead of letting it into the CSV unnoticed
    sc = str_match(url, "-(\\d+)-x-(\\d+)-")
    gp = function(b) { v = b$PTS[b$IS_TEAM]; if (length(v)) v[1] else NA_real_ }
    if (!is.na(sc[, 2])) {
      if (!isTRUE(gp(box_home) == as.numeric(sc[, 2])) ||
          !isTRUE(gp(box_away) == as.numeric(sc[, 3]))) {
        warning("score mismatch for ", fx$SLUG, ": parsed ", gp(box_home), "-", gp(box_away),
                " vs slug ", sc[, 2], "-", sc[, 3], call. = FALSE)
      }
    }
    
    both = bind_rows(box_home, box_away)
    
    PP[[fx$SLUG]] = both %>%
      filter(!IS_TEAM, !is.na(MIN), MIN > 0) %>%
      select(-IS_TEAM) %>%
      mutate(PLAYER = stri_trans_general(toupper(as.character(PLAYER)), "latin-ascii"),
             TEAM   = stri_trans_general(toupper(as.character(TEAM)),   "latin-ascii"))
    
    TT[[fx$SLUG]] = both %>%
      filter(IS_TEAM) %>%
      transmute(TEAM = stri_trans_general(toupper(as.character(TEAM)), "latin-ascii"),
                CODE, MATCHUP, PTS, `2PM`, `2PA`, `3PM`, `3PA`, FTM, FTA,
                DREB, OREB, REB, AST, STL, BLK, TOV, PF)
    
    "ok"
  },
  # One bad page must never kill the whole run. The old script had no guard here, so a single
  # odd report aborted everything and no CSV was written at all.
  error = function(e) paste0("FAILED: ", conditionMessage(e)))
  
  if (res != "ok") {
    problems = c(problems, paste0(fx$SLUG, " -> ", res))
    message("  ", fx$SLUG, " ", res)
  }
  
  if (i %% 25 == 0) message("...", i, "/", nrow(fixtures), " games")
  Sys.sleep(polite)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

players = bind_rows(PP) %>% mutate(TEAM = toupper(TEAM), PLAYER = str_squish(PLAYER))
teams   = bind_rows(TT) %>% mutate(TEAM = toupper(TEAM))

if (nrow(players) == 0)
  stop("parsed 0 player rows - refusing to overwrite BR-players.csv. Problems:\n",
       paste(problems, collapse = "\n"))

# CANONICAL TEAM NAMES: the 3-letter CODE is stable across the whole season, but the printed
# name varies ("Minas Storm" vs "Minas", "Itatiaiucu Cruzeiro Basquete" vs "Cruzeiro", or
# leftover playoff text). Pick ONE canonical name per code - the most frequent spelling seen -
# and apply it to BOTH files. Then drop CODE.
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

write_csv(players, "BR-players.csv")
write_csv(teams,   "BR-teams.csv")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *RUN SUMMARY* - so a partial run is obvious instead of looking like a complete one
message("\n--- summary ---")
dead = grepl("DEAD LINK", problems, fixed = TRUE)
message("fixtures seen:   ", nrow(fixtures))
message("games parsed:    ", length(PP))
message("dead links:      ", sum(dead), "  (missing on lnb.com.br, not a scraper problem)")
message("games failed:    ", sum(!dead))
message("player rows:     ", nrow(players))
message("team rows:       ", nrow(teams), " (expect 2 per parsed game)")
if (length(problems)) {
  message("\nfailures:")
  message(paste(problems, collapse = "\n"))
}