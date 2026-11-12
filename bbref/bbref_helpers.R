# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Shared helpers for the Basketball Reference scrapers.
# Every bbref_* function sources this once (guarded) so the whole toolkit shares:
#   - ONE polite fetcher (browser UA + a session-wide <=~19 requests/minute throttle
#     + 429 back-off), because bbref rate-limits around 20 req/min and will ban for
#     an hour if you blow past it;
#   - the comment-unwrap step (bbref hides many tables inside HTML comments, so a plain
#     read_html() can't see them);
#   - the NBA.com conventions you asked for: season input "YYYY-YY" (e.g. "2026-27"),
#     team codes exactly as nba.com (BRK->BKN, CHO->CHA, PHO->PHX), and traded players
#     kept as one row per team PLUS a combined row whose team shows the real codes
#     (e.g. "DAL/LAL") instead of 2TM/3TM/TOT.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pacman::p_load(dplyr, tidyr, stringr, purrr, rvest, xml2, httr, glue, tibble)

# ---- Rate limiter ---------------------------------------------------------------------------
# Shared across EVERY bbref_* call in the session via options(), so firing several
# scrapers back-to-back (or looping game logs) still stays under bbref's ~20/min cap.
# Keeps >= min_gap seconds between requests and <= max_per_min in any rolling 60s.
.bbref_throttle = function(max_per_min = 19, min_gap = 3.2) {
  st  = getOption("bbref.req_times", numeric(0))
  now = as.numeric(Sys.time())
  if (length(st)) {                                   # minimum gap since last request
    wait = min_gap - (now - max(st))
    if (wait > 0) { Sys.sleep(wait); now = as.numeric(Sys.time()) }
  }
  st = st[now - st < 60]                               # rolling 60s window
  if (length(st) >= max_per_min) {
    wait = 60 - (now - min(st)) + 0.1
    if (wait > 0) Sys.sleep(wait)
    now = as.numeric(Sys.time()); st = st[now - st < 60]
  }
  options(bbref.req_times = c(st, as.numeric(Sys.time())))
  invisible(TRUE)
}

# ---- Polite fetch + comment-unwrap ----------------------------------------------------------
# Returns a parsed HTML document with bbref's comment-wrapped tables exposed, so tables
# can be selected by their stable id regardless of whether the server comments them out.
bbref_read = function(url, max_tries = 3) {
  ua = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0 Safari/537.36"
  for (attempt in seq_len(max_tries)) {
    .bbref_throttle()
    resp = tryCatch(httr::GET(url, httr::user_agent(ua), httr::timeout(60)),
                    error = function(e) NULL)
    if (!is.null(resp) && httr::status_code(resp) == 200) {
      txt = httr::content(resp, as = "text", encoding = "UTF-8")
      txt = gsub("<!--|-->", "", txt, perl = TRUE)      # expose commented tables
      return(xml2::read_html(txt))
    }
    code = if (is.null(resp)) NA_integer_ else httr::status_code(resp)
    if (!is.na(code) && code == 429) {                  # rate-limited: honour Retry-After
      ra = suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))
      Sys.sleep(if (is.finite(ra) && ra > 0) ra + 1 else 60)
      next
    }
    if (attempt < max_tries) Sys.sleep(5) else
      stop("bbref_read(): could not fetch ", url,
           if (!is.na(code)) paste0(" (HTTP ", code, ")") else " (no response)")
  }
}

# ---- Small utilities ------------------------------------------------------------------------
# bbref season end-year from an nba.com-format season string: "2026-27" -> 2027.
.bbref_year = function(season) {
  if (!grepl("^[0-9]{4}-[0-9]{2}$", season))
    stop("season must be nba.com format, e.g. \"2026-27\" (got: ", season, ")")
  as.numeric(substr(season, 1, 4)) + 1
}

# bbref 3-letter team code -> nba.com code (only three ever differ).
.bbref_nba_code = function(x) dplyr::recode(x, BRK = "BKN", CHO = "CHA", PHO = "PHX")

# Pull the bbref team code out of a /teams/XXX/YYYY.html href, then map to nba.com.
.bbref_code_from_href = function(href) .bbref_nba_code(str_match(href, "/teams/([A-Z]{3})/")[, 2])

# Select a table node by id from a (comment-unwrapped) document.
.bbref_table = function(doc, id) {
  t = html_element(doc, paste0("table#", id))
  if (inherits(t, "xml_missing") || length(t) == 0) return(NULL)
  t
}

# The main regular-season player table on a stat page: the one carrying a
# data-stat="name_display" cell whose id does NOT end in "_post" (that's the playoffs copy).
.bbref_player_table = function(doc) {
  tabs = html_elements(doc, "table")
  has_player = map_lgl(tabs, ~ length(html_elements(.x, "[data-stat='name_display']")) > 0)
  tabs = tabs[has_player]
  if (!length(tabs)) stop("No player-stats table found on the page.")
  ids = map_chr(tabs, ~ html_attr(.x, "id") %||% "")
  reg = tabs[!str_ends(ids, "_post")]
  if (length(reg)) reg[[1]] else tabs[[1]]
}

# Turn a bbref table node into a tibble keyed by data-stat, dropping the periodic
# in-body header rows (<tr class="thead">) and the blank DUMMY spacer columns.
.bbref_extract = function(tbl, drop_class = "thead") {
  stat_names = tbl %>% html_element("thead") %>% html_elements("tr") %>% tail(1) %>%
    html_elements("th,td") %>% html_attr("data-stat")
  stat_names = unique(stat_names[!is.na(stat_names) & stat_names != "" & stat_names != "DUMMY"])
  rows = tbl %>% html_elements("tbody > tr")
  cls  = rows %>% html_attr("class") %>% tidyr::replace_na("")
  rows = rows[!str_detect(cls, drop_class)]
  getcol = function(stat) rows %>% html_element(sprintf("[data-stat='%s']", stat)) %>% html_text2()
  df = stat_names %>% rlang::set_names() %>% map(getcol) %>% as_tibble()
  # a href-derived nba team code, when the identity column is a full team name with a link
  df
}

# One row per team + a combined row (team = real codes joined, not 2TM/3TM/TOT).
# Expects a `TEAM` column already mapped to nba.com codes for single-team rows; the
# aggregate rows still carry the bbref "2TM"/"3TM"/... marker, which we relabel here.
.bbref_regroup_traded = function(df, player_col = "PLAYER_NAME", team_col = "TEAM") {
  is_agg = str_detect(df[[team_col]], "^[0-9]+TM$")
  if (!any(is_agg)) return(df)
  df$.rowid = seq_len(nrow(df))
  combo = df %>%
    filter(!is_agg) %>%
    group_by(.data[[player_col]]) %>%
    summarise(codes = paste(.data[[team_col]], collapse = "/"), .groups = "drop")
  agg_idx = which(is_agg)
  for (i in agg_idx) {
    pl = df[[player_col]][i]
    code = combo$codes[match(pl, combo[[player_col]])]
    if (!is.na(code)) df[[team_col]][i] = code
  }
  df$.rowid = NULL
  df
}

# Clean, uppercase column names for the player tables: rename the identity columns,
# uppercase the rest of the data-stat names. Mode-agnostic.
.bbref_player_names = c(
  name_display = "PLAYER_NAME", team_name_abbr = "TEAM", pos = "POS", age = "AGE",
  games = "GP", games_started = "GS", mp = "MIN", mp_per_g = "MIN"
)
.bbref_clean_player = function(df) {
  df = df %>% select(-any_of(c("ranker", "awards")))
  nm = names(df)
  ren = ifelse(nm %in% names(.bbref_player_names), .bbref_player_names[nm], toupper(nm))
  names(df) = make.unique(unname(ren), sep = "_")
  df
}

# Convert every column except the listed character ones to numeric (quietly).
.bbref_numify = function(df, keep_chr) {
  df %>% mutate(across(-any_of(keep_chr), ~ suppressWarnings(as.numeric(.x))))
}

`%||%` = function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

.bbref_read_defined = TRUE
