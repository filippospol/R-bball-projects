# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Shared helpers for the PBP Stats (api.pbpstats.com) scrapers.
# Every pbp_* function sources this once (guarded). It gives the toolkit:
#   - ONE fetcher with a session-wide polite throttle + retry/429 back-off (pbpstats is a
#     small site, so default spacing is gentle);
#   - NO dependency on hoopR / stats.nba.com - the pbpstats responses already carry Name,
#     EntityId (nba.com player id), TeamId and TeamAbbreviation (already nba.com codes),
#     so names and ids come straight from the data;
#   - a cached player index per season so looking up several players (e.g. for WOWY) costs
#     one request, not one per player.
# Seasons go in as nba.com strings ("2026-27"), which is exactly what the API expects.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pacman::p_load(dplyr, tidyr, stringr, purrr, tibble, httr, jsonlite, glue, janitor)

# ---- Session-wide throttle (shared across every pbp_* call via options) ----------------------
.pbp_throttle = function(max_per_min = 20, min_gap = 2.5) {
  st  = getOption("pbp.req_times", numeric(0))
  now = as.numeric(Sys.time())
  if (length(st)) {
    wait = min_gap - (now - max(st))
    if (wait > 0) { Sys.sleep(wait); now = as.numeric(Sys.time()) }
  }
  st = st[now - st < 60]
  if (length(st) >= max_per_min) {
    wait = 60 - (now - min(st)) + 0.1
    if (wait > 0) Sys.sleep(wait)
    now = as.numeric(Sys.time()); st = st[now - st < 60]
  }
  options(pbp.req_times = c(st, as.numeric(Sys.time())))
  invisible(TRUE)
}

# ---- One fetcher for the whole API ----------------------------------------------------------
# path e.g. "/get-totals/nba"; query is a named list. Returns the parsed JSON (a list).
pbp_get = function(path, query = list(), max_tries = 3) {
  ua  = "bball-projects R scraper (contact: filippospol on github)"
  url = paste0("https://api.pbpstats.com", path)
  for (attempt in seq_len(max_tries)) {
    .pbp_throttle()
    resp = tryCatch(httr::GET(url, query = query, httr::user_agent(ua), httr::timeout(90)),
                    error = function(e) NULL)
    if (!is.null(resp) && httr::status_code(resp) == 200)
      return(jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8")))
    code = if (is.null(resp)) NA_integer_ else httr::status_code(resp)
    if (!is.na(code) && code == 429) {
      ra = suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))
      Sys.sleep(if (is.finite(ra) && ra > 0) ra + 1 else 30); next
    }
    if (attempt < max_tries) Sys.sleep(5) else
      stop("pbp_get(): could not fetch ", url,
           if (!is.na(code)) paste0(" (HTTP ", code, ")") else " (no response)")
  }
}

# ---- Shared tidy step -----------------------------------------------------------------------
# A list-of-rows (multi_row_table_data / results) -> tibble with native pbpstats names.
# Identity columns are renamed to a consistent front block and `Season` is prepended.
.pbp_tidy = function(rows, season) {
  df = tibble::as_tibble(rows)
  if (nrow(df) == 0) return(df)
  if ("TeamAbbreviation" %in% names(df)) names(df)[names(df) == "TeamAbbreviation"] = "Team"
  df$ShortName = NULL
  front = intersect(c("Name", "EntityId", "Team", "TeamId"), names(df))
  df %>% mutate(Season = season, .before = 1) %>% relocate(all_of(front), .after = Season)
}

# Apply the requested column-name case, LAST (after numeric conversion), so downstream code
# can rely on native names. "none" keeps pbpstats' own names.
.pbp_case = function(df, clean_names = c("none", "all_caps", "snake")) {
  clean_names = match.arg(clean_names)
  if (clean_names == "all_caps") df = janitor::clean_names(df, case = "all_caps")
  if (clean_names == "snake")    df = janitor::clean_names(df, case = "snake")
  df
}

# ---- url helpers / identity (no hoopR) ------------------------------------------------------
.pbp_enc = function(x) gsub(" ", "+", x, fixed = TRUE)   # "Regular Season" -> "Regular+Season"

# Cached {Name, EntityId, TeamId, Team} index for a season, built from ONE totals(Player) call.
.pbp_player_index = function(season, season_type = "Regular Season") {
  key = paste0("pbp.pidx.", season, ".", season_type)
  hit = getOption(key); if (!is.null(hit)) return(hit)
  tot = pbp_totals(season, type = "Player", season_type = season_type, clean_names = "none")
  idx = tot %>% select(any_of(c("Name", "EntityId", "TeamId", "Team")))
  do.call(options, setNames(list(idx), key)); idx
}

# Resolve a player name (or id) to EntityId + TeamId for the wowy / game-log endpoints.
.pbp_resolve_player = function(season, who, season_type = "Regular Season") {
  idx = .pbp_player_index(season, season_type)
  if (grepl("^[0-9]+$", who)) {
    row = idx %>% filter(EntityId == who)
  } else {
    row = idx %>% filter(tolower(Name) == tolower(who))
  }
  if (nrow(row) == 0) stop("Could not find player \"", who, "\" in ", season, " pbpstats totals.")
  list(EntityId = row$EntityId[1], TeamId = row$TeamId[1], Name = row$Name[1], Team = row$Team[1])
}

`%||%` = function(a, b) if (is.null(a) || length(a) == 0) b else a

.pbp_read_defined = TRUE
