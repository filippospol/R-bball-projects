# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script extracts box-score data for the Spanish ACB (Liga Endesa).
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
library(httr)
library(jsonlite)
library(glue)
library(lubridate)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *CONFIG*
league        = "ACB"
season        = "2025-26"
competitionId = 1                     # Liga Endesa (regular season + play-offs)
BASE_API      = "https://api2.acb.com/api/matchdata"
SEASONDATA    = "https://api2.acb.com/api/seasondata/Competition/matches"
# editionId + first roundId are looked up live from SEASONDATA (see acb_discover);
# nothing about a specific season is hardcoded below.

# x-apikey: a STATIC public key baked into the live.acb.com front-end (it does NOT
# rotate daily). Keep it in a GitHub secret; the value below is the fallback default.
# If a request ever returns 401 (ACB rotated the key), acb_get auto-refreshes it
# from the site via get_acb_apikey() and retries - so this needs no maintenance.
ACB_APIKEY = Sys.getenv("ACB_APIKEY", "0dd94928-6f57-4c08-a3bd-b1b2f092976e")
ACB_UA = paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ",
                "(KHTML, like Gecko) Chrome/151.0.0.0 Safari/537.36")

# The `season` string above is the ONLY per-season value. editionId and the first
# roundId are resolved live from SEASONDATA; the sweep then walks up from there and
# stops after a long run of off-edition rounds, capturing the regular season AND the
# (separate, higher) play-off roundId block with nothing hardcoded.
gap_tolerance = 150     # consecutive off-edition roundIds before the sweep stops

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *AUTO-EXTRACT x-apikey* (only used as a fallback if the stored key 401s)
# The key lives in the live.acb.com front-end. In R we can read the page's scripts
# (unlike a headless fetch), so we scan the HTML + its JS chunks for the key.
get_acb_apikey = function() {
  uuid = "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"
  grab = function(txt) {
    if (is.na(txt) || !nzchar(txt)) return(NA_character_)
    m = str_match(txt, paste0("(?i)api[_-]?key[\"'`:=\\s]{0,8}(", uuid, ")"))
    if (!is.na(m[1, 2])) return(m[1, 2])
    if (str_detect(txt, "x-apikey")) {          # header name present -> take a nearby UUID
      u = str_extract(txt, uuid); if (!is.na(u)) return(u)
    }
    NA_character_
  }
  home = tryCatch(content(GET("https://live.acb.com/", user_agent(ACB_UA)),
                          "text", encoding = "UTF-8"), error = function(e) "")
  k = grab(home); if (!is.na(k)) return(k)
  chunks = unique(str_extract_all(home, "/_next/static/[^\"']+\\.js")[[1]])
  for (u in paste0("https://live.acb.com", chunks)) {
    js = tryCatch(content(GET(u, user_agent(ACB_UA)), "text", encoding = "UTF-8"),
                  error = function(e) "")
    k = grab(js); if (!is.na(k)) return(k)
  }
  stop("Could not auto-extract x-apikey from live.acb.com.")
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *LOW-LEVEL FETCH (x-apikey auth; retry network/5xx, skip 4xx, refresh key on 401)*
acb_get = function(url, tries = 3) {
  refreshed = FALSE
  for (k in seq_len(tries)) {
    res = tryCatch(
      GET(url,
          add_headers(`x-apikey` = ACB_APIKEY, Accept = "*/*",
                      Origin = "https://live.acb.com", Referer = "https://live.acb.com/"),
          user_agent(ACB_UA), timeout(30)),
      error = function(e) NULL)
    if (is.null(res)) { Sys.sleep(k); next }              # network error -> retry
    sc = status_code(res)
    if (sc == 200) {
      txt = content(res, "text", encoding = "UTF-8")
      if (nchar(txt) > 0) return(fromJSON(txt, simplifyVector = FALSE))
      return(NULL)
    }
    if (sc == 401 && !refreshed) {                        # key may have rotated -> refresh once
      new_key = tryCatch(get_acb_apikey(), error = function(e) NULL)
      if (!is.null(new_key)) {
        ACB_APIKEY <<- new_key; refreshed = TRUE
        message("ACB_APIKEY was rejected; refreshed from live.acb.com and retrying.")
        next
      }
    }
    if (sc >= 400 && sc < 500) return(NULL)               # 4xx (incl. unrefreshable 401) -> skip
    Sys.sleep(k)                                          # 5xx -> retry
  }
  NULL
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *HELPERS*
`%||%` = function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1]) || identical(a, "")) b else a
mmss_to_min = function(x) {                               # "20:29" -> 20 (rounded minutes)
  x = str_trim(x %||% "")
  if (!str_detect(x, "\\d+:\\d+")) return(NA_real_)
  p = as.integer(str_split(x, ":", simplify = TRUE))
  round((p[1] * 60 + p[2]) / 60)
}
clean_name = function(x) stri_trans_general(toupper(x %||% ""), "latin-ascii")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *EXTRACT MATCH ID'S*  (sweep rounds -> keep this competition/edition -> match id + date)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *DISCOVER edition + first round from SEASONDATA*  (no hardcoded season ids)
# availableFilters.seasons maps year -> edition id; availableFilters.rounds gives
# the regular-season roundIds, whose minimum is Jornada 1.
acb_discover = function(season_str, competitionId) {
  base = acb_get(glue("{SEASONDATA}?competitionId={competitionId}&isRoundSelected=true"))
  if (is.null(base)) stop("SEASONDATA unreachable - check ACB_APIKEY.")
  
  startY  = as.integer(str_match(season_str, "(\\d{4})")[, 2])
  hit     = purrr::detect(base$availableFilters$seasons, ~ isTRUE(.x$seasonStartYear == startY))
  if (is.null(hit)) stop("Season ", season_str, " not found in SEASONDATA.")
  edId = hit$id
  
  # If the default response isn't already this season, re-select it (auto-detect
  # the query-param name across a few candidates).
  sd = base
  if (!isTRUE(base$selectedFilters$season == edId)) {
    sd = NULL
    for (p in c("season", "seasonId", "editionId")) {
      d = acb_get(glue("{SEASONDATA}?competitionId={competitionId}&{p}={edId}&isRoundSelected=false"))
      if (!is.null(d) && isTRUE(d$selectedFilters$season == edId)) { sd = d; break }
    }
    if (is.null(sd)) stop("Could not select season ", season_str,
                          " (edition ", edId, ") - the season query param may have changed.")
  }
  rounds = purrr::map_int(sd$availableFilters$rounds, "id")
  if (length(rounds) == 0) stop("No rounds returned for season ", season_str, ".")
  list(editionId = edId, first_round = min(rounds))
}

disc        = acb_discover(season, competitionId)
editionId   = disc$editionId
first_round = disc$first_round
message("ACB ", season, " -> editionId ", editionId, ", first roundId ", first_round)

fixture_info = local({
  rows = list(); misses = 0L; rid = first_round
  repeat {
    raw = acb_get(glue("{BASE_API}/Menu/matchlist?roundId={rid}&format=round"))
    good = !is.null(raw) &&
      isTRUE(raw$competitionId == competitionId) &&
      isTRUE(raw$editionId == editionId) &&
      length(raw$matches) > 0
    if (good) {
      misses = 0L
      Sys.sleep(0.15)
      rows[[length(rows) + 1L]] = map_df(raw$matches, function(m) tibble(
        GAME_ID   = m$id,
        GAME_DATE = as_date(ymd_hms(m$startDateTime)),
        STATUS    = m$matchStatus %||% NA_character_
      ))
    } else {
      misses = misses + 1L
    }
    if (misses >= gap_tolerance) break
    rid = rid + 1L
  }
  bind_rows(rows)
}) %>%
  distinct(GAME_ID, .keep_all = TRUE) %>%
  filter(STATUS == "FINALIZED") %>%          # played games only (drops scheduled/future)
  arrange(GAME_DATE)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *PARSE ONE TEAM'S FULL-GAME BOX SCORE*  (verified against real data)
# statsByPeriods has quarter 0 (full game) + 1..4; we take quarter 0.
parse_team = function(tb, game_id, matchup) {
  per0  = purrr::detect(tb$statsByPeriods, ~ .x$quarter == 0)
  tname = tb$team$fullName
  tcode = tb$team$abbreviatedName
  
  players = map_df(per0$stats$players, function(p) tibble(
    GAME_ID = game_id, SEASON = season, LEAGUE = league,
    PLAYER  = clean_name(p$player$nickname %||% paste(p$player$firstName, p$player$lastName)),
    TEAM    = clean_name(tname), MATCHUP = matchup,
    MIN  = mmss_to_min(p$playTime), PTS = p$points,
    `2PM`= p$twoPointersMade,   `2PA`= p$twoPointersAttempted,
    `3PM`= p$threePointersMade, `3PA`= p$threePointersAttempted,
    FTM  = p$freeThrowsMade,    FTA  = p$freeThrowsAttempted,
    DREB = p$defRebounds, OREB = p$offRebounds, REB = p$totalRebounds,
    AST  = p$assists, STL = p$steals, TOV = p$turnovers, BLK = p$blocks, PF = p$personalFouls
  ))
  
  tot = per0$stats$total
  team = tibble(
    TEAM = clean_name(tname), CODE = tcode, MATCHUP = matchup,
    PTS = tot$points, `2PM`= tot$twoPointersMade, `2PA`= tot$twoPointersAttempted,
    `3PM`= tot$threePointersMade, `3PA`= tot$threePointersAttempted,
    FTM = tot$freeThrowsMade, FTA = tot$freeThrowsAttempted,
    DREB = tot$defRebounds, OREB = tot$offRebounds, REB = tot$totalRebounds,
    AST = tot$assists, STL = tot$steals, TOV = tot$turnovers, BLK = tot$blocks, PF = tot$personalFouls
  )
  list(players = players, team = team)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *LOOP OVER MATCH ID'S AND GET BOXSCORES*
PP = list(); TT = list()

for (i in seq_len(nrow(fixture_info))) {
  Sys.sleep(0.4)                                          # be a polite bot
  gid = fixture_info$GAME_ID[i]
  
  raw = acb_get(glue("{BASE_API}/Result/boxscores?matchId={gid}"))
  if (is.null(raw) || !isTRUE(raw$matchFinished)) next
  
  home = raw$teamBoxscores[[1]]                           # [[1]] = home, [[2]] = away
  away = raw$teamBoxscores[[2]]
  matchup = paste0(fixture_info$GAME_DATE[i], ", ",
                   home$team$abbreviatedName, " vs ", away$team$abbreviatedName)
  
  h = parse_team(home, gid, matchup)
  a = parse_team(away, gid, matchup)
  
  PP[[i]] = bind_rows(h$players, a$players) %>%
    select(GAME_ID,SEASON,LEAGUE,PLAYER,TEAM,MATCHUP,MIN,PTS,
           `2PM`,`2PA`,`3PM`,`3PA`,FTM,FTA,DREB,OREB,REB,AST,STL,TOV,BLK,PF) %>%
    filter(!is.na(MIN))
  TT[[i]] = bind_rows(h$team, a$team) %>%
    select(TEAM,CODE,MATCHUP,PTS,`2PM`,`2PA`,`3PM`,`3PA`,FTM,FTA,
           DREB,OREB,REB,AST,STL,TOV,BLK,PF)
}

rm(list=setdiff(ls(),c("PP","TT")))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' *WRITE FILES*
write.csv(bind_rows(PP) %>% mutate(TEAM = toupper(TEAM)), "bball-stats/data/ES-players.csv")
write.csv(bind_rows(TT) %>% mutate(TEAM = toupper(TEAM)), "bball-stats/data/ES-teams.csv")