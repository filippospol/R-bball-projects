# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# WOWY (with-or-without-you) combination stats from PBP Stats for two or more team-mates.
# For N players you get every on/off combination (2^N rows): all off, each subset on, all on.
#
#   pbp_wowy("2026-27", c("Dorian Finney-Smith", "Nickeil Alexander-Walker"))
#
# season       : nba.com format, e.g. "2026-27".
# players      : a vector of 2+ player names (or EntityIds) on the SAME team.
# season_type  : "Regular Season", "Playoffs", "All" or "PlayIn".
# only_common_games : TRUE restricts to games all the players appeared in.
# Returns On / Off splits with Minutes, OffRtg, DefRtg, NetRtg and FG% (team + opponent).
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".pbp_read_defined"))
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/pbpstats/pbpstats_helpers.R")

pbp_wowy = function(season, players, season_type = "Regular Season",
                    only_common_games = FALSE, clean_names = "none") {
  if (length(players) < 2) stop("Give at least two players for a WOWY combination.")

  resolved = lapply(as.character(players), function(p) .pbp_resolve_player(season, p, season_type))
  team_ids = unique(vapply(resolved, `[[`, "", "TeamId"))
  if (length(team_ids) > 1)
    warning("Players span multiple teams; using the first player's team (", resolved[[1]]$Team, ").")
  team_id    = resolved[[1]]$TeamId
  player_ids = paste(vapply(resolved, `[[`, "", "EntityId"), collapse = ",")

  json = pbp_get("/get-wowy-combination-stats/nba", query = list(
    Season = season, SeasonType = .pbp_enc(season_type),
    TeamId = team_id, PlayerIds = player_ids,
    OnlyCommonGames = tolower(as.character(only_common_games))
  ))
  res = json[["results"]]
  if (is.null(res) || length(res) == 0) stop("pbpstats returned no WOWY rows.")

  keep = c("On", "Off", "Minutes", "OffRtg", "DefRtg", "NetRtg",
           "Fg2Pct", "Fg3Pct", "OppFg2Pct", "OppFg3Pct")
  tibble::as_tibble(res) %>%
    select(any_of(keep)) %>%
    mutate(Season = season, Team = resolved[[1]]$Team, .before = 1) %>%
    mutate(across(any_of(c("Minutes", "OffRtg", "DefRtg", "NetRtg",
                           "Fg2Pct", "Fg3Pct", "OppFg2Pct", "OppFg3Pct")),
                  ~ suppressWarnings(as.numeric(.x)))) %>%
    .pbp_case(clean_names)
}
