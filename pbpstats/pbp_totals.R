# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Season totals from PBP Stats, for players, teams or opponents.
# Replaces the old pbp_*_shot_distribution scrapers - shot distribution is just a handful of
# the ~218 columns this returns (AtRimFrequency, Arc3Frequency, ...).
#
#   pbp_totals("2026-27", type = "Player", stat_type = "PerGame")
#
# season      : nba.com format, e.g. "2026-27" (a comma-list like "2026-27,2025-26" also works).
# type        : "Player", "Team" or "Opponent".
# stat_type   : "PerGame", "Totals" or "Per100Possessions".
# season_type : "Regular Season", "Playoffs", "All" or "PlayIn".
# starter_state / start_type : advanced filters (default "All"); e.g. starter_state = "5v5".
# clean_names : "none" (native pbpstats names, default), "all_caps" or "snake".
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".pbp_read_defined"))
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/scraper%20functions/pbpstats/pbpstats_helpers.R")

pbp_totals = function(season, type = "Player", stat_type = "PerGame",
                      season_type = "Regular Season",
                      starter_state = "All", start_type = "All",
                      clean_names = "none") {
  type = match.arg(type, c("Player", "Team", "Opponent"))
  json = pbp_get("/get-totals/nba", query = list(
    Season = season, SeasonType = .pbp_enc(season_type), Type = type,
    StatType = stat_type, StarterState = starter_state, StartType = start_type
  ))
  rows = json[["multi_row_table_data"]]
  if (is.null(rows) || length(rows) == 0)
    stop("pbpstats returned no rows for ", type, " totals, ", season, ".")
  .pbp_tidy(rows, season) %>%
    mutate(across(-any_of(c("Season", "Name", "EntityId", "Team", "TeamId")),
                  ~ suppressWarnings(as.numeric(.x)))) %>%
    .pbp_case(clean_names)
}
