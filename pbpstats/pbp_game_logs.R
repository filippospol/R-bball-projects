# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Game-by-game logs from PBP Stats for one player, team or opponent.
#
#   pbp_game_logs("2026-27", "CJ McCollum")                 # player by name
#   pbp_game_logs("2026-27", "ATL", type = "Team")          # team by nba.com code
#
# season      : nba.com format, e.g. "2026-27".
# entity      : player name or EntityId (type "Player"); team code or TeamId (type "Team"/"Opponent").
# type        : "Player", "Team" or "Opponent".
# season_type : "Regular Season", "Playoffs", "All" or "PlayIn".
# Returns one row per game (Date, GameId, Team, Opponent + stats), with the entity labelled.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".pbp_read_defined"))
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/scraper%20functions/pbpstats/pbpstats_helpers.R")

pbp_game_logs = function(season, entity, type = "Player",
                         season_type = "Regular Season", clean_names = "none") {
  type = match.arg(type, c("Player", "Team", "Opponent"))

  if (type == "Player") {
    who = .pbp_resolve_player(season, as.character(entity), season_type)
    entity_id = who$EntityId; entity_name = who$Name
  } else {
    team_idx = pbp_totals(season, type = "Team", season_type = season_type, clean_names = "none") %>%
      select(any_of(c("Name", "Team", "TeamId")))
    row = if (grepl("^[0-9]+$", as.character(entity)))
            filter(team_idx, TeamId == as.character(entity)) else
            filter(team_idx, toupper(Team) == toupper(entity))
    if (nrow(row) == 0) stop("Could not find team \"", entity, "\" in ", season, " pbpstats totals.")
    entity_id = row$TeamId[1]; entity_name = row$Team[1]
  }

  json = pbp_get("/get-game-logs/nba", query = list(
    Season = season, SeasonType = .pbp_enc(season_type),
    EntityId = entity_id, EntityType = type
  ))
  rows = json[["multi_row_table_data"]]
  if (is.null(rows) || length(rows) == 0)
    stop("pbpstats returned no game logs for ", entity, " (", season, ").")

  .pbp_tidy(rows, season) %>%
    mutate(EntityId = entity_id, EntityName = entity_name, .after = 1) %>%
    mutate(across((-any_of(c("Season", "EntityId", "EntityName", "Team", "Opponent",
                             "GameId", "Date"))) & where(is.character),
                  ~ suppressWarnings(as.numeric(.x)))) %>%
    .pbp_case(clean_names)
}
