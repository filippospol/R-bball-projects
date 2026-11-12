# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Player on/off ratings for a season from PBP Stats: team Off/Def/Net rating with each player
# ON vs OFF the floor, and the ON-OFF swing.
#
#   pbp_on_off("2026-27")
#
# season      : nba.com format, e.g. "2026-27".
# season_type : "Regular Season", "Playoffs", "All" or "PlayIn".
# Loops the 30 teams (team ids come from pbp_totals - no hoopR). Each team is two requests, so
# ~60 in all; the shared throttle keeps that polite (~a couple of minutes), replacing the old
# blind Sys.sleep(10) that took ~10 minutes.
# NOTE: not verified against a saved response in this pass - it follows the old endpoint's
#       fields, so give it a quick look on first run.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".pbp_read_defined"))
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/pbpstats/pbpstats_helpers.R")

pbp_on_off = function(season, season_type = "Regular Season", clean_names = "none") {
  teams = pbp_totals(season, type = "Team", season_type = season_type, clean_names = "none") %>%
    select(Team, TeamId)

  one_side = function(team_id, stat, on_col, off_col, onoff_col) {
    json = pbp_get("/get-on-off/nba/stat", query = list(
      Season = season, SeasonType = .pbp_enc(season_type), TeamId = team_id, Stat = stat
    ))
    rows = json[[1]]
    if (is.null(rows) || length(rows) == 0) return(NULL)
    df = tibble::as_tibble(rows)
    out = tibble(
      TeamId      = team_id,
      PlayerName  = df[["Name"]],
      EntityId    = if ("EntityId" %in% names(df)) df[["EntityId"]] else NA_character_,
      Minutes     = suppressWarnings(as.numeric(df[["MinutesOn"]] %||% df[["Minutes"]])),
      !!on_col    := suppressWarnings(as.numeric(df[["On"]])),
      !!off_col   := suppressWarnings(as.numeric(df[["Off"]])),
      !!onoff_col := suppressWarnings(as.numeric(df[["OnOff"]]))
    )
    out
  }

  off = map_dfr(teams$TeamId, one_side, stat = "PtsPer100Poss",
                on_col = "OffRtgOn", off_col = "OffRtgOff", onoff_col = "OffRtgOnOff")
  def = map_dfr(teams$TeamId, one_side, stat = "PtsPer100PossOpponent",
                on_col = "DefRtgOn", off_col = "DefRtgOff", onoff_col = "DefRtgOnOff")

  off %>%
    left_join(def %>% select(TeamId, PlayerName, DefRtgOn, DefRtgOff, DefRtgOnOff),
              by = c("TeamId", "PlayerName")) %>%
    left_join(teams, by = "TeamId") %>%
    mutate(NetRtgOn    = OffRtgOn    - DefRtgOn,
           NetRtgOff   = OffRtgOff   - DefRtgOff,
           NetRtgOnOff = OffRtgOnOff - DefRtgOnOff,
           Season      = season) %>%
    select(Season, EntityId, TeamId, PlayerName, Team, Minutes,
           OffRtgOn, OffRtgOff, OffRtgOnOff,
           DefRtgOn, DefRtgOff, DefRtgOnOff,
           NetRtgOn, NetRtgOff, NetRtgOnOff) %>%
    arrange(PlayerName) %>%
    .pbp_case(clean_names)
}
