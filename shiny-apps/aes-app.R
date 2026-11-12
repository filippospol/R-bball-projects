# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script contains the source code for the Adjusted Era Stats application.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Setup environment:
pacman::p_load(tidyverse,glue,shiny,bslib,bsicons,plotly,reactable,shinycssloaders)

Sys.setenv(RETICULATE_PYTHON = "")
options(rsconnect.http.timeout = 120)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Raw data:
aesRaw = suppressMessages(read_csv("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/shiny-apps/aes-raw-NEW.csv")) %>%
  distinct(SEASON, PLAYER, .keep_all = TRUE) %>%
  # Current season stats filter
  filter(case_when(SEASON == "2026-27" ~ MIN >= 1000 & MIN/GP >= 25,
                   TRUE ~ MIN >= 1500 & MIN/GP >= 25)) %>%
  mutate(PTS_75 = round(PTS_100*0.75, 2),
         AST_75 = round(AST_100*0.75, 2),
         REB_75 = round(REB_100*0.75, 2),
         MVP    = as.logical(MVP)) %>%
  # League-relative rate metrics (100 = average qualified player that season), the TS+ analogues
  group_by(SEASON) %>%
  mutate(REB_PLUS = round(REB_PCT / mean(REB_PCT, na.rm = TRUE) * 100),
         AST_PLUS = round(AST_PCT / mean(AST_PCT, na.rm = TRUE) * 100)) %>%
  ungroup()

# Adjusted Volume builder ---------------------------------------------------------------------
# vol   : per-75 volume column       (PTS_75 / REB_75 / AST_75)
# rate  : era-neutral rate column    (TS_PLUS / REB_PLUS / AST_PLUS)
# score : name of the resulting metric (AVS / AVR / AVA)
# top_n : players per season used for the benchmark average
build_av = function(data, vol, rate, score, top_n = 25, w_vol = 0.55, w_rate = 0.45) {
  data %>%
    group_by(SEASON) %>%
    arrange(desc(.data[[vol]]), .by_group = TRUE) %>%
    slice_head(n = top_n) %>%
    mutate(TOP_AVG     = round(mean(.data[[vol]]), 1),
           ABOVE_BELOW = round(.data[[vol]] - TOP_AVG, 1)) %>%
    ungroup() %>%
    transmute(SEASON, PLAYER, MVP, TEAM, POS, AGE, MIN,
              VOL  = .data[[vol]],
              TOP_AVG, ABOVE_BELOW,
              RATE = .data[[rate]]) %>%
    mutate(VOL_Z  = as.numeric(scale(ABOVE_BELOW)),
           RATE_Z = as.numeric(scale(RATE)),
           SCORE  = round(w_vol*VOL_Z + w_rate*RATE_Z, 3)) %>%
    arrange(desc(SCORE)) %>%
    mutate(Rk  = row_number(),
           PCT = dense_rank(SCORE) / n_distinct(SCORE) * 100) %>%
    select(Rk, PLAYER, MVP, SEASON, TEAM, POS, AGE, MIN, VOL, TOP_AVG, ABOVE_BELOW, RATE, SCORE, PCT)
}

# The three leaderboards: same recipe, different inputs
AV = list(
  scoring    = list(data = build_av(aesRaw, "PTS_75", "TS_PLUS",  "AVS"),
                    vol = "Pts/75", rate = "TS+",  score = "AVS", full = "Adjusted Volume Scoring"),
  rebounding = list(data = build_av(aesRaw, "REB_75", "REB_PLUS", "AVR"),
                    vol = "Reb/75", rate = "TRB+", score = "AVR", full = "Adjusted Volume Rebounding"),
  passing    = list(data = build_av(aesRaw, "AST_75", "AST_PLUS", "AVA"),
                    vol = "Ast/75", rate = "AST+", score = "AVA", full = "Adjusted Volume Passing")
)

# Player view data --------------------------------------------------------------------------------
# One row per player-season with all three scores (NA where the player was not in that stat's top-25)
pick = function(x, score) x$data %>% select(SEASON, PLAYER, MVP, TEAM, POS, AGE, MIN,
                                            !!paste0(score, "_VOL") := VOL, !!paste0(score, "_RATE") := RATE,
                                            !!score := SCORE, !!paste0(score, "_PCT") := PCT)
playerSeasons = aesRaw %>%
  select(SEASON, PLAYER, MVP, TEAM, POS, AGE, MIN) %>%
  left_join(pick(AV$scoring,    "AVS") %>% select(-c(MVP, TEAM, POS, AGE, MIN)), by = c("SEASON", "PLAYER")) %>%
  left_join(pick(AV$rebounding, "AVR") %>% select(-c(MVP, TEAM, POS, AGE, MIN)), by = c("SEASON", "PLAYER")) %>%
  left_join(pick(AV$passing,    "AVA") %>% select(-c(MVP, TEAM, POS, AGE, MIN)), by = c("SEASON", "PLAYER")) %>%
  arrange(PLAYER, SEASON)

# Per-player summary: qualifying seasons in the raw data, top-25 appearances per stat, best scores
playerSummary = playerSeasons %>%
  group_by(PLAYER) %>%
  summarise(seasons  = n(),
            first    = min(SEASON), last = max(SEASON),
            mvps     = sum(MVP, na.rm = TRUE),
            top_avs  = sum(!is.na(AVS)), top_avr = sum(!is.na(AVR)), top_ava = sum(!is.na(AVA)),
            best_avs = suppressWarnings(max(AVS, na.rm = TRUE)),
            best_avr = suppressWarnings(max(AVR, na.rm = TRUE)),
            best_ava = suppressWarnings(max(AVA, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(across(starts_with("best_"), ~ ifelse(is.finite(.x), .x, NA)))

# Picker choices: alphabetical, with the span of seasons the player appears in the database (end years)
playerChoices = playerSummary %>%
  arrange(PLAYER) %>%
  transmute(value = PLAYER, label = PLAYER,
            span  = paste0(as.integer(substr(first, 1, 4)) + 1, "-", as.integer(substr(last, 1, 4)) + 1))

# Selectize renderer: name in normal type, season span smaller and grey
aes_player_render = I('{
  option: function(item, escape) {
    return "<div>" + escape(item.label) +
           " <span style=\'font-size:.82em;color:#7a8a93\'>(" + escape(item.span) + ")</span></div>";
  },
  item: function(item, escape) {
    return "<div>" + escape(item.label) +
           " <span style=\'font-size:.82em;color:#7a8a93\'>(" + escape(item.span) + ")</span></div>";
  }
}')
defaultPlayer = AV$scoring$data$PLAYER[1]

# Headline numbers for the Home page (computed from the data, so they update with every refresh)
aesStats = list(
  seasons = n_distinct(aesRaw$SEASON),
  first   = min(aesRaw$SEASON),
  last    = max(aesRaw$SEASON),
  rows    = nrow(aesRaw),
  players = n_distinct(aesRaw$PLAYER)
)

# Season trends: the top-25 average each player is measured against, per stat
aesTrend = imap_dfr(AV, ~ .x$data %>% distinct(SEASON, TOP_AVG) %>% mutate(stat = .y)) %>%
  mutate(stat = recode(stat, scoring = "PTS", rebounding = "REB", passing = "AST")) %>%
  pivot_wider(names_from = stat, values_from = TOP_AVG) %>%
  arrange(SEASON)

# One trend chart per stat: season points plus a smoothed trend line
aes_trend_plot = function(stat, label, span = 0.4) {
  d = aesTrend %>%
    transmute(SEASON, value = .data[[stat]]) %>%
    mutate(idx = row_number(),
           smooth = predict(loess(value ~ idx, span = span)))
  plot_ly(d, x = ~SEASON) %>%
    add_trace(y = ~smooth, type = "scatter", mode = "lines", name = "Trend",
              line = list(color = "#1d7898", width = 3, shape = "spline"),
              hoverinfo = "skip") %>%
    add_trace(y = ~value, type = "scatter", mode = "markers", name = "Top-25 average",
              marker = list(color = "#e16a0a", size = 8, line = list(color = "#fff", width = 1)),
              text = ~paste0(SEASON, ": ", round(value, 1), " ", label),
              hoverinfo = "text") %>%
    layout(
      showlegend = FALSE,
      xaxis = list(title = "", type = "category", tickangle = -45,
                   tickvals = d$SEASON[seq(1, nrow(d), by = 5)],
                   showgrid = FALSE, tickfont = list(color = "#4a5a63")),
      yaxis = list(title = label, gridcolor = "#e3e8ec", zeroline = FALSE,
                   tickfont = list(color = "#4a5a63")),
      margin = list(t = 10, r = 10, b = 60),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Barlow", size = 13),
      hoverlabel = list(bgcolor = "#165a72", font = list(color = "#fff", family = "Barlow"))
    ) %>%
    config(displayModeBar = FALSE)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# User Interface:
aes_theme = bslib::bs_theme(
  version      = 5,
  bg           = "#f6f8fa",
  fg           = "#1b2a33",
  primary      = "#e16a0a",
  secondary    = "#165a72",
  base_font    = bslib::font_google("Barlow", wght = c(400, 500, 600)),
  heading_font = bslib::font_google("Barlow Condensed", wght = c(600, 700)),
  "navbar-bg"  = "#165a72",
  "border-radius" = "6px"
)

aes_css = HTML('
  /* ---- Navbar ------------------------------------------------------------ */
  .navbar { padding-top: .4rem; padding-bottom: .4rem; position: sticky; top: 0; z-index: 1030; }
  .navbar-brand { display:flex; align-items:center; gap:.6rem;
                  font-family:"Barlow Condensed"; font-weight:700; font-size:1.55rem;
                  letter-spacing:.02em; color:#fff !important; }
  .navbar-brand img { height:34px; }
  .navbar-nav .nav-link { color:rgba(255,255,255,.78) !important; font-weight:500; }
  .navbar-nav .nav-link .bi { margin-right:.35rem; vertical-align:-.1em; }
  .navbar-nav .nav-link:hover { color:#fff !important; }
  .navbar-nav .nav-link.active { color:#fff !important;
                                 border-bottom:3px solid #e16a0a; }

  /* ---- Page containers --------------------------------------------------- */
  .aes-page  { max-width: 760px; margin: 0 auto; padding: 2.5rem 1.25rem 4rem; }
  .aes-wide  { max-width: 1400px; margin: 0 auto; padding: 2rem 1.25rem 4rem; }
  .aes-hero  { text-align:center; margin-bottom:2.5rem; }
  .aes-hero img { width:170px; margin-bottom:1rem; }
  .aes-hero h1 { font-size:3rem; line-height:1; margin:0 0 .5rem;
                 color:#165a72; }
  .aes-hero h1 span { color:#e16a0a; }
  .aes-hero .lead { font-size:1.2rem; color:#4a5a63; max-width:560px; margin:0 auto; }

  .aes-home { display:inline-flex; align-items:center; gap:.4rem; margin-bottom:1rem;
              font-size:.95rem; font-weight:500; color:#1d7898; text-decoration:none;
              padding:.3rem .8rem; border:1px solid #dbe3e8; border-radius:999px; background:#fff; }
  .aes-home:hover { color:#e16a0a; border-color:#e16a0a; text-decoration:none; }
  h2 { color:#165a72; font-size:1.9rem; margin-top:0; }
  h3 { color:#e16a0a; font-size:1.45rem; margin:2rem 0 .75rem; }
  p, li { font-size:1.05rem; line-height:1.6; }
  a { color:#1d7898; }

  .aes-actions { text-align:center; margin:2rem 0 2.5rem; }
  .btn-primary { font-weight:600; padding:.6rem 1.4rem; }
  .btn-outline-secondary { font-weight:500; padding:.6rem 1.4rem; margin-left:.5rem; }

  /* ---- Stats strip ------------------------------------------------------- */
  .aes-stats { display:flex; justify-content:center; gap:2.5rem; flex-wrap:wrap;
               margin:0 0 2.25rem; padding:1.1rem 1.5rem; background:#fff;
               border:1px solid #e3e8ec; border-radius:6px; }
  .aes-stat  { text-align:center; min-width:120px; }
  .aes-stat .num { display:block; font-family:"Barlow Condensed"; font-weight:700;
                   font-size:2.1rem; line-height:1; color:#e16a0a; }
  .aes-stat .lbl { display:block; font-size:.9rem; color:#4a5a63; margin-top:.3rem; }

  /* ---- Decade chips -------------------------------------------------------- */
  .aes-chips { display:flex; gap:.4rem; flex-wrap:wrap; margin:0 0 .9rem; align-items:center; }
  .aes-chips .lbl { font-size:.85rem; color:#7a8a93; margin-right:.3rem; }
  .aes-chip { border:1px solid #dbe3e8; background:#fff; color:#165a72; border-radius:999px;
              padding:.25rem .8rem; font-size:.85rem; font-weight:600; cursor:pointer;
              font-family:"Barlow"; }
  .aes-chip:hover { border-color:#e16a0a; color:#e16a0a; }
  .aes-chip.active { background:#165a72; border-color:#165a72; color:#fff; }

  /* ---- Trends -------------------------------------------------------------- */
  .aes-trend { margin-bottom:2rem; }
  .aes-trend h3 { margin-top:0; }
  .aes-trend p  { color:#4a5a63; font-size:.98rem; }

  /* ---- Methodology layout ------------------------------------------------- */
  .aes-page-m { max-width: 960px; }
  .aes-note-top { margin-top:0; margin-bottom:.5rem; }
  .aes-recipe { width:100%; margin-top:1.25rem; border-collapse:collapse; font-size:.95rem; }
  .aes-recipe th, .aes-recipe td { padding:.5rem .7rem; border-top:1px solid #e3e8ec; text-align:left; }
  .aes-recipe thead th { border-top:none; color:#165a72; font-family:"Barlow Condensed"; font-size:1.05rem;
                         letter-spacing:.02em; }
  .aes-recipe tbody th { color:#5f6f78; font-weight:600; width:26%; }
  .aes-notes3 { display:grid; grid-template-columns:repeat(3, 1fr); gap:1rem; }
  .aes-notes3 .aes-note { margin-top:0; }
  .aes-notes3 p, .aes-recipe td, .aes-recipe th { font-size:.95rem; line-height:1.5; }
  .aes-note-grey { border-left-color:#7a8a93; background:#f1f4f6; }
  @media (max-width: 720px) { .aes-notes3 { grid-template-columns:1fr; } }

  /* ---- Cards / callouts -------------------------------------------------- */
  .aes-card { background:#fff; border:1px solid #e3e8ec; border-radius:6px;
              padding:1.5rem 1.75rem; }
  .aes-steps { padding-left:1.4rem; }
  .aes-steps li { margin-bottom:.9rem; }
  .aes-steps li::marker { color:#e16a0a; font-weight:700; }
  .aes-note  { border-left:4px solid #1d7898; background:#eaf4f8;
               padding:1rem 1.25rem; margin-top:1.5rem; border-radius:0 6px 6px 0; }
  .aes-note p:last-child { margin-bottom:0; }

  /* ---- Database sub-tabs ------------------------------------------------- */
  .aes-wide .nav-tabs { border-bottom:2px solid #dbe3e8; margin-bottom:1.25rem; }
  .aes-wide .nav-tabs .nav-link { color:#4a5a63; font-weight:500; border:none; }
  /* Players tab stands out from the three leaderboards */
  .aes-wide .nav-tabs .nav-item:last-child { margin-left:auto; }
  .aes-wide .nav-tabs .nav-item:last-child .nav-link { color:#fff; background:#e16a0a; border-radius:999px;
      padding:.35rem 1rem; margin-bottom:.35rem; font-weight:600; }
  .aes-wide .nav-tabs .nav-item:last-child .nav-link .bi { margin-right:.35rem; vertical-align:-.15em; }
  .aes-wide .nav-tabs .nav-item:last-child .nav-link:hover { background:#c95d06; color:#fff; }
  .aes-wide .nav-tabs .nav-item:last-child .nav-link.active { background:#165a72; color:#fff; border-bottom:none; }
  .aes-wide .nav-tabs .nav-link.active { color:#e16a0a; background:transparent;
                                          border-bottom:3px solid #e16a0a; }
  .aes-empty { text-align:center; color:#7a8a93; padding:4rem 1rem; }
  .aes-empty h3 { color:#7a8a93; }

  .aes-footer { text-align:center; color:#7a8a93; font-size:.9rem;
                padding:2rem 0 1rem; border-top:1px solid #e3e8ec; margin-top:3rem; }

  /* ---- Player view ------------------------------------------------------------ */
  .aes-pick { display:flex; gap:1rem; flex-wrap:wrap; align-items:flex-end; margin:0 0 1.25rem; }
  .aes-pick .form-group { margin-bottom:0; min-width:260px; }
  .aes-pick label { font-size:.85rem; color:#5f6f78; font-weight:600; }
  .aes-pcards { display:grid; grid-template-columns:repeat(auto-fit, minmax(300px, 1fr)); gap:1rem; margin-bottom:1.25rem; }
  .aes-pcard { background:#fff; border:1px solid #e3e8ec; border-radius:6px; padding:1.1rem 1.3rem; border-top:4px solid #e16a0a; }
  .aes-pcard.b { border-top-color:#1d7898; }
  .aes-pcard h3 { margin:0 0 .15rem; font-size:1.5rem; color:#165a72; }
  .aes-pcard .meta { color:#5f6f78; font-size:.9rem; margin-bottom:.8rem; }
  .aes-pcard .grid { display:grid; grid-template-columns:repeat(3, 1fr); gap:.6rem; }
  .aes-pcard .cell { background:#f6f8fa; border-radius:4px; padding:.5rem .6rem; }
  .aes-pcard .cell .k { display:block; font-size:.72rem; color:#5f6f78; text-transform:uppercase; letter-spacing:.04em; }
  .aes-pcard .cell .v { display:block; font-family:"Barlow Condensed"; font-weight:700; font-size:1.35rem; color:#165a72; line-height:1.1; }
  .aes-pcard .cell .s { display:block; font-size:.78rem; color:#5f6f78; }
  .aes-pchart { background:#fff; border:1px solid #e3e8ec; border-radius:6px; padding:1rem 1rem .5rem; margin-bottom:1.25rem; }

  /* ---- Mobile ---------------------------------------------------------------- */
  @media (max-width: 720px) {
    .aes-mobile-hide { display:none !important; }
    .aes-table .rt-search { width:100%; }
    .aes-hero h1 { font-size:2.1rem; }
    .aes-hero img { width:120px; }
    .aes-stats { gap:1.2rem; padding:.9rem 1rem; }
    .aes-stat { min-width:90px; }
    .aes-stat .num { font-size:1.6rem; }
    .aes-page, .aes-wide { padding:1.5rem .9rem 3rem; }
    .aes-card { padding:1.1rem 1.1rem; }
    .btn-outline-secondary { margin-left:0; margin-top:.5rem; }
    .aes-chips { gap:.3rem; }
    .aes-chip { padding:.2rem .6rem; font-size:.8rem; }
  }

  /* ---- Reactable ---------------------------------------------------------- */
  .aes-table .rt-thead { position:sticky; top:0; z-index:2; }
  .aes-table .rt-th-group { background:#0f4457 !important; font-size:12px;
                            letter-spacing:.05em; text-transform:uppercase; }
  .aes-table .rt-search { width:260px; margin-bottom:.75rem; }
  .mvp-badge { display:inline-block; margin-left:.45rem; padding:.05rem .4rem;
               border-radius:3px; background:#c95d06; color:#fff;
               font-family:"Barlow Condensed"; font-size:11px; font-weight:700;
               letter-spacing:.06em; vertical-align:1px; }
')

# Loading spinner in brand colours
aes_spin = function(ui) {
  shinycssloaders::withSpinner(ui, type = 6, color = "#e16a0a", color.background = "#f6f8fa", size = 0.8)
}

# "Back to home" pill used on inner pages
aes_home_link = function(id) {
  actionLink(id, class = "aes-home", label = tagList(HTML("&larr;"), "Home"))
}

# Headline numbers strip
aes_stats_strip = function(st) {
  stat = function(num, lbl) div(class = "aes-stat", span(class = "num", num), span(class = "lbl", lbl))
  div(class = "aes-stats",
      stat(st$seasons, glue("seasons")),
      stat(format(st$rows, big.mark = ","), "qualifying player-seasons"),
      stat(format(st$players, big.mark = ","), "players"))
}

# Decade quick filters: set the Season column filter to a decade prefix ("197" = 1970s)
aes_decade_chips = function(table_id) {
  decades = c("All" = "", "70s" = "197", "80s" = "198", "90s" = "199",
              "00s" = "200", "10s" = "201", "20s" = "202")
  div(class = "aes-chips", id = paste0(table_id, "_chips"), `data-era` = "",
      span(class = "lbl", "Era:"),
      lapply(names(decades), function(nm) {
        tags$button(
          class = if (nm == "All") "aes-chip active" else "aes-chip",
          onclick = sprintf(
            "var era = '%s';
             document.getElementById('%s_chips').dataset.era = era;
             Reactable.setFilter('%s', 'SEASON', era);
             var sel = document.getElementById('%s_season');
             if (sel) {
               sel.value = '';
               Array.from(sel.options).forEach(o => o.hidden = (o.value !== '' && !o.value.startsWith(era)));
             }
             document.querySelectorAll('#%s_chips .aes-chip').forEach(b => b.classList.remove('active'));
             this.classList.add('active');",
            decades[[nm]], table_id, table_id, table_id, table_id),
          nm)
      }))
}

ui = bslib::page_navbar(
  id      = "nav",
  theme   = aes_theme,
  window_title = "Adjusted Era Stats",
  title   = tags$span(
    img(src = "https://i.ibb.co/3yrBSHjh/aes-logo-no-bg.png", alt = ""),
    "Adjusted Era Stats"
  ),
  header  = tags$head(
    tags$style(aes_css),
    tags$link(rel = "shortcut icon", href = "https://i.ibb.co/jZDhS88z/aes-logo.png")
  ),
  
  # ---- Home -----------------------------------------------------------------
  bslib::nav_panel("Home", icon = bsicons::bs_icon("house-door"),
                   div(class = "aes-page",
                       div(class = "aes-hero",
                           img(src = "https://i.ibb.co/3yrBSHjh/aes-logo-no-bg.png", alt = "Adjusted Era Stats logo"),
                           h1("Points, rebounds and assists, ", tags$span("adjusted for every era.")),
                           p(class = "lead",
                             "Era-adjusted metrics for the NBA's best single seasons. Scoring, rebounding
                              and passing measured against the top of the league, not the box score.")
                       ),
                       aes_stats_strip(aesStats),
                       div(class = "aes-actions",
                           actionButton("go_db", "Explore the leaderboard", class = "btn-primary"),
                           actionButton("go_method", "How it works", class = "btn-outline-secondary")
                       ),
                       div(class = "aes-card",
                           p("Per-game averages don't travel well across decades. A 30-point season in 1985
                              was produced at a different pace, against different spacing and under different
                              rules than one in 2025. Adjusted Era Stats replaces raw volume with a relative
                              measure: how far a player's per-possession production stood above the top-25 of
                              his own season, blended with his efficiency relative to the league that year."),
                           p("The result is a single score per player-season that can be compared across
                              eras, filtered by position, age or team, and used to check narratives against the
                              numbers. It is built on public data, the method is fully documented, and the
                              complete leaderboard is open to explore."),
                           p("A project by ",
                             a(href = "https://twitter.com/taylormetrics", "Taylor Wyman", target = "_blank"),
                             " and ",
                             a(href = "https://filippospol.github.io/bball-projects/", "Filippos Polyzos", target = "_blank"),
                             ". Data from ",
                             a(href = "https://basketball-reference.com/", "Basketball Reference", target = "_blank"),
                             ", 1973-74 to the current season.")
                       ),
                       div(class = "aes-footer", "App built by Filippos Polyzos.")
                   )
  ),
  
  # ---- Methodology ----------------------------------------------------------
  bslib::nav_panel("Methodology", icon = bsicons::bs_icon("book"),
                   div(class = "aes-page aes-page-m",
                       aes_home_link("go_home_m"),
                       h2("Methodology"),
                       p("We gathered per possession, rebounding, passing and adjusted shooting data from Basketball Reference,
                          from 1973-74 until the latest regular season. All three leaderboards use the same recipe; only the
                          inputs change."),
                       
                       div(class = "aes-note aes-note-top",
                           p(strong("How to read the score."), " AVS, AVR and AVA are standardized values, so they share one scale:
                              0 is the average qualified season in the whole database, 1.0 is one standard deviation above it,
                              and 2.0 or more is reserved for a few dozen seasons in NBA history. Standardizing against all seasons
                              at once (rather than within each season) means a player is rewarded for towering over a deep field
                              of peers, not only for being the best of a thin one.")),
                       
                       h3("The recipe"),
                       div(class = "aes-card",
                           tags$ol(class = "aes-steps",
                                   tags$li("Select all players that played a total of 1500 minutes and 25 minutes/game in each season. ",
                                           em("(Different filters applied to ongoing seasons.)")),
                                   tags$li("Sort player list by volume (per 75 possessions) and select the top 25 for each season."),
                                   tags$li("Calculate the average volume of those 25 players in each season."),
                                   tags$li("Subtract the top-25 average from every qualified players volume for each season.
                                            This is called Above/Below."),
                                   tags$li("Standardize Above/Below and the efficiency metric* against every qualified season since 1973-74
                                            and combine them into a single score."),
                                   tags$li("Do that for every season and that's how you get the Adjusted Volume score!")
                           ),
                           tags$table(class = "aes-recipe",
                                      tags$thead(tags$tr(tags$th(""), tags$th("Scoring"), tags$th("Rebounding"), tags$th("Passing"))),
                                      tags$tbody(
                                        tags$tr(tags$th("Volume"),       tags$td("Points/75"),          tags$td("Rebounds/75"),          tags$td("Assists/75")),
                                        tags$tr(tags$th("Above/Below"),  tags$td("Points Above/Below"), tags$td("Rebounds Above/Below"), tags$td("Assists Above/Below")),
                                        tags$tr(tags$th("Efficiency*"),  tags$td("TS+"),                tags$td("TRB+"),                 tags$td("AST+")),
                                        tags$tr(tags$th("Result"),       tags$td(strong("AVS")),        tags$td(strong("AVR")),          tags$td(strong("AVA")))
                                      ))
                       ),
                       
                       h3("* The efficiency metrics"),
                       div(class = "aes-notes3",
                           div(class = "aes-note",
                               p(strong("TS+"), br(), "An efficiency metric that allows cross-era comparison. We simply divide a player's TS%
                                  by the league average TS% for any given season. 100 is league average efficiency; 105 means you shot
                                  5% better than the league that season.")),
                           div(class = "aes-note",
                               p(strong("TRB+"), br(), "A player's total rebound percentage (the share of available rebounds he grabbed while
                                  on the court, which already accounts for how many shots were missed in that era) divided by the
                                  average of all qualified players that season. 130 means 30% more of the available rebounds than
                                  the typical qualified player.")),
                           div(class = "aes-note",
                               p(strong("AST+"), br(), "A player's assist percentage (the share of teammates' field goals he assisted while on
                                  the court, which accounts for how well his teammates shot) divided by the average of all qualified
                                  players that season. 200 means twice the share of baskets that the typical qualified player created."))
                       ),
                       
                       div(class = "aes-note aes-note-grey",
                           p(strong("What we don't adjust for."), " League quality: the method compares a player with his own
                              season's peers, so it says nothing about whether the talent pool of 1978 and 2024 were equally deep.
                              Scorekeeping: assists in particular were recorded more or less generously in different arenas and
                              decades, and no box-score method can correct for that."))
                   )
  ),
  
  # ---- Database -------------------------------------------------------------
  bslib::nav_panel("Database", icon = bsicons::bs_icon("table"),
                   div(class = "aes-wide",
                       aes_home_link("go_home_d"),
                       tabsetPanel(id = "db_tabs",
                                   tabPanel("Scoring",    br(), aes_decade_chips("avs"), aes_spin(reactableOutput("avs", width = "100%"))),
                                   tabPanel("Rebounding", br(), aes_decade_chips("avr"), aes_spin(reactableOutput("avr", width = "100%"))),
                                   tabPanel("Passing",    br(), aes_decade_chips("ava"), aes_spin(reactableOutput("ava", width = "100%"))),
                                   tabPanel(title = tagList(bsicons::bs_icon("person-lines-fill"), "Players"),
                                            value = "Players", br(),
                                            div(class = "aes-pick",
                                                selectizeInput("player_a", "Player", choices = NULL, width = "100%"),
                                                selectizeInput("player_b", "Compare with (optional)", choices = NULL, width = "100%",
                                                               options = list(placeholder = "Pick a second player", allowEmptyOption = TRUE))),
                                            uiOutput("player_cards"),
                                            uiOutput("player_middle"),
                                            uiOutput("player_bottom"))
                       )
                   )
  ),
  
  # ---- Trends ---------------------------------------------------------------
  bslib::nav_panel("Trends", icon = bsicons::bs_icon("graph-up"),
                   div(class = "aes-page",
                       aes_home_link("go_home_t"),
                       h2("How the league has changed"),
                       p("Each chart shows the top-25 average for that season, the benchmark every qualified player is
                          measured against in the Above/Below step. Where the line is high, the league's best were
                          producing a lot; where it dips, standing out took less raw volume."),
                       div(class = "aes-trend aes-card",
                           h3("Scoring"),
                           p("Average points per 75 possessions of the season's top-25 scorers."),
                           aes_spin(plotlyOutput("trend_pts", height = "320px"))),
                       div(class = "aes-trend aes-card",
                           h3("Rebounding"),
                           p("Average rebounds per 75 possessions of the season's top-25 rebounders."),
                           aes_spin(plotlyOutput("trend_reb", height = "320px"))),
                       div(class = "aes-trend aes-card",
                           h3("Passing"),
                           p("Average assists per 75 possessions of the season's top-25 passers."),
                           aes_spin(plotlyOutput("trend_ast", height = "320px")))
                   )
  ),
  
)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Server:

# Helper: signed number in brand colours (blue above centre, muted brick below, grey at centre)
aes_signed_style = function(value, centre = 0) {
  col = if (is.na(value)) "#5f6f78" else if (value > centre) "#1d7898" else if (value < centre) "#b5533c" else "#5f6f78"
  list(color = col, fontWeight = 600)
}

# Helper: colour scale for the score column (light grey-blue to brand blue)
aes_score_scale = function(values) {
  pal = colorRampPalette(c("#f6f8fa", "#c9e1ec", "#6fb0cc", "#165a72"))(100)
  rng = range(values, na.rm = TRUE)
  function(value) {
    if (is.na(value)) return(list())
    i   = 1 + round((value - rng[1]) / diff(rng) * 99)
    bg  = pal[i]
    list(background = bg, fontWeight = 700,
         color = if (i > 55) "#ffffff" else "#165a72")
  }
}

# Helper: dropdown column filter
aes_select_filter = function(table_id, decreasing = FALSE, season = FALSE) {
  function(values, name) {
    # For the Season column: "All" falls back to the era selected with the chips (stored on the chip row)
    onchange = if (season) sprintf(
      "var era = document.getElementById('%s_chips').dataset.era || '';
       Reactable.setFilter('%s', '%s', event.target.value || era)", table_id, table_id, name)
    else sprintf("Reactable.setFilter('%s', '%s', event.target.value)", table_id, name)
    tags$select(
      id = if (season) paste0(table_id, "_season") else NULL,
      onchange = onchange,
      tags$option(value = "", "All"),
      lapply(sort(unique(values), decreasing = decreasing), tags$option),
      style = "width:100%; font-size:12px;"
    )
  }
}

server = function(input, output, session) {
  
  observeEvent(input$go_db,     bslib::nav_select("nav", "Database"))
  observeEvent(input$go_method, bslib::nav_select("nav", "Methodology"))
  observeEvent(input$go_home_m, bslib::nav_select("nav", "Home"))
  observeEvent(input$go_home_d, bslib::nav_select("nav", "Home"))
  observeEvent(input$go_home_t, bslib::nav_select("nav", "Home"))
  
  # Trends
  output$trend_pts = renderPlotly(aes_trend_plot("PTS", "Pts/75"))
  output$trend_reb = renderPlotly(aes_trend_plot("REB", "Reb/75"))
  output$trend_ast = renderPlotly(aes_trend_plot("AST", "Ast/75"))
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # One table builder for all three leaderboards ----------------------------------------------
  av_table = function(id, spec) {
    d = spec$data
    # width that fits the longest name plus the MVP badge (approx. 7.5px per character at 13px Barlow)
    player_w = ceiling(max(nchar(d$PLAYER)) * 7.5 + 70)
    reactable(
      d,
      class = "aes-table",
      defaultSorted = "SCORE", defaultSortOrder = "desc",
      defaultPageSize = 25, showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100),
      searchable = TRUE, striped = TRUE, highlight = TRUE,
      wrap = FALSE, showSortable = TRUE,
      language = reactableLang(searchPlaceholder = "Search player, team or season..."),
      theme = reactableTheme(
        headerStyle      = list(background = "#165a72", color = "#fff", fontWeight = 600,
                                fontFamily = "Barlow Condensed", fontSize = 13,
                                letterSpacing = ".02em"),
        groupHeaderStyle = list(color = "#fff"),
        stripedColor     = "#f2f5f7", highlightColor = "#fdf0e6",
        borderColor      = "#e3e8ec", cellPadding = "7px 10px",
        style            = list(fontFamily = "Barlow", fontSize = 13)
      ),
      defaultColDef = colDef(align = "right", vAlign = "center", headerVAlign = "bottom"),
      
      columnGroups = list(
        colGroup(name = "Player",       columns = c("SEASON", "TEAM", "POS", "AGE", "MIN")),
        colGroup(name = spec$full,      columns = c("VOL", "TOP_AVG", "ABOVE_BELOW", "RATE")),
        colGroup(name = "Result",       columns = c("SCORE", "PCT"))
      ),
      
      columns = list(
        Rk     = colDef(name = "#", width = 56, sticky = "left", sortable = FALSE,
                        style = list(color = "#5f6f78", fontWeight = 600)),
        PLAYER = colDef(name = "Player", align = "left", width = player_w, sticky = "left",
                        filterable = TRUE,
                        cell = function(value, index) {
                          if (isTRUE(d$MVP[index])) tagList(value, span(class = "mvp-badge", "MVP")) else value
                        },
                        style = list(fontWeight = 600, borderRight = "1px solid #dbe3e8")),
        MVP    = colDef(show = FALSE),
        SEASON = colDef(name = "Season", align = "left", width = 90, filterable = TRUE,
                        filterInput = aes_select_filter(id, decreasing = TRUE, season = TRUE),
                        # prefix match so a decade chip ("197") and an exact season ("1975-76") both work
                        filterMethod = JS("function(rows, columnId, filterValue) {
                          return rows.filter(r => String(r.values[columnId]).startsWith(filterValue))
                        }")),
        TEAM   = colDef(name = "Team", align = "left", minWidth = 110, filterable = TRUE,
                        class = "aes-mobile-hide", headerClass = "aes-mobile-hide"),
        POS    = colDef(name = "Pos", align = "left", width = 60, filterable = TRUE,
                        filterInput = aes_select_filter(id)),
        AGE    = colDef(name = "Age", width = 56, class = "aes-mobile-hide", headerClass = "aes-mobile-hide"),
        MIN    = colDef(name = "Min", width = 70, format = colFormat(separators = TRUE),
                        class = "aes-mobile-hide", headerClass = "aes-mobile-hide"),
        
        VOL         = colDef(name = spec$vol, width = 76, format = colFormat(digits = 1),
                             header = function(value) span(title = "Per 75 possessions (pace-adjusted)", value)),
        TOP_AVG     = colDef(name = "Top-25 Avg", width = 92, format = colFormat(digits = 1),
                             class = "aes-mobile-hide", headerClass = "aes-mobile-hide",
                             header = function(value) span(title = "Average of the season's 25 highest-volume qualified players", value),
                             style = list(color = "#7a8a93")),
        ABOVE_BELOW = colDef(name = "Above/Below", width = 104,
                             header = function(value) span(title = "Player's volume minus the season's top-25 average", value),
                             cell = function(value) sprintf("%+.1f", value),
                             style = function(value) aes_signed_style(value, 0)),
        RATE        = colDef(name = spec$rate, width = 64,
                             header = function(value) span(title = "Efficiency relative to that season: 100 = average, 110 = 10% better", value),
                             style = function(value) aes_signed_style(value, 100)),
        
        SCORE  = colDef(name = spec$score, width = 80, format = colFormat(digits = 2),
                        header = function(value) span(title = glue("{spec$full}. 0 = average qualified season, 1 = one standard deviation above, 2+ = all-time elite"), value),
                        style = aes_score_scale(d$SCORE),
                        headerStyle = list(background = "#0f4457")),
        PCT    = colDef(name = "%tile", width = 64, format = colFormat(digits = 0),
                        header = function(value) span(title = "Percentile among all qualified seasons in this table", value))
      )
    )
  }
  
  output$avs = renderReactable(av_table("avs", AV$scoring))
  output$avr = renderReactable(av_table("avr", AV$rebounding))
  output$ava = renderReactable(av_table("ava", AV$passing))
  
  # ---- Player view -----------------------------------------------------------------------------
  updateSelectizeInput(session, "player_a", choices = playerChoices, selected = defaultPlayer, server = TRUE,
                       options = list(render = aes_player_render, searchField = "label"))
  updateSelectizeInput(session, "player_b", choices = playerChoices, selected = "", server = TRUE,
                       options = list(render = aes_player_render, searchField = "label",
                                      placeholder = "Pick a second player", allowEmptyOption = TRUE))
  
  sel_players = reactive({
    a = input$player_a; b = input$player_b
    req(!is.null(a), a != "")
    p = c(a, if (!is.null(b) && b != "" && b != a) b)
    p
  })
  
  player_card = function(name, cls) {
    st = playerSummary %>% filter(PLAYER == name)
    if (nrow(st) == 0) return(NULL)
    fmt = function(x) if (is.na(x)) "-" else sprintf("%.2f", x)
    cell = function(k, v, s) div(class = "cell", span(class = "k", k), span(class = "v", v), span(class = "s", s))
    div(class = paste("aes-pcard", cls),
        h3(name),
        div(class = "meta", glue("{st$first} to {st$last} | {st$seasons} qualifying season{ifelse(st$seasons == 1, '', 's')} in the database",
                                 "{ifelse(st$mvps > 0, paste0(' | ', st$mvps, ' time MVP'), '')}")),
        div(class = "grid",
            cell("Scoring",    glue("{st$top_avs} of {st$seasons}"), glue("top-25 seasons | best AVS {fmt(st$best_avs)}")),
            cell("Rebounding", glue("{st$top_avr} of {st$seasons}"), glue("top-25 seasons | best AVR {fmt(st$best_avr)}")),
            cell("Passing",    glue("{st$top_ava} of {st$seasons}"), glue("top-25 seasons | best AVA {fmt(st$best_ava)}"))))
  }
  
  output$player_cards = renderUI({
    p = sel_players()
    div(class = "aes-pcards", player_card(p[1], "a"), if (length(p) > 1) player_card(p[2], "b"))
  })
  
  # Middle block: career chart for one player, best-season comparison for two
  output$player_middle = renderUI({
    p = sel_players()
    if (length(p) == 1) div(class = "aes-pchart", aes_spin(plotlyOutput("player_chart", height = "340px")))
    else div(class = "aes-pchart", aes_spin(reactableOutput("player_compare", width = "100%")))
  })
  
  output$player_chart = renderPlotly({
    p = sel_players(); req(length(p) == 1)
    d = playerSeasons %>% filter(PLAYER == p) %>%
      select(SEASON, AVS, AVR, AVA) %>%
      pivot_longer(c(AVS, AVR, AVA), names_to = "metric", values_to = "value") %>%
      filter(!is.na(value))
    cols = c(AVS = "#e16a0a", AVR = "#165a72", AVA = "#2596be")
    fig = plot_ly()
    for (m in c("AVS", "AVR", "AVA")) {
      dd = d %>% filter(metric == m)
      if (nrow(dd) == 0) next
      fig = fig %>% add_trace(data = dd, x = ~SEASON, y = ~value, type = "scatter", mode = "lines+markers", name = m,
                              line = list(color = cols[[m]], width = 2.5), marker = list(color = cols[[m]], size = 7),
                              text = ~paste0(SEASON, ": ", m, " ", sprintf("%.2f", value)), hoverinfo = "text")
    }
    fig %>% layout(
      xaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = sort(unique(d$SEASON)),
                   tickangle = -45, showgrid = FALSE, tickfont = list(color = "#4a5a63")),
      yaxis = list(title = "Score", gridcolor = "#e3e8ec", zeroline = TRUE, zerolinecolor = "#c9d3d9",
                   tickfont = list(color = "#4a5a63")),
      legend = list(orientation = "h", x = 0, y = 1.12),
      margin = list(t = 30, r = 10, b = 60),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Barlow", size = 13),
      hoverlabel = list(bgcolor = "#165a72", font = list(color = "#fff", family = "Barlow"))) %>%
      config(displayModeBar = FALSE)
  })
  
  # Best-season comparison (Basketball Reference style): each player's best season per stat, side by side
  best_rows = function(name) {
    d = playerSeasons %>% filter(PLAYER == name)
    one = function(score, vol_lbl, rate_lbl, grp) {
      sc = d[[score]]
      if (all(is.na(sc))) return(tibble(group = grp, metric = c("Best season", vol_lbl, rate_lbl, score, "%tile"),
                                        value = NA_character_, num = NA_real_))
      b = d[which.max(sc), ]
      tibble(group  = grp,
             metric = c("Best season", vol_lbl, rate_lbl, score, "%tile"),
             value  = c(paste0(b$SEASON, " (", b$TEAM, ", age ", b$AGE, ")"),
                        sprintf("%.1f", b[[paste0(score, "_VOL")]]), sprintf("%.0f", b[[paste0(score, "_RATE")]]),
                        sprintf("%.2f", b[[score]]), sprintf("%.0f", b[[paste0(score, "_PCT")]])),
             num    = c(NA, b[[paste0(score, "_VOL")]], b[[paste0(score, "_RATE")]], b[[score]], b[[paste0(score, "_PCT")]]))
    }
    bind_rows(
      one("AVS", "Pts/75", "TS+",  "Scoring"),
      one("AVR", "Reb/75", "TRB+", "Rebounding"),
      one("AVA", "Ast/75", "AST+", "Passing"))
  }
  
  output$player_compare = renderReactable({
    p = sel_players(); req(length(p) == 2)
    a = best_rows(p[1]); b = best_rows(p[2])
    d = a %>% select(group, metric, A = value, A_num = num) %>%
      bind_cols(b %>% select(B = value, B_num = num))
    win_style = function(side) function(value, index) {
      x = d$A_num[index]; y = d$B_num[index]
      if (is.na(x) || is.na(y) || x == y) return(list())
      better = if (side == "A") x > y else y > x
      if (better) list(fontWeight = 700, color = "#165a72", background = "#eaf4f8") else list(color = "#5f6f78")
    }
    reactable(
      d %>% select(group, metric, A, B), class = "aes-table",
      groupBy = "group", defaultExpanded = TRUE, pagination = FALSE, sortable = FALSE, highlight = TRUE,
      theme = reactableTheme(
        headerStyle = list(background = "#165a72", color = "#fff", fontWeight = 600, fontFamily = "Barlow Condensed", fontSize = 13),
        borderColor = "#e3e8ec", cellPadding = "7px 10px", style = list(fontFamily = "Barlow", fontSize = 13),
        highlightColor = "#fdf0e6"),
      columns = list(
        group  = colDef(name = "", width = 140, style = list(fontWeight = 700, color = "#e16a0a", fontFamily = "Barlow Condensed", fontSize = 15),
                        grouped = JS("function(cellInfo) { return cellInfo.value }")),
        metric = colDef(name = "", align = "left", minWidth = 180, style = list(color = "#5f6f78")),
        A = colDef(name = p[1], align = "right", minWidth = 170, na = "-", style = win_style("A")),
        B = colDef(name = p[2], align = "right", minWidth = 170, na = "-", style = win_style("B"))))
  })
  
  # Season-by-season table only in single-player mode
  output$player_bottom = renderUI({
    if (length(sel_players()) == 1) aes_spin(reactableOutput("player_table", width = "100%")) else NULL
  })
  
  output$player_table = renderReactable({
    p = sel_players(); req(length(p) == 1)
    d = playerSeasons %>% filter(PLAYER %in% p) %>%
      select(PLAYER, SEASON, TEAM, POS, AGE, MIN, MVP,
             AVS_VOL, AVS_RATE, AVS, AVS_PCT, AVR_VOL, AVR_RATE, AVR, AVR_PCT, AVA_VOL, AVA_RATE, AVA, AVA_PCT)
    num  = function(name, digits = 1, w = 64) colDef(name = name, width = w, format = colFormat(digits = digits), na = "-")
    rk   = function(name) colDef(name = name, width = 60, na = "-", format = colFormat(digits = 0),
                                 style = list(color = "#5f6f78"),
                                 header = function(value) span(title = "Percentile among all qualified seasons in that leaderboard", value))
    scr  = function(name) colDef(name = name, width = 70, format = colFormat(digits = 2), na = "-",
                                 style = list(fontWeight = 700, color = "#165a72", background = "#eaf4f8"))
    reactable(
      d, class = "aes-table", defaultPageSize = 30, pagination = nrow(d) > 30, striped = TRUE, highlight = TRUE,
      wrap = FALSE, showSortable = TRUE, defaultSorted = list(PLAYER = "asc", SEASON = "asc"),
      theme = reactableTheme(
        headerStyle = list(background = "#165a72", color = "#fff", fontWeight = 600, fontFamily = "Barlow Condensed", fontSize = 13),
        groupHeaderStyle = list(color = "#fff"), stripedColor = "#f2f5f7", highlightColor = "#fdf0e6",
        borderColor = "#e3e8ec", cellPadding = "7px 10px", style = list(fontFamily = "Barlow", fontSize = 13)),
      defaultColDef = colDef(align = "right", vAlign = "center", headerVAlign = "bottom"),
      columnGroups = list(
        colGroup(name = "Scoring",    columns = c("AVS_VOL", "AVS_RATE", "AVS", "AVS_PCT")),
        colGroup(name = "Rebounding", columns = c("AVR_VOL", "AVR_RATE", "AVR", "AVR_PCT")),
        colGroup(name = "Passing",    columns = c("AVA_VOL", "AVA_RATE", "AVA", "AVA_PCT"))),
      columns = list(
        PLAYER = colDef(name = "Player", align = "left", sticky = "left", show = length(p) > 1, width = 190,
                        style = list(fontWeight = 600)),
        SEASON = colDef(name = "Season", align = "left", sticky = "left", width = 90,
                        cell = function(value, index) if (isTRUE(d$MVP[index])) tagList(value, span(class = "mvp-badge", "MVP")) else value,
                        style = list(fontWeight = 600, borderRight = "1px solid #dbe3e8")),
        TEAM = colDef(name = "Team", align = "left", width = 90), POS = colDef(name = "Pos", align = "left", width = 56),
        AGE  = colDef(name = "Age", width = 56), MIN = colDef(name = "Min", width = 70, format = colFormat(separators = TRUE)),
        MVP  = colDef(show = FALSE),
        AVS_VOL = num("Pts/75"), AVS_RATE = num("TS+", 0, 56),  AVS = scr("AVS"), AVS_PCT = rk("%tile"),
        AVR_VOL = num("Reb/75"), AVR_RATE = num("TRB+", 0, 56), AVR = scr("AVR"), AVR_PCT = rk("%tile"),
        AVA_VOL = num("Ast/75"), AVA_RATE = num("AST+", 0, 56), AVA = scr("AVA"), AVA_PCT = rk("%tile")))
  })
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Run the app:
shinyApp(ui = ui, server = server)