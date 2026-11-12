# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script contains the source code for the Modern Playoff Scoring Runs application.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *SETUP ENVIRONMENT*
pacman::p_load(tidyverse, readr, glue,
               shiny, bslib, reactable, shinycssloaders)

Sys.setenv(RETICULATE_PYTHON = "")
options(rsconnect.http.timeout = 120)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' [PART 1: DATA CLEANING]

#' *MPSR WEIGHTS* - tune here, then pick a version in the two select() calls below
W2_VOL = 0.50 ; W2_EFF = 0.50                  # V2: TSA/75 vs TS+
W3_VOL = 0.40 ; W3_EFF = 0.40 ; W3_USG = 0.20  # V3: SI Pts/75, TS+, Usage
#' V4 has no weights at all - see PAA_75 below.

#' *BASE TABLE*
baseTable = read_csv("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/shiny-apps/mpsr-raw.csv") %>%
  suppressMessages() %>%
  mutate_at(c(10:26), as.numeric) %>%
  mutate(# A: calculate the per 75 possessions stats and relative efficiency values:
    MIN_TOT     = round(GP*MIN),
    OPP_REL_DEF = round(OPP_DEF_RATING_ROUND-LA_DEF_RATING,1),
    OPP_REL_OFF = round(OPP_OFF_RATING_ROUND-LA_OFF_RATING,1),
    PTS_GP      = round(PTS/GP,1),
    PTS_75      = round(PTS/POSS*75,1),
    FGA_75      = round(FGA/POSS*75,1),
    FTA_75      = round(FTA/POSS*75,1),
    TSA_75      = round(FGA_75+0.44*FTA_75,1),
    # B: Series adjustment: league ORTG / DRTG of the defense faced
    SI_MULT     = LA_OFF_RATING/OPP_DEF_RATING_ROUND,
    SI_PTS_75   = round(PTS_75*SI_MULT,2),
    # C: Playoff Round:
    PO_ROUND = case_when(
      PO_ROUND==1 ~ "First Round",
      PO_ROUND==2 ~ "Conference Semifinals",
      PO_ROUND==3 ~ "Conference Finals",
      TRUE ~ "NBA Finals"
    )) %>%
  group_by(SEASON,SERIES) %>%
  mutate(# D: TS% for each playoff matchup
    TS_PCT_ROUND = round(sum(PTS,na.rm=T)/
                           (2*(sum(FGA,na.rm=T)+0.44*sum(FTA,na.rm=T))),3)
  ) %>%
  ungroup() %>%
  mutate(# E: Calculate the MPSR candidates:
    TS_PLUS_RS    = round(TS_PCT/TS_PCT_RS*100),
    TS_PLUS_ROUND = round(TS_PCT/TS_PCT_ROUND*100),       # series TS+
    # series uTS+ = the efficiency EDGE over the series, scaled by load.
    # Centred on 100 so average efficiency scores 0 whatever the usage, and
    # divided by 0.20 (the structural average usage of five players on the
    # floor) so the result stays in TS+ units.
    uTS_PLUS      = round((TS_PLUS_ROUND-100)*(USG_PCT/0.20),1),
    # PAA/75 = extra points per 75 possessions versus an average scorer in that
    # same series, taking the same number of shots, against the same defense.
    #   2*TSA_75        -> the points on offer from his shot volume
    #   TS_PCT-TS_PCT_ROUND -> how much better he shot than the series
    #   SI_MULT         -> the defensive adjustment
    # 0 means he was exactly the series average; +5 means five extra points per 75.
    PAA_75        = round(2*TSA_75*(TS_PCT-TS_PCT_ROUND)*SI_MULT,2),
    
    # V1 - current: adjusted volume + usage weighted efficiency edge
    MPSR1 = round(0.50*scale(SI_PTS_75)[,1] + 0.50*scale(uTS_PLUS)[,1],2),
    # V2 - orthogonal split: shot volume vs efficiency. These two correlate -0.04,
    #      so the weights genuinely trade one against the other
    MPSR2 = round(W2_VOL*scale(TSA_75)[,1] + W2_EFF*scale(TS_PLUS_ROUND)[,1],2),
    # V3 - adjusted volume, efficiency and usage. NOTE usage overlaps SI Pts/75 at
    #      0.91, so W3_USG amplifies volume rather than adding a third axis
    MPSR3 = round(W3_VOL*scale(SI_PTS_75)[,1] + W3_EFF*scale(TS_PLUS_ROUND)[,1] +
                    W3_USG*scale(USG_PCT)[,1],2),
    # V4 - no weights, no z-scores: adjusted scoring rate PLUS the points he added
    #      over an average scorer. Both terms are already points per 75, so they
    #      simply add.
    MPSR4 = round(SI_PTS_75 + PAA_75,2)
  ) %>%
  select(SEASON,PLAYER_NAME,POS,AGE,TEAM=TEAM_NAME,OPPONENT,ROUND=PO_ROUND,
         GP,MIN=MIN_TOT,PTS,FGA,FTA,PTS_75,TSA_75,SI_MULT,SI_PTS_75,USG_PCT,TS_PCT,
         TS_PCT_ROUND,TS_PLUS_ROUND,uTS_PLUS,PAA_75,
         Z_SCORE=MPSR1,                       #' *PICK THE VERSION: MPSR1 / 2 / 3 / 4*
         OPP_REL_DEF,POST_PTS_75,POST_TS_PCT) %>%
  mutate(POS=substr(POS,1,2)) %>%
  distinct(PLAYER_NAME,SEASON,ROUND,.keep_all = T)

#' *SINGLE ROUND TABLE*
single_round = baseTable %>%
  filter(MIN>=100 & GP>=4) %>%
  arrange(-Z_SCORE) %>%
  mutate(RANK_DB=percent_rank(Z_SCORE)*100) %>%
  group_by(SEASON) %>%
  arrange(-Z_SCORE) %>%
  distinct(PLAYER_NAME,SEASON,ROUND, .keep_all=T) %>%
  mutate(RANK_SEASON=row_number()) %>%
  ungroup() %>%
  select(RANK_DB,RANK_SEASON,PLAYER_NAME,POS,AGE,SEASON,TEAM,ROUND,OPPONENT,
         GP,MIN,PTS_75,OPP_REL_DEF,SI_PTS_75,TS_PCT,TS_PLUS_ROUND,USG_PCT,
         uTS_PLUS,Z_SCORE)

#' *CANONICAL POSTSEASON AGGREGATE*
#' One row per player-season, built ONCE from the qualified rounds so the Whole
#' Postseason leaderboard and the player card's Postseason total are guaranteed to
#' report identical numbers. Every averaged measure is minute weighted, exactly as
#' the card is, over the same qualified round set the card shows by default
#' (>=100 MIN & >=4 GP per round). A sub-qualification cameo - e.g. a 12 minute
#' injury game - therefore never lands in either total.
po_summary = suppressWarnings(
  baseTable %>%
    filter(MIN>=100 & GP>=4) %>%                     # qualified rounds only
    group_by(PLAYER_NAME,SEASON,TEAM,AGE) %>%
    summarise(ROUNDS=n(),
              # every minute weighted mean MUST come before MIN=sum(MIN) overwrites
              # MIN with a single number
              AVG_SI_MULT   = weighted.mean(SI_MULT,MIN,na.rm=T),
              AVG_TSA_75    = round(weighted.mean(TSA_75,MIN,na.rm=T),2),
              AVG_PAA_75    = round(weighted.mean(PAA_75,MIN,na.rm=T),2),
              AVG_USG       = weighted.mean(USG_PCT,MIN,na.rm=T),
              AVG_TS_PLUS   = round(weighted.mean(TS_PLUS_ROUND,MIN,na.rm=T)),
              # centred, as in baseTable above, and minute weighted throughout
              AVG_uTS_PLUS  = round((weighted.mean(TS_PLUS_ROUND,MIN,na.rm=T)-100)*
                                      (weighted.mean(USG_PCT,MIN,na.rm=T)/0.20),1),
              AVG_REL_DEF   = round(mean(OPP_REL_DEF),1),
              MIN           = sum(MIN),
              GP_ALL        = sum(GP),
              AVG_PTS_75    = first(POST_PTS_75) %>% round(1),
              AVG_SI_PTS_75 = round(first(POST_PTS_75)*AVG_SI_MULT,2),
              AVG_TS_PCT    = first(POST_TS_PCT) %>% round(3),
              .groups="drop")
)

#' *WHOLE POSTSEASON TABLE* - the qualified multi round runs, scored and ranked.
#' Derived straight from po_summary, so it cannot drift from the player card.
po_scored = suppressWarnings(
  po_summary %>%
    filter(MIN>=200 & ROUNDS>1) %>%
    mutate(AVG_Z1 = round(0.50*scale(AVG_SI_PTS_75)[,1] + 0.50*scale(AVG_uTS_PLUS)[,1],2),
           AVG_Z2 = round(W2_VOL*scale(AVG_TSA_75)[,1] + W2_EFF*scale(AVG_TS_PLUS)[,1],2),
           AVG_Z3 = round(W3_VOL*scale(AVG_SI_PTS_75)[,1] + W3_EFF*scale(AVG_TS_PLUS)[,1] +
                            W3_USG*scale(AVG_USG)[,1],2),
           AVG_Z4 = round(AVG_SI_PTS_75 + AVG_PAA_75,2)) %>%
    mutate(AVG_Z = AVG_Z1,                    #' *KEEP THIS IN STEP WITH baseTable*
           DB_PCTILE = percent_rank(AVG_Z)*100) %>%
    distinct(PLAYER_NAME,SEASON, .keep_all=T) %>%
    group_by(SEASON) %>%
    arrange(-AVG_Z) %>%
    mutate(RANK=row_number(), .before=1) %>%
    ungroup()
)

multi_round = po_scored %>%
  select(RANK,DB_PCTILE,PLAYER_NAME,AGE,SEASON,TEAM,ROUNDS,GP=GP_ALL,MIN,
         AVG_PTS_75,AVG_REL_DEF,AVG_SI_PTS_75,AVG_TS_PCT,AVG_TS_PLUS,
         AVG_USG,AVG_uTS_PLUS,AVG_Z) %>%
  drop_na()

#' *PLAYER CARD SEASON TOTALS* - the SAME aggregate the leaderboard uses, so the
#' card's Postseason row equals the Whole Postseason row for that player-season.
#' AVG_Z exists only for leaderboard qualified runs (>=200 MIN & >1 round); it is
#' NA elsewhere and the card prints a dash there.
pcTotals = po_summary %>%
  left_join(po_scored %>% select(PLAYER_NAME,SEASON,AVG_Z),
            by=c("PLAYER_NAME","SEASON")) %>%
  transmute(PLAYER_NAME, SEASON,
            TOT_GP     = GP_ALL,
            TOT_MIN    = MIN,
            TOT_SI     = AVG_SI_PTS_75,
            TOT_TSPLUS = AVG_TS_PLUS,
            TOT_Z      = AVG_Z)

#' *PLAYER CARD TABLE*
#' Every series a player appeared in, qualified or not. Ranks only exist for
#' qualified runs, so they are joined back from single_round and stay NA elsewhere.
playerCard = baseTable %>%
  mutate(QUALIFIED = MIN >= 100 & GP >= 4) %>%
  left_join(single_round %>% select(PLAYER_NAME, SEASON, ROUND, RANK_SEASON, RANK_DB),
            by = c("PLAYER_NAME","SEASON","ROUND")) %>%
  mutate(ROUND = factor(ROUND, levels = c("First Round","Conference Semifinals",
                                          "Conference Finals","NBA Finals"))) %>%
  arrange(desc(SEASON), ROUND) %>%
  select(PLAYER_NAME, SEASON, ROUND, TEAM, OPPONENT, GP, MIN, SI_MULT, POST_PTS_75,
         SI_PTS_75, TS_PLUS_ROUND, Z_SCORE, RANK_SEASON, RANK_DB, QUALIFIED)

player_names = sort(unique(playerCard$PLAYER_NAME))

#' *HEADLINE NUMBERS*
mpsrStats = list(
  series  = round(nrow(distinct(baseTable, SEASON, TEAM, ROUND)) / 2),
  first   = min(baseTable$SEASON),
  last    = max(baseTable$SEASON),
  runs    = nrow(single_round),
  players = n_distinct(single_round$PLAYER_NAME)
)

#' Year the database currently runs through, e.g. "2025-26" becomes 2026
mpsrStats$through = paste0("20", substr(mpsrStats$last, 6, 7))

#' *HOME PAGE TEASER* - the five best single round scoring runs on record
topRuns = single_round %>%
  arrange(-Z_SCORE) %>%
  slice(1:5) %>%
  select(PLAYER_NAME, SEASON, TEAM, OPPONENT, ROUND, SI_PTS_75, Z_SCORE)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' [PART 2: LOOK AND FEEL]

#' *PALETTE*
#' navy  #11394e  logo ring, hero band, table headers
#' teal  #2a7f94  links and secondary accents
#' mint  #a2e4d7  database percentile column
#' grey  #d4dbdb  season rank column
#' amber #f9cd88  the MPSR column

mpsr_logo = "https://i.ibb.co/YTBq836q/mpsr-logo.png"
mpsr_site = "https://filippospol.github.io/bball-projects/"

mpsr_theme = bslib::bs_theme(
  version         = 5,
  bg              = "#ffffff",
  fg              = "#14252f",
  primary         = "#11394e",
  secondary       = "#2a7f94",
  base_font       = bslib::font_google("IBM Plex Sans", wght = c(400, 500, 600)),
  heading_font    = bslib::font_google("Oswald", wght = c(500, 600)),
  "navbar-bg"     = "#ffffff",
  "border-radius" = "3px"
)

mpsr_css = HTML('
  /* ---- Light navbar with a mint rule --------------------------------------- */
  .navbar { padding:.35rem 0; border-bottom:3px solid #a2e4d7; position:sticky; top:0; z-index:1030; }
  .navbar-brand { display:flex; align-items:center; gap:.55rem; font-family:"Oswald";
                  font-weight:600; font-size:1.1rem; text-transform:uppercase;
                  letter-spacing:.09em; color:#11394e !important; }
  .navbar-brand a { display:flex; align-items:center; gap:.55rem; color:#11394e;
                    text-decoration:none; }
  .navbar-brand a:hover { color:#2a7f94; }
  .navbar-brand img { height:30px; border-radius:50%; }
  .navbar-nav .nav-link { color:#5b6a72 !important; font-weight:500; font-size:.82rem;
                          text-transform:uppercase; letter-spacing:.11em; padding:.5rem .95rem; }
  .navbar-nav .nav-link:hover { color:#11394e !important; }
  .navbar-nav .nav-link.active { color:#11394e !important; font-weight:600;
                                 box-shadow:inset 0 -3px 0 #f9cd88; }
  .navbar-toggler { border-color:#d4dbdb; }

  /* ---- Full bleed navy hero band ------------------------------------------- */
  .mp-band { background:#11394e; color:#dceeea; padding:2.6rem 1.5rem 0; }
  .mp-band-inner { max-width:1100px; margin:0 auto; display:flex; gap:2rem;
                   align-items:center; flex-wrap:wrap; }
  .mp-band img { width:118px; border-radius:50%; flex:0 0 auto; }
  .mp-band-txt { flex:1 1 380px; }
  .mp-band h1 { font-family:"Oswald"; font-weight:600; text-transform:uppercase;
                letter-spacing:.03em; font-size:2.3rem; line-height:1.12;
                margin:0 0 .6rem; color:#fff; }
  .mp-band h1 em { font-style:normal; color:#a2e4d7; }
  .mp-band p { font-size:1.05rem; color:#b6d3d0; margin:0; max-width:52ch; }
  .mp-updated { display:inline-block; margin-top:1rem; font-size:.72rem;
                text-transform:uppercase; letter-spacing:.11em; color:#11394e;
                background:#a2e4d7; padding:.3rem .7rem; font-weight:600; }

  /* ---- Call to action buttons ---------------------------------------------- */
  .mp-cta { margin-top:1.4rem; display:flex; gap:.6rem; flex-wrap:wrap; }
  .mp-cta .btn { font-family:"Oswald"; font-weight:500; text-transform:uppercase;
                 letter-spacing:.08em; font-size:.82rem; padding:.6rem 1.35rem;
                 border-radius:2px; }
  .mp-cta .btn-primary { background:#f9cd88; border-color:#f9cd88; color:#11394e; }
  .mp-cta .btn-primary:hover { background:#fff; border-color:#fff; color:#11394e; }
  .mp-cta .btn-outline-light { border-color:rgba(162,228,215,.5); color:#dceeea; }
  .mp-cta .btn-outline-light:hover { background:#a2e4d7; border-color:#a2e4d7; color:#11394e; }

  /* numbers sit on the bottom edge of the band */
  .mp-nums { max-width:1100px; margin:2.2rem auto 0; display:flex; flex-wrap:wrap;
             border-top:1px solid rgba(162,228,215,.28); }
  .mp-num { flex:1 1 140px; padding:1.05rem 1.2rem 1.05rem .5rem; text-align:left;
            border-right:1px solid rgba(162,228,215,.18); }
  .mp-num:last-child { border-right:none; }
  .mp-num b { display:block; font-family:"Oswald"; font-weight:600; font-size:1.75rem;
              line-height:1; color:#f9cd88; }
  .mp-num span { display:block; font-size:.74rem; color:#93b6b2; margin-top:.4rem;
                 text-transform:uppercase; letter-spacing:.1em; }

  /* ---- Body ---------------------------------------------------------------- */
  .mp-body { max-width:1100px; margin:0 auto; padding:2.4rem 1.5rem 3.5rem; }
  .mp-narrow { max-width:780px; }
  .mp-full { max-width:1620px; margin:0 auto; padding:1.6rem 1rem 3.5rem; }

  h2 { font-family:"Oswald"; font-weight:600; text-transform:uppercase; letter-spacing:.06em;
       color:#11394e; font-size:1.35rem; margin:0 0 1rem; padding-bottom:.5rem;
       border-bottom:2px solid #a2e4d7; }
  h3 { font-family:"Oswald"; font-weight:500; text-transform:uppercase; letter-spacing:.07em;
       color:#2a7f94; font-size:.95rem; margin:1.8rem 0 .55rem; }
  p, li { font-size:1rem; line-height:1.65; color:#31434c; }
  a { color:#2a7f94; }

  /* two column intro prose on the home page */
  .mp-split { display:grid; grid-template-columns:1fr 1fr; gap:2.2rem; }
  .mp-split p:first-child { margin-top:0; }
  @media (max-width:820px) { .mp-split { grid-template-columns:1fr; gap:0; } }

  .mp-links { margin-top:2rem; padding-top:1.1rem; border-top:1px solid #e3eaea;
              font-size:.92rem; color:#6b7b84; }

  /* ---- Steps --------------------------------------------------------------- */
  .mp-steps { list-style:none; padding:0; margin:0; counter-reset:step; }
  .mp-steps li { counter-increment:step; position:relative; padding:0 0 .9rem 2.4rem; margin:0; }
  .mp-steps li:before { content:counter(step); position:absolute; left:0; top:.05rem;
                        width:1.55rem; height:1.55rem; border-radius:50%;
                        background:#a2e4d7; color:#11394e; font-family:"Oswald";
                        font-weight:600; font-size:.85rem; display:flex;
                        align-items:center; justify-content:center; }

  .mp-thresh { font-size:.95rem; color:#4a5c65; margin-top:1.6rem;
               padding:.85rem 1.1rem; background:#f4f7f7; border:1px solid #e3eaea; }
  .mp-thresh b { color:#11394e; }

  .mp-limits { font-size:.95rem; color:#4a5c65; margin-top:1.1rem;
               padding:.85rem 1.1rem; background:#fdf7ec; border-left:4px solid #f9cd88; }
  .mp-limits b { color:#11394e; }

  /* ---- Plain tables: home teaser, illustration, glossary -------------------- */
  .mp-mini { width:100%; border-collapse:collapse; font-size:.93rem; margin-top:.4rem; }
  .mp-mini th { font-family:"Oswald"; font-weight:500; text-transform:uppercase;
                letter-spacing:.06em; font-size:.72rem; color:#6b7b84; text-align:left;
                padding:.35rem .6rem; border-bottom:2px solid #e3eaea; white-space:nowrap; }
  .mp-mini td { padding:.5rem .6rem; border-bottom:1px solid #eef2f2; color:#31434c; }
  .mp-mini tr:last-child td { border-bottom:none; }
  .mp-mini .rk { font-family:"Oswald"; font-weight:600; color:#a9b7bd; width:2rem; }
  .mp-mini .pl { font-weight:600; color:#11394e; white-space:nowrap; }
  .mp-mini .sc { text-align:right; font-weight:700; color:#11394e;
                 background:#f9cd88; width:4.5rem; }
  .mp-mini .num { text-align:right; white-space:nowrap; }
  .mp-mini .dim { color:#6b7b84; font-size:.88rem; }
  .mp-teaser { margin-top:2.2rem; }
  .mp-teaser h2 { margin-bottom:.4rem; }
  @media (max-width:700px) { .mp-mini .hide-xs { display:none; } }

  /* the worked examples on the methodology page */
  .mp-illus { margin-top:.6rem; border:1px solid #e3eaea; padding:.9rem 1.1rem .4rem; }
  .mp-illus .cap { font-size:.86rem; color:#6b7b84; margin:.6rem 0 .3rem; }
  .mp-illus .up { color:#2a7f94; font-weight:600; }
  .mp-illus .dn { color:#b06a4a; font-weight:600; }

  /* glossary */
  .mp-gloss { width:100%; border-collapse:collapse; font-size:.95rem; margin-top:.4rem; }
  .mp-gloss th { width:8.5rem; text-align:left; vertical-align:top; padding:.5rem .7rem .5rem 0;
                 font-family:"Oswald"; font-weight:500; color:#2a7f94; font-size:.95rem;
                 letter-spacing:.03em; border-bottom:1px solid #eef2f2; white-space:nowrap; }
  .mp-gloss td { padding:.5rem 0; color:#4a5c65; border-bottom:1px solid #eef2f2;
                 line-height:1.55; }
  .mp-gloss tr:last-child th, .mp-gloss tr:last-child td { border-bottom:none; }

  /* ---- Player card --------------------------------------------------------- */
  .mp-pc-controls { display:flex; gap:1.6rem; align-items:center; flex-wrap:wrap;
                    justify-content:center; margin-bottom:1.6rem; }
  .mp-pc-controls .form-group { margin-bottom:0; }
  .mp-pc-controls .form-check, .mp-pc-controls .checkbox { margin:0; }
  .mp-pc-controls label { font-size:.9rem; color:#4a5c65; margin-bottom:0; }
  .mp-exp-btn { font-family:"Oswald"; font-weight:500; text-transform:uppercase;
                letter-spacing:.08em; font-size:.76rem; padding:.45rem 1.1rem;
                border-radius:2px; background:#11394e; border:1px solid #11394e;
                color:#fff; }
  .mp-exp-btn:hover { background:#2a7f94; border-color:#2a7f94; color:#fff; }

  .mp-pc { max-width:660px; margin:0 auto; }
  .mp-pc .ttl { text-align:center; font-family:"Oswald"; font-weight:600;
                text-transform:uppercase; color:#11394e; line-height:1.1;
                margin:0 0 1rem; }
  .mp-pc .ttl .nm { display:block; font-size:1.85rem; letter-spacing:.02em; }
  .mp-pc .ttl .sb { display:block; font-size:1.15rem; color:#2a7f94;
                    letter-spacing:.08em; margin-top:.15rem; }
  .mp-pc .ttl:after { content:""; display:block; width:100%; height:3px;
                      background:#11394e; margin:.7rem auto 0; }

  .mp-pc table { width:100%; border-collapse:collapse; font-size:.86rem;
                 font-variant-numeric:tabular-nums; }
  .mp-pc th { background:#11394e; color:#fff; font-family:"Oswald"; font-weight:500;
              text-transform:uppercase; letter-spacing:.05em; font-size:.7rem;
              padding:.5rem .45rem; text-align:right; white-space:nowrap;
              border-right:1px solid rgba(255,255,255,.14); }
  .mp-pc th:first-child, .mp-pc th.l { text-align:left; }
  .mp-pc th:last-child { border-right:none; }
  .mp-pc td { padding:.36rem .45rem; text-align:right; white-space:nowrap;
              border-bottom:1px solid #eef2f2; color:#31434c; }
  .mp-pc td.l { text-align:left; }
  .mp-pc tr.alt td { background:#f7fafa; }
  .mp-pc tr.tot td { background:#e6ecec; font-weight:700; color:#11394e;
                     border-bottom:1px solid #d4dbdb; }
  .mp-pc tr.car td { background:#11394e; color:#fff; font-weight:700;
                     border-bottom:none; }
  .mp-pc tr.unq td { color:#9aa8ae; font-style:italic; }
  .mp-pc .yr { color:#6b7b84; }
  .mp-pc tr.tot .yr, .mp-pc tr.car .yr { color:inherit; }
  .mp-pc .sc { font-weight:700; color:#11394e; }
  .mp-pc tr.car .sc { color:#11394e; }
  .mp-pc .up { color:#2a7f94; font-weight:600; }
  .mp-pc .dn { color:#b06a4a; font-weight:600; }
  .mp-pc tr.car .up, .mp-pc tr.car .dn { color:#a2e4d7; }
  .mp-pc .foot { font-size:.78rem; color:#7d8b92; margin-top:.8rem; text-align:center; }

  /* branding strip: hidden on screen, drawn into the exported image */
  .mp-pc .exp { display:none; align-items:center; justify-content:center; gap:.6rem;
                margin-top:1rem; padding-top:.8rem; border-top:2px solid #a2e4d7; }
  .mp-pc.exporting .exp { display:flex; }
  .mp-pc.exporting { background:#fff; padding:1.4rem 1.6rem; }
  .mp-pc .exp img { width:38px; height:38px; border-radius:50%; }
  .mp-pc .exp b { display:block; font-family:"Oswald"; font-weight:600; font-size:.82rem;
                  text-transform:uppercase; letter-spacing:.07em; color:#11394e;
                  line-height:1.2; }
  .mp-pc .exp span { display:block; font-size:.72rem; color:#7d8b92; letter-spacing:.01em; }
  .mp-pc.exporting .foot { margin-bottom:0; }
  @media (max-width:620px) {
    .mp-pc table { font-size:.79rem; }
    .mp-pc .hide-xs { display:none; }
    .mp-pc .ttl .nm { font-size:1.35rem; }
  }

  /* ---- Tables -------------------------------------------------------------- */
  .mp-caption { font-size:.85rem; color:#7d8b92; margin:0 0 1rem; }
  .mp-caption b { color:#11394e; }

  .mp-table .rt-thead { position:sticky; top:0; z-index:2; }
  .mp-table .rt-th-group { background:#0b2a3a !important; font-size:11px;
                           letter-spacing:.1em; text-transform:uppercase; }
  .mp-table .rt-search { width:280px; margin-bottom:.8rem; }

  /* ---- Mobile -------------------------------------------------------------- */
  @media (max-width:760px) {
    .mp-hide-sm { display:none !important; }
    .mp-band { padding:1.8rem 1rem 0; }
    .mp-band h1 { font-size:1.6rem; }
    .mp-band img { width:76px; }
    .mp-num b { font-size:1.35rem; }
    .mp-body, .mp-full { padding:1.6rem 1rem 2.5rem; }
    .mp-table .rt-search { width:100%; }
  }
')

mpsr_spin = function(ui) {
  shinycssloaders::withSpinner(ui, type = 6, color = "#2a7f94",
                               color.background = "#ffffff", size = 0.8)
}

#' *HOME PAGE TEASER TABLE* - plain HTML, so the home page needs no reactable
mpsr_teaser = function(d) {
  short = c("First Round" = "R1", "Conference Semifinals" = "CSF",
            "Conference Finals" = "CF", "NBA Finals" = "Finals")
  tags$table(
    class = "mp-mini",
    tags$thead(tags$tr(
      tags$th(""), tags$th("Player"), tags$th("Season"),
      tags$th(class = "hide-xs", "Matchup"), tags$th(class = "hide-xs", "Round"),
      tags$th(class = "num", "SI Pts/75"), tags$th(class = "num", "MPSR"))),
    tags$tbody(
      lapply(seq_len(nrow(d)), function(i) {
        r = d[i, ]
        tags$tr(
          tags$td(class = "rk", i),
          tags$td(class = "pl", r$PLAYER_NAME),
          tags$td(class = "dim", r$SEASON),
          tags$td(class = "dim hide-xs", paste(r$TEAM, "vs", r$OPPONENT)),
          tags$td(class = "dim hide-xs", unname(short[r$ROUND])),
          tags$td(class = "num", sprintf("%.1f", r$SI_PTS_75)),
          tags$td(class = "sc", sprintf("%.2f", r$Z_SCORE)))
      })))
}

#' *ABBREVIATIONS* - keep the player card narrow
mpsr_tm = c(
  "Atlanta Hawks"="ATL","Boston Celtics"="BOS","Brooklyn Nets"="BKN",
  "Charlotte Bobcats"="CHA","Charlotte Hornets"="CHA","Chicago Bulls"="CHI",
  "Cleveland Cavaliers"="CLE","Dallas Mavericks"="DAL","Denver Nuggets"="DEN",
  "Detroit Pistons"="DET","Golden State Warriors"="GSW","Houston Rockets"="HOU",
  "Indiana Pacers"="IND","LA Clippers"="LAC","Los Angeles Lakers"="LAL",
  "Memphis Grizzlies"="MEM","Miami Heat"="MIA","Milwaukee Bucks"="MIL",
  "Minnesota Timberwolves"="MIN","New Jersey Nets"="NJN","New Orleans Hornets"="NOH",
  "New Orleans Pelicans"="NOP","New York Knicks"="NYK","Oklahoma City Thunder"="OKC",
  "Orlando Magic"="ORL","Philadelphia 76ers"="PHI","Phoenix Suns"="PHX",
  "Portland Trail Blazers"="POR","Sacramento Kings"="SAC","San Antonio Spurs"="SAS",
  "Seattle SuperSonics"="SEA","Toronto Raptors"="TOR","Utah Jazz"="UTA",
  "Washington Wizards"="WAS")

mpsr_rd = c("First Round"="First Round","Conference Semifinals"="Conf Semis",
            "Conference Finals"="Conf Finals","NBA Finals"="Finals")

#' Season label: the playoffs of "2025-26" happen in 2026
mpsr_yr = function(s) paste0("20", substr(s, 6, 7))

#' *MPSR CELL COLOUR* - anchored to the whole database, not to one player
mpsr_amber = local({
  pal = colorRampPalette(c("#fdf6ea", "#f9cd88", "#e09b2b"))(100)
  rng = range(playerCard$Z_SCORE, na.rm = TRUE)
  function(v) {
    if (is.na(v)) return("transparent")
    pal[min(100, max(1, 1 + round((v - rng[1]) / diff(rng) * 99)))]
  }
})

#' *PLAYER CARD* - a compact, centred card rather than a full leaderboard
mpsr_card = function(d, name, totals = NULL, n_all = NULL) {
  
  num = function(x, dig = 1) if (is.na(x)) "\u2014" else formatC(x, format = "f", digits = dig)
  ts  = function(x) {
    if (is.na(x)) return(tags$td("\u2014"))
    tags$td(class = if (x > 100) "up" else if (x < 100) "dn" else NULL, round(x))
  }
  sc  = function(x) {
    if (is.na(x)) return(tags$td("\u2014"))
    tags$td(class = "sc", style = paste0("background:", mpsr_amber(x), ";"), num(x, 2))
  }
  
  # one block per season: the rounds, then a bold Postseason row
  seasons = split(d, d$SEASON)
  seasons = seasons[order(names(seasons), decreasing = TRUE)]
  
  body = do.call(c, lapply(seasons, function(s) {
    s = s[order(s$ROUND), ]
    rows = lapply(seq_len(nrow(s)), function(i) {
      r = s[i, ]
      cls = paste(c(if (i %% 2 == 0) "alt", if (!r$QUALIFIED) "unq"), collapse = " ")
      tags$tr(class = if (nzchar(cls)) cls else NULL,
              tags$td(class = "l yr", mpsr_yr(r$SEASON)),
              tags$td(class = "l", paste0(unname(mpsr_rd[as.character(r$ROUND)]),
                                          if (!r$QUALIFIED) " \u2020" else "")),
              tags$td(class = "hide-xs", unname(mpsr_tm[r$TEAM])),
              tags$td(unname(mpsr_tm[r$OPPONENT])),
              tags$td(r$GP),
              tags$td(num(r$SI_PTS_75)),
              ts(r$TS_PLUS_ROUND),
              sc(r$Z_SCORE))
    })
    # Postseason total: read straight from the shared aggregate so this row equals
    # the Whole Postseason leaderboard. Fall back to the shown rounds only when the
    # season has no qualified aggregate (e.g. a cameo-only season revealed by the
    # include-non-qualified toggle), in which case there is no MPSR to show.
    tr_row = if (!is.null(totals)) totals[totals$SEASON == s$SEASON[1], ] else NULL
    if (!is.null(tr_row) && nrow(tr_row) == 1) {
      t_gp = tr_row$TOT_GP; t_si = tr_row$TOT_SI
      t_tsp = tr_row$TOT_TSPLUS; t_z = tr_row$TOT_Z
    } else {
      t_gp  = sum(s$GP)
      t_si  = s$POST_PTS_75[1] * weighted.mean(s$SI_MULT, s$MIN, na.rm = TRUE)
      t_tsp = round(weighted.mean(s$TS_PLUS_ROUND, s$MIN, na.rm = TRUE))
      t_z   = NA_real_
    }
    tot = tags$tr(class = "tot",
                  tags$td(class = "l yr", mpsr_yr(s$SEASON[1])),
                  tags$td(class = "l", "Postseason"),
                  tags$td(class = "hide-xs", unname(mpsr_tm[s$TEAM[1]])),
                  tags$td("TOT"),
                  tags$td(t_gp),
                  tags$td(num(t_si)),
                  ts(t_tsp),
                  sc(t_z))
    c(rows, list(tot))
  }))
  
  # Career line: a minute weighted blend of the player's Postseason totals above,
  # across the seasons shown. It has no leaderboard counterpart, so it stays a
  # weighted average of the season rows (scored only over seasons that have a MPSR).
  ct = if (!is.null(totals)) totals[totals$SEASON %in% names(seasons), ] else NULL
  career = if (length(seasons) > 1 && !is.null(ct) && nrow(ct) > 0) {
    cz = ct[!is.na(ct$TOT_Z), ]
    list(tags$tr(class = "car",
                 tags$td(class = "l yr", "Career"),
                 tags$td(class = "l", "Postseason"),
                 tags$td(class = "hide-xs", "TOT"),
                 tags$td("TOT"),
                 tags$td(sum(ct$TOT_GP)),
                 tags$td(num(weighted.mean(ct$TOT_SI, ct$TOT_MIN, na.rm = TRUE))),
                 ts(round(weighted.mean(ct$TOT_TSPLUS, ct$TOT_MIN, na.rm = TRUE))),
                 sc(if (nrow(cz) > 0) weighted.mean(cz$TOT_Z, cz$TOT_MIN, na.rm = TRUE)
                    else NA_real_))) }
  else NULL
  
  sub = if (length(seasons) == 1) paste(mpsr_yr(names(seasons)), "Postseason")
  else if (!is.null(n_all) && length(seasons) < n_all)
    paste0(mpsr_yr(min(names(seasons))), "\u2013", mpsr_yr(max(names(seasons))), " Postseasons")
  else "Career Postseason"
  
  div(class = "mp-pc", id = "mpsr-card", `data-name` = name,
      div(class = "ttl", tags$span(class = "nm", name),
          tags$span(class = "sb", sub)),
      tags$table(
        tags$thead(tags$tr(
          tags$th(class = "l", "Year"), tags$th(class = "l", "Round"),
          tags$th(class = "hide-xs", "Tm"), tags$th("Opp"), tags$th("GP"),
          tags$th("SI Pts/75"), tags$th("TS+"), tags$th("MPSR"))),
        tags$tbody(body, career)),
      # shown only inside the exported image
      div(class = "exp",
          img(src = mpsr_logo, alt = ""),
          div(tags$b("Modern Playoff Scoring Runs"),
              tags$span("filippos-pol.shinyapps.io/modern-playoff-scoring-runs"))))
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' [PART 3: USER INTERFACE]

ui = bslib::page_navbar(
  id           = "nav",
  theme        = mpsr_theme,
  window_title = "Modern Playoff Scoring Runs",
  title        = tags$span(
    tags$a(href = "#", onclick = "Shiny.setInputValue('brand_click', Math.random()); return false;",
           img(src = mpsr_logo, alt = ""), "MPSR")),
  header       = tags$head(
    tags$style(mpsr_css),
    tags$link(rel = "shortcut icon", href = mpsr_logo),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"),
    tags$script(HTML("
      function mpsrExport() {
        var node = document.getElementById('mpsr-card');
        if (!node || typeof html2canvas === 'undefined') return;
        node.classList.add('exporting');
        html2canvas(node, {backgroundColor: '#ffffff', scale: 2, useCORS: true})
          .then(function(canvas) {
            node.classList.remove('exporting');
            var a = document.createElement('a');
            a.download = (node.getAttribute('data-name') || 'mpsr-card')
                           .replace(/[^A-Za-z0-9]+/g, '-') + '-mpsr.png';
            a.href = canvas.toDataURL('image/png');
            a.click();
          })
          .catch(function() { node.classList.remove('exporting'); });
      }"))
  ),
  
  # ---- Home ------------------------------------------------------------------
  bslib::nav_panel(
    "Home",
    div(class = "mp-band",
        div(class = "mp-band-inner",
            img(src = mpsr_logo, alt = "Modern Playoff Scoring Runs logo"),
            div(class = "mp-band-txt",
                h1("Playoff scoring, ", tags$em("adjusted for the defense faced")),
                p("The best postseason scoring runs since 2000, measured against the defenses
                   that actually had to stop them."),
                div(class = "mp-cta",
                    actionButton("go_single", "Browse the leaderboard", class = "btn-primary"),
                    actionButton("go_method", "How it works", class = "btn-outline-light")),
                div(class = "mp-updated",
                    glue("Updated through the {mpsrStats$through} NBA Finals")))),
        div(class = "mp-nums",
            div(class = "mp-num", tags$b(format(mpsrStats$series, big.mark = ",")), tags$span("Playoff series")),
            div(class = "mp-num", tags$b(format(mpsrStats$runs, big.mark = ",")), tags$span("Scoring runs")),
            div(class = "mp-num", tags$b(format(mpsrStats$players, big.mark = ",")), tags$span("Players")),
            div(class = "mp-num", tags$b(glue("{substr(mpsrStats$first,1,4)}-{substr(mpsrStats$last,6,7)}")),
                tags$span("Seasons")))),
    
    div(class = "mp-body",
        div(class = "mp-split",
            div(p("Playoff scoring numbers are hard to read on their own. Thirty points against a
                 bottom ten defense is not thirty points against the best defense in the league,
                 and raw True Shooting says little when a whole series is played at a crawl."),
                p("MPSR corrects for both. Volume is taken per 75 possessions and scaled by the
                 quality of the defense on the other side of the series. Efficiency is measured
                 against the shooting environment of that specific series, then weighted by how
                 much of the offense the player was carrying.")),
            div(p("The result is one number per scoring run that can be compared across rounds,
                 opponents and seasons, in a single round or across a whole postseason."),
                p("A project by ",
                  a(href = "https://twitter.com/taylormetrics", "Taylor Wyman", target = "_blank"),
                  " and ",
                  a(href = mpsr_site, "Filippos Polyzos", target = "_blank"), "."))),
        div(class = "mp-teaser",
            h2("The five best single round runs on record"),
            p(class = "mp-caption", "Every qualified player, every playoff series since 2000."),
            mpsr_teaser(topRuns)),
        
        div(class = "mp-links",
            "Sources: ",
            a(href = "https://www.stats.nba.com", "NBA Stats", target = "_blank"), ", ",
            a(href = "https://www.basketball-reference.com", "Basketball Reference", target = "_blank"),
            glue(". {mpsrStats$first} to {mpsrStats$last}.")))
  ),
  
  # ---- Player card -----------------------------------------------------------
  bslib::nav_panel(
    "Players",
    div(class = "mp-body",
        div(class = "mp-pc-controls",
            selectizeInput("pc_player", NULL, choices = NULL, width = "280px",
                           options = list(placeholder = "Search for a player...")),
            selectizeInput("pc_seasons", NULL, choices = NULL, multiple = TRUE,
                           width = "260px",
                           options = list(placeholder = "All seasons",
                                          plugins = list("remove_button"))),
            checkboxInput("pc_all", "Include non qualified series (\u2020)", value = FALSE),
            tags$button("Export PNG", class = "btn btn-primary mp-exp-btn",
                        onclick = "mpsrExport(); return false;")),
        mpsr_spin(uiOutput("pc_card")))
  ),
  
  # ---- Single round ----------------------------------------------------------
  bslib::nav_panel(
    "Single Round",
    div(class = "mp-full",
        h2("Single Round"),
        p(class = "mp-caption",
          HTML("One row per player, per playoff series. Qualification: <b>100 minutes</b> played and
                <b>4 games</b> in the series. Column abbreviations are explained in the Glossary,
                at the bottom of the Methodology page.")),
        mpsr_spin(reactableOutput("single", width = "100%")))
  ),
  
  # ---- Whole postseason ------------------------------------------------------
  bslib::nav_panel(
    "Whole Postseason",
    div(class = "mp-full",
        h2("Whole Postseason"),
        p(class = "mp-caption",
          HTML("One row per player, per postseason. Qualification: <b>200 total minutes</b> and
                <b>more than one round</b> played. Column abbreviations are explained in the Glossary,
                at the bottom of the Methodology page.")),
        mpsr_spin(reactableOutput("whole", width = "100%")))
  ),
  
  # ---- Methodology -----------------------------------------------------------
  bslib::nav_panel(
    "Methodology",
    div(class = "mp-body mp-narrow",
        h2("Methodology"),
        p("Every row is one player, in one playoff series. For each row the MPSR value is
           calculated as follows:"),
        tags$ol(class = "mp-steps",
                tags$li(HTML("Get the scoring volume (<b>Points/75</b>) and adjust it for the strength
                              of the defense faced in that series. Points scored against a stingy
                              defense are worth more than the same output against a soft one, and the
                              adjustment is scaled to the scoring environment of that season so the
                              numbers travel across eras. The result is
                              <b>Series Adjusted Points per 75</b> (SI Points/75).")),
                tags$li(HTML("Measure shooting efficiency against the series itself
                              (<b>TS+ relative to series</b>), where 100 is exactly what everyone
                              else managed in those same games.")),
                tags$li(HTML("Weight the <i>edge</i> over that series baseline by usage rate
                              (<b>uTS+</b>), so it counts in proportion to how much of the offense
                              the player carried. Shooting exactly as well as the series scores
                              zero, however many possessions it took.")),
                tags$li(HTML("<b>MPSR</b> is the weighted average of the standardized (z-score)
                              volume and efficiency values, weighted equally."))),
        
        div(class = "mp-illus",
            tags$table(class = "mp-mini",
                       tags$thead(tags$tr(
                         tags$th("Same scoring run"), tags$th(class = "num", "Points/75"),
                         tags$th("Defense faced"), tags$th(class = "num", "SI Points/75"))),
                       tags$tbody(
                         tags$tr(tags$td(class = "pl", "vs an elite defense"),
                                 tags$td(class = "num", "25.0"),
                                 tags$td(class = "dim", "well above average"),
                                 tags$td(class = "num up", "27.5")),
                         tags$tr(tags$td(class = "pl", "vs a poor defense"),
                                 tags$td(class = "num", "25.0"),
                                 tags$td(class = "dim", "well below average"),
                                 tags$td(class = "num dn", "23.1")))),
            p(class = "cap", "Identical raw output, valued differently by who had to guard it.")),
        
        div(class = "mp-illus",
            tags$table(class = "mp-mini",
                       tags$thead(tags$tr(
                         tags$th("Same shooting"), tags$th(class = "num", "TS+"),
                         tags$th(class = "num", "Usage"), tags$th(class = "num", "uTS+"))),
                       tags$tbody(
                         tags$tr(tags$td(class = "pl", "carrying the offense"),
                                 tags$td(class = "num", "118"), tags$td(class = "num", "32%"),
                                 tags$td(class = "num up", "+28.8")),
                         tags$tr(tags$td(class = "pl", "in a secondary role"),
                                 tags$td(class = "num", "118"), tags$td(class = "num", "22%"),
                                 tags$td(class = "num up", "+19.8")),
                         tags$tr(tags$td(class = "pl", "no edge at all"),
                                 tags$td(class = "num", "100"), tags$td(class = "num", "32%"),
                                 tags$td(class = "num", "0.0")))),
            p(class = "cap", "The same efficiency counts for more the heavier the load. A player who
                              only matches his series earns nothing from it, however much he used.")),
        
        div(class = "mp-thresh",
            HTML("<b>How to read the score.</b> MPSR is standardized, so the whole database shares one
                  scale. Around 0 is a typical qualified playoff run, 1 is a clearly strong one, and
                  2.5 or higher belongs to a few dozen series in the last 25 years. Negative values
                  are runs that fell short of the postseason field.")),
        
        div(class = "mp-thresh",
            HTML("For the <b>Single Round</b> table we set a threshold of 100 minutes played and 4 games
                  in the series. For the <b>Whole Postseason</b> table, 200 total minutes and more than
                  one round.")),
        
        div(class = "mp-limits",
            HTML("<b>What this does not capture.</b> Teammate quality, injuries and shot difficulty all
                  sit outside the box score. Series ratings also come from small samples, four to seven
                  games, so one blowout can move them. MPSR describes what a scoring run looked like in
                  its context; it does not isolate the player from it.")),
        
        h2("Glossary", style = "margin-top:2.6rem;"),
        p("Every column shown in the two leaderboards."),
        tags$table(class = "mp-gloss",
                   tags$tbody(
                     tags$tr(tags$th("DB %ile"),    tags$td("Percentile of the MPSR score against every qualified run in the database.")),
                     tags$tr(tags$th("Sn Rk"),      tags$td("Rank within that postseason.")),
                     tags$tr(tags$th("Pos / Age"),  tags$td("Listed position and age for that season.")),
                     tags$tr(tags$th("Rds"),        tags$td("Playoff rounds played that postseason.")),
                     tags$tr(tags$th("G / MP"),     tags$td("Games played and total minutes.")),
                     tags$tr(tags$th("Pts/75"),     tags$td("Points per 75 possessions, before any adjustment. Removes pace.")),
                     tags$tr(tags$th("Def"),        tags$td("Defensive rating of the opponent in that series, shown relative to the league average. Negative means a better defense.")),
                     tags$tr(tags$th("SI Pts/75"),  tags$td("Series Adjusted Points per 75. Scoring volume revalued for the strength of the defense faced, on the scoring level of that season.")),
                     tags$tr(tags$th("TS%"),        tags$td("True Shooting percentage.")),
                     tags$tr(tags$th("TS+"),        tags$td("True Shooting relative to the whole series, where 100 is the series average and 115 is fifteen percent better than everyone else in the same games.")),
                     tags$tr(tags$th("Usg"),        tags$td("Share of team possessions the player used while on the floor.")),
                     tags$tr(tags$th("uTS+"),       tags$td("The efficiency edge over the series, scaled by usage, so it counts in proportion to the load carried. 0 means the player shot exactly as well as his series; negative means worse.")),
                     tags$tr(tags$th("MPSR"),       tags$td("The final score. Higher is better; roughly, 1 is a strong run and 2.5 or more is among the best on record.")))))
  )
)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' [PART 4: SERVER]

mpsr_reactable_theme = reactableTheme(
  headerStyle      = list(background = "#11394e", color = "#fff", fontWeight = 500,
                          fontFamily = "Oswald", fontSize = 12, letterSpacing = ".03em",
                          textTransform = "uppercase", whiteSpace = "normal",
                          lineHeight = 1.25),
  groupHeaderStyle = list(color = "#fff"),
  stripedColor     = "#f5f8f8",
  highlightColor   = "#eafaf6",
  borderColor      = "#e6ecec",
  cellPadding      = "7px 10px",
  style            = list(fontFamily = "IBM Plex Sans", fontSize = 13)
)

#' *SIGNED VALUE COLOURING* (teal above the centre, muted rust below)
mpsr_signed = function(value, centre = 0) {
  col = if (is.na(value)) "#6b7b84" else if (value > centre) "#2a7f94" else if (value < centre) "#b06a4a" else "#6b7b84"
  list(color = col, fontWeight = 600)
}

#' *MPSR COLUMN RAMP* (pale sand to the brand amber)
mpsr_score_scale = function(values) {
  pal = colorRampPalette(c("#fdf6ea", "#f9cd88", "#e09b2b"))(100)
  rng = range(values, na.rm = TRUE)
  function(value) {
    if (is.na(value)) return(list())
    i = 1 + round((value - rng[1]) / diff(rng) * 99)
    list(background = pal[i], fontWeight = 600, color = "#11394e")
  }
}

#' *DROPDOWN COLUMN FILTER*
mpsr_select_filter = function(table_id, decreasing = FALSE) {
  function(values, name) {
    tags$select(
      onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value)", table_id, name),
      tags$option(value = "", "All"),
      lapply(sort(unique(values), decreasing = decreasing), tags$option),
      style = "width:100%; font-size:12px;")
  }
}

#' *SHARED COLUMN DEFINITIONS*
#' Both leaderboards show the same fourteen measures under different column names
#' (SI_PTS_75 vs AVG_SI_PTS_75 and so on), so they are defined once here. `nm` maps
#' the generic key to the actual column name in that table, and `tips` overrides
#' any header tooltip whose wording differs between the two.
mpsr_shared_cols = function(tbl, nm, tips = list(), score_values, team_w = 130) {
  
  hdr = function(txt) function(value) span(title = txt, value)
  
  tips = modifyList(list(
    pctile = "Percentile among every qualified run in the database",
    rank   = "Rank within that postseason",
    pts75  = "Points per 75 possessions, before the defensive adjustment",
    def    = "Defensive rating of the opponent in that series, relative to the league average. Negative is a better defense",
    si     = "Series Adjusted Points per 75: scoring volume revalued for the strength of the defense faced",
    tsplus = "True Shooting relative to the whole series: 100 = the series average",
    uts    = "The efficiency edge over the series, scaled by how much of the offense the player carried. 0 is neutral",
    score  = "The MPSR score for this run"
  ), tips)
  
  out = list()
  out[[nm[["pctile"]]]] = colDef(name = "DB %ile", width = 74, align = "left",
                                 sticky = "left", format = colFormat(digits = 1),
                                 header = hdr(tips$pctile),
                                 style = list(background = "#a2e4d7", fontWeight = 600,
                                              color = "#11394e"))
  out[[nm[["rank"]]]]   = colDef(name = "Sn Rk", width = 66, align = "left",
                                 sticky = "left", header = hdr(tips$rank),
                                 style = list(background = "#d4dbdb", fontWeight = 600,
                                              color = "#11394e"))
  out[["PLAYER_NAME"]]  = colDef(name = "Player", align = "left", width = 165,
                                 sticky = "left", filterable = TRUE,
                                 style = list(fontWeight = 600,
                                              borderRight = "2px solid #c9d5d5"))
  out[["AGE"]]          = colDef(name = "Age", width = 52,
                                 class = "mp-hide-sm", headerClass = "mp-hide-sm")
  out[["SEASON"]]       = colDef(name = "Season", align = "left", width = 80,
                                 filterable = TRUE,
                                 filterInput = mpsr_select_filter(tbl, decreasing = TRUE))
  out[["TEAM"]]         = colDef(name = "Team", align = "left",
                                 minWidth = team_w, filterable = TRUE)
  out[["GP"]]           = colDef(name = "G", width = 48)
  out[["MIN"]]          = colDef(name = "MP", width = 62)
  
  out[[nm[["pts75"]]]]  = colDef(name = "Pts/75", width = 70,
                                 format = colFormat(digits = 1),
                                 header = hdr(tips$pts75),
                                 style = list(color = "#6b7b84"))
  out[[nm[["def"]]]]    = colDef(name = "Def", width = 62, header = hdr(tips$def),
                                 cell = function(value) sprintf("%+.1f", value),
                                 style = function(value) mpsr_signed(value, 0))
  out[[nm[["si"]]]]     = colDef(name = "SI Pts/75", width = 80,
                                 format = colFormat(digits = 1), header = hdr(tips$si),
                                 style = list(fontWeight = 600, color = "#11394e"))
  
  out[[nm[["ts"]]]]     = colDef(name = "TS%", width = 62,
                                 format = colFormat(percent = TRUE, digits = 1))
  out[[nm[["tsplus"]]]] = colDef(name = "TS+", width = 62, header = hdr(tips$tsplus),
                                 style = function(value) mpsr_signed(value, 100))
  out[[nm[["usg"]]]]    = colDef(name = "Usg", width = 58,
                                 format = colFormat(percent = TRUE, digits = 1),
                                 class = "mp-hide-sm", headerClass = "mp-hide-sm")
  out[[nm[["uts"]]]]    = colDef(name = "uTS+", width = 60, header = hdr(tips$uts),
                                 cell = function(value) sprintf("%+.1f", value),
                                 style = function(value) mpsr_signed(value, 0))
  
  out[[nm[["score"]]]]  = colDef(name = "MPSR", width = 70,
                                 format = colFormat(digits = 2), header = hdr(tips$score),
                                 style = mpsr_score_scale(score_values),
                                 headerStyle = list(background = "#0b2a3a"))
  out
}

server = function(input, output, session) {
  
  # ---- Navigation ------------------------------------------------------------
  observeEvent(input$go_single,   bslib::nav_select("nav", "Single Round"))
  observeEvent(input$go_method,   bslib::nav_select("nav", "Methodology"))
  observeEvent(input$brand_click, bslib::nav_select("nav", "Home"))
  
  # ---- Single round ----------------------------------------------------------
  output$single = renderReactable({
    d = single_round
    reactable(
      d,
      class = "mp-table",
      defaultSorted = "Z_SCORE", defaultSortOrder = "desc",
      defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(10, 25, 50, 100),
      searchable = TRUE, striped = TRUE, highlight = TRUE, wrap = FALSE, showSortable = TRUE,
      language = reactableLang(searchPlaceholder = "Search player, team or opponent..."),
      theme = mpsr_reactable_theme,
      defaultColDef = colDef(align = "right", vAlign = "center", headerVAlign = "bottom"),
      
      columnGroups = list(
        colGroup(name = "Series", columns = c("SEASON", "TEAM", "ROUND", "OPPONENT", "GP", "MIN")),
        colGroup(name = "Volume", columns = c("PTS_75", "OPP_REL_DEF", "SI_PTS_75")),
        colGroup(name = "Efficiency", columns = c("TS_PCT", "TS_PLUS_ROUND", "USG_PCT", "uTS_PLUS")),
        colGroup(name = "Result", columns = c("Z_SCORE"))
      ),
      
      columns = c(
        mpsr_shared_cols(
          tbl = "single",
          nm = c(pctile = "RANK_DB", rank = "RANK_SEASON", pts75 = "PTS_75",
                 def = "OPP_REL_DEF", si = "SI_PTS_75", ts = "TS_PCT",
                 tsplus = "TS_PLUS_ROUND", usg = "USG_PCT", uts = "uTS_PLUS",
                 score = "Z_SCORE"),
          team_w = 114,
          tips = list(
            pctile = "Percentile among every qualified single round run",
            score  = "0 = average qualified run, 1 = one standard deviation above, 2.5+ = all time elite"),
          score_values = d$Z_SCORE),
        # columns unique to the single round table
        list(
          POS = colDef(name = "Pos", align = "left", width = 54, filterable = TRUE,
                       filterInput = mpsr_select_filter("single"),
                       class = "mp-hide-sm", headerClass = "mp-hide-sm"),
          ROUND = colDef(name = "Round", align = "left", width = 150, filterable = TRUE,
                         filterInput = mpsr_select_filter("single")),
          OPPONENT = colDef(name = "Opp", align = "left", minWidth = 114, filterable = TRUE,
                            class = "mp-hide-sm", headerClass = "mp-hide-sm"))
      )
    )
  })
  
  # ---- Whole postseason ------------------------------------------------------
  output$whole = renderReactable({
    d = multi_round
    reactable(
      d,
      class = "mp-table",
      defaultSorted = "AVG_Z", defaultSortOrder = "desc",
      defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(10, 25, 50, 100),
      searchable = TRUE, striped = TRUE, highlight = TRUE, wrap = FALSE, showSortable = TRUE,
      language = reactableLang(searchPlaceholder = "Search player, team or season..."),
      theme = mpsr_reactable_theme,
      defaultColDef = colDef(align = "right", vAlign = "center", headerVAlign = "bottom"),
      
      columnGroups = list(
        colGroup(name = "Postseason", columns = c("SEASON", "TEAM", "ROUNDS", "GP", "MIN")),
        colGroup(name = "Volume", columns = c("AVG_PTS_75", "AVG_REL_DEF", "AVG_SI_PTS_75")),
        colGroup(name = "Efficiency", columns = c("AVG_TS_PCT", "AVG_TS_PLUS", "AVG_USG", "AVG_uTS_PLUS")),
        colGroup(name = "Result", columns = c("AVG_Z"))
      ),
      
      columns = c(
        mpsr_shared_cols(
          tbl = "whole",
          nm = c(pctile = "DB_PCTILE", rank = "RANK", pts75 = "AVG_PTS_75",
                 def = "AVG_REL_DEF", si = "AVG_SI_PTS_75", ts = "AVG_TS_PCT",
                 tsplus = "AVG_TS_PLUS", usg = "AVG_USG", uts = "AVG_uTS_PLUS",
                 score = "AVG_Z"),
          team_w = 140,
          tips = list(
            pctile = "Percentile among every qualified postseason run",
            pts75  = "Points per 75 possessions across the whole postseason",
            def    = "Average defensive rating of the opponents faced, relative to the league average",
            si     = "Series Adjusted Points per 75: scoring volume revalued for the strength of the defenses faced across the run",
            tsplus = "Average series TS+ across the rounds played",
            uts    = "The average efficiency edge over the series played, scaled by load. 0 is neutral",
            score  = "0 = average qualified run, 1 = one standard deviation above, 2.5+ = all time elite"),
          score_values = d$AVG_Z),
        # column unique to the whole postseason table
        list(
          ROUNDS = colDef(name = "Rds", width = 56,
                          header = function(value) span(title = "Playoff rounds played that postseason", value)))
      )
    )
  })
  
  # ---- Player card -----------------------------------------------------------
  updateSelectizeInput(session, "pc_player", choices = player_names,
                       selected = "LeBron James", server = TRUE)
  
  # season choices follow the selected player; labels are the playoff year
  pc_season_choices = reactive({
    req(input$pc_player)
    s = playerCard %>% filter(PLAYER_NAME == input$pc_player)
    if (!isTRUE(input$pc_all)) s = s %>% filter(QUALIFIED)
    ch = sort(unique(s$SEASON), decreasing = TRUE)
    setNames(ch, mpsr_yr(ch))
  })
  
  # a new player clears the season picks
  observeEvent(input$pc_player, {
    updateSelectizeInput(session, "pc_seasons", choices = pc_season_choices(),
                         selected = character(0))
  })
  
  # toggling qualification only refreshes the list: whatever is still valid stays
  # selected, so ticking the box does not throw the user back to the career view
  observeEvent(input$pc_all, {
    ch = pc_season_choices()
    updateSelectizeInput(session, "pc_seasons", choices = ch,
                         selected = intersect(isolate(input$pc_seasons), ch))
  }, ignoreInit = TRUE)
  
  pc_data = reactive({
    req(input$pc_player)
    d = playerCard %>% filter(PLAYER_NAME == input$pc_player)
    if (!isTRUE(input$pc_all)) d = d %>% filter(QUALIFIED)
    keep = intersect(input$pc_seasons, d$SEASON)   # drop stale picks from a previous player
    if (length(keep) > 0) d = d %>% filter(SEASON %in% keep)
    d
  })
  
  output$pc_card = renderUI({
    d = pc_data()
    if (nrow(d) == 0)
      return(div(class = "mp-pc", p(class = "foot",
                                    "No qualified series for this player. Tick the box above to show
                  every series he played.")))
    all_d = playerCard %>% filter(PLAYER_NAME == input$pc_player)
    if (!isTRUE(input$pc_all)) all_d = all_d %>% filter(QUALIFIED)
    tot = pcTotals %>% filter(PLAYER_NAME == input$pc_player)
    mpsr_card(d, input$pc_player, totals = tot, n_all = n_distinct(all_d$SEASON))
  })
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' *RUN THE APPLICATION*
shinyApp(ui = ui, server = server)