# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script builds the clean data for the Outbound Comms app. It writes:
#   - misc/outbound-clean-data.csv  (plain CSV, kept for other consumers)
#   - misc/outbound-clean-data.rds  (typed bundle the app loads fast: the full
#     dataset + the CQS exclusion list + a build timestamp)
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' [ =============== SETUP =============== ]

# Load packages:
library(dplyr)
library(googlesheets4)
library(lubridate)
library(readr)
library(purrr)
library(tidyr)
library(stringr)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' [ =============== ACCESS SHEET =============== ]

# Adjust maximum upload capacity:
options(shiny.maxRequestSize = 300 * 1024^2)
# Allow access to Google Sheets:
gs4_deauth()

# Google Sheets urls:
outbound_url  = "https://docs.google.com/spreadsheets/d/1hBLcuMiPDyy-z0av1d-gKaRKewkQZYnoxYBc3nZTIsw/edit?gid=0#gid=0"
# CQS exclusion list - read here so the app no longer has to hit Google Sheets live:
exclusion_url = "https://docs.google.com/spreadsheets/d/1Ms_HFZN7NYLNv-zJSHDemNiW9TMtLTgb_brHs6FI8CY/edit?gid=0#gid=0"

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' [ =============== GET DATA =============== ]

# The master column list, in the order the app expects. Used to (a) backfill any
# column a tab happens not to have, so the cleaning step never errors, and (b)
# order the final output by name instead of by position.
master_cols = c(
  "Campaign", "Username", "Profile ID", "Email Ban",
  "Account Manager(VIP) / Black List", "VIP Category", "Type", "Value",
  "Agent's Gender", "Agent", "Date of Call", "Time of Call", "Response",
  "Attitude", "Reason", "Promo Code Credited", "Comments", "Feedback", "Loyalty",
  "Agent 2nd attempt", "Response 2nd attempt", "Attitude 2nd attempt", "Reason 2nd attempt"
)

# Columns forced to text before binding. Cells with mixed types come back from
# read_sheet() as list-columns; flattening them here keeps bind_rows() happy.
char_cols = c("Username", "Comments", "Profile ID", "Date of Call", "Time of Call",
              "Promo Code Credited", "Feedback", "Loyalty",
              "VIP Category", "Type")   # flattened so the swap repair below is safe

# The minimum a tab must contain to count as campaign data. Anything missing these
# (the "1" template tab, Data / Black List helper tabs, pivots, scratch tabs, ...)
# is skipped automatically, so we can safely read EVERY worksheet.
required_cols = c("Campaign", "Profile ID", "Response", "Date of Call")

# Flatten one (possibly list-) column to a plain character vector.
to_text = function(x) {
  map_chr(x, function(v) if (length(v) == 0 || is.null(v)) NA_character_ else as.character(v[[1]]))
}

# Normalise Agent's Gender to exactly M / F / -. Agents on a Greek keyboard type a
# Greek capital Mu ("Μ"), which LOOKS identical to Latin "M" but is a
# different character, so it slips through as an out-of-domain value; this maps it
# (and lower-case / stray typos) back. Anything that is not clearly M or F becomes
# "-" rather than being guessed at. This only fixes keyboard/entry noise - it
# cannot recover a gender that a column shift stranded in the wrong column; that
# has to be fixed in the source worksheet.
normalise_gender = function(x) {
  g      = enc2utf8(as.character(x))
  mu_cap = enc2utf8(intToUtf8(0x039C))   # Greek capital Mu  (Μ)
  mu_low = enc2utf8(intToUtf8(0x03BC))   # Greek small   mu  (μ)
  # Byte-wise replace, so it works no matter the server's locale (toupper() on its
  # own does not touch Greek letters under a C locale).
  g = gsub(mu_cap, "M", g, useBytes = TRUE)
  g = gsub(mu_low, "M", g, useBytes = TRUE)
  g = toupper(trimws(g))
  ifelse(g %in% c("M", "F"), g, "-")
}

# The two value families that VIP Category and Type draw from. By convention
# VIP Category holds the TIER and Type holds the PLAYER TYPE, but some worksheet
# blocks were pasted with the two adjacent columns reversed (tier in Type, player
# type in VIP Category). These sets, compared upper-cased, both (a) drive the
# automatic swap-back below and (b) let the data-quality guard flag anything the
# swap could not safely fix. Kept here as the single source of truth for both.
vip_tier_values = c("NON VIP", "BRONZE", "SILVER", "GOLD", "ELITE", "POTENTIAL")
vip_player_type_values = c(
  "CASINO PLAYER", "CASINO & SPORTSBOOK", "SPORTSBOOK", "UNDER REVIEW", "HIGH ROLLER",
  "FUN SPORTSBOOK", "CASINO CLUB", "POKER & CASINO & SB", "TIPPING LINE", "INDUSTRY",
  "POKER PLAYER", "POKER & CASINO", "POKER & SB", "LATE PUNTER", "MONITORED",
  "VIRTUAL SPORTS PLAYER", "ARB", "SUSPICIOUS"
)

# Function that imports ONE worksheet by name.
load_sheet = function(sheet_name) {
  df = tryCatch(
    suppressMessages(read_sheet(outbound_url, sheet = sheet_name)),
    error = function(e) { message("  ! could not read '", sheet_name, "': ", conditionMessage(e)); NULL }
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Normalise header whitespace ("Reason " -> "Reason", "Profile ID " -> "Profile ID")
  # so the name-based logic below is not tripped up by stray spaces.
  names(df) = trimws(names(df))
  
  # Skip tabs that are not campaign data.
  if (!all(required_cols %in% names(df))) {
    message("Skipped  '", sheet_name, "'  [not a campaign data tab]")
    return(NULL)
  }
  
  # Row number within this sheet, captured BEFORE any row is dropped, so each row
  # keeps its original position in the tab (`Source Tab` records which tab it is).
  df = df %>% mutate(`Source Tab` = sheet_name, `Sheet Row` = row_number())
  
  # --- OPTIONAL: restore the old World Cup prefix, now matched by tab name ---
  # if (sheet_name == "World Cup 2026") df = df %>% mutate(Campaign = paste0("WC 2026 - ", Campaign))
  
  df = df %>%
    mutate(across(any_of(char_cols), to_text)) %>%
    # Drop repeated header rows AND blank spill-over rows (the alignment formula
    # fills whole columns, so empty trailing rows arrive with no Username).
    filter(!is.na(Username), Username != "Username")
  
  message("Loaded   '", sheet_name, "'  [", nrow(df), " rows, ",
          n_distinct(df$Campaign), " campaign(s)]")
  Sys.sleep(0.5)
  df
}

# Read EVERY worksheet in the workbook. New campaigns and new tabs are picked up
# automatically, and a tab that holds several campaigns keeps every one of them,
# because the campaign name always comes from each row's own Campaign cell -
# never from a per-tab default.
all_tabs = sheet_names(outbound_url)
message("Workbook has ", length(all_tabs), " tab(s). Reading campaign tabs...")

raw_outbound_data = suppressWarnings(
  map(all_tabs, load_sheet) %>% bind_rows()
)

# Backfill any master column that no tab provided, so the cleaning step below can
# reference every column by name without failing.
for (col in setdiff(master_cols, names(raw_outbound_data))) {
  raw_outbound_data[[col]] = NA
}

# --- Repair VIP Category <-> Type column swaps -------------------------------
# A row is unambiguously reversed only when BOTH are wrong in the complementary
# way: VIP Category holds a player-type AND Type holds a tier. On exactly those
# rows we swap the two values back. Rows where only one side looks off are left
# alone (the swap would not be safe) and the guard reports them instead.
.vip_u  = toupper(trimws(as.character(raw_outbound_data[["VIP Category"]])))
.type_u = toupper(trimws(as.character(raw_outbound_data[["Type"]])))
vip_type_swap_mask = (.vip_u %in% vip_player_type_values) & (.type_u %in% vip_tier_values)
vip_type_swap_mask[is.na(vip_type_swap_mask)] = FALSE
n_vip_type_swapped = sum(vip_type_swap_mask)
vip_type_swap_tabs = if (n_vip_type_swapped > 0)
  sort(table(raw_outbound_data[["Source Tab"]][vip_type_swap_mask]), decreasing = TRUE) else NULL

if (n_vip_type_swapped > 0) {
  .tmp = raw_outbound_data[["VIP Category"]][vip_type_swap_mask]
  raw_outbound_data[["VIP Category"]][vip_type_swap_mask] = raw_outbound_data[["Type"]][vip_type_swap_mask]
  raw_outbound_data[["Type"]][vip_type_swap_mask]         = .tmp
  message("VIP Category <-> Type: swapped ", n_vip_type_swapped,
          " reversed row(s) back into place across ", length(vip_type_swap_tabs), " tab(s).")
} else {
  message("VIP Category <-> Type: no reversed rows found.")
}

clean_outbound_data = raw_outbound_data %>%
  mutate(
    .call_date = as.Date(`Date of Call`),                               # parse ONCE
    .call_time = as.POSIXct(str_sub(`Time of Call`, -8), format = "%H:%M:%S")
  ) %>%
  mutate(
    `Email Ban` = ifelse(is.na(`Email Ban`), "NO", toupper(`Email Ban`)),
    `Account Manager(VIP) / Black List` = coalesce(`Account Manager(VIP) / Black List`, "NO"),
    `VIP Category` = coalesce(`VIP Category`, "NON VIP"),
    Type = coalesce(Type, "No Type"),
    `Agent's Gender` = normalise_gender(`Agent's Gender`),   # -> M / F / - only
    Agent = coalesce(Agent, "-"),
    `Date of Call`  = format(.call_date, "%d-%b-%Y"),                    # reuse
    `Month of Call` = format(.call_date, "%Y-%m"),                      # reuse (no dmy)
    `Time of Call`  = if_else(is.na(.call_time), "-", format(.call_time, "%H:%M")),
    Attitude = coalesce(Attitude, "-"),
    Reason = coalesce(Reason, "-"),
    `Promo Code Credited` = coalesce(`Promo Code Credited`, "-"),
    Comments = coalesce(Comments, "-"),
    Loyalty = coalesce(Loyalty, "-"),
    Feedback = coalesce(Feedback, "-"),
    `Agent 2nd attempt` = coalesce(`Agent 2nd attempt`, "-"),
    `Attitude 2nd attempt` = coalesce(`Attitude 2nd attempt`, "-"),
    `Reason 2nd attempt` = coalesce(`Reason 2nd attempt`, "-"),
    Response = toupper(coalesce(Response, "-")),
    `Response 2nd attempt` = toupper(coalesce(`Response 2nd attempt`, "-"))
  ) %>%
  arrange(.call_date, `Time of Call`) %>%                               # sort on the parsed date
  select(
    `Campaign`, `Username`, `Profile ID`, `Email Ban`,
    `Account Manager(VIP) / Black List`, `VIP Category`, `Type`, `Value`,
    `Agent's Gender`, `Agent`, `Date of Call`, `Time of Call`, `Month of Call`,
    `Response`, `Attitude`, `Reason`, `Promo Code Credited`, `Comments`,
    `Feedback`, `Loyalty`, `Agent 2nd attempt`, `Response 2nd attempt`,
    `Attitude 2nd attempt`, `Reason 2nd attempt`,
    `Source Tab`, `Sheet Row`
  )

# Plain CSV - kept for any other consumer / the Download-the-raw use. The app no
# longer reads this; it reads the .rds below.
write_csv(clean_outbound_data, "misc/outbound-clean-data.csv")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' [ =============== DATA-QUALITY GUARD =============== ]

# Tight-domain columns: a value outside the listed set is a strong signal that a
# batch was pasted into that worksheet with its columns misaligned (a "shift") -
# the way the GR NON DEP 09-Oct block put gender values into Email Ban and ban
# values into Agent's Gender. We scan each column PER Source Tab and report any
# out-of-domain values, so a bad import is named in the next run's log and summary
# the following morning instead of being discovered months later. This only
# REPORTS - it never changes the data and never fails the build.
#
# Note: Agent's Gender has already been normalised to M/F/- above, so it acts as a
# tripwire (it should always be clean; a hit means the normaliser itself broke). A
# real column shift also corrupts Email Ban / Response, which are NOT normalised,
# so the shift is still caught there.
response_domain = c("ANSWERED", "UNANSWERED", "NOT CALLED", "INVALID NUMBER",
                    "CANNOT CONNECT TO NUMBER", "ANSWERED BY 3RD",
                    "ANSWERED BY 3RD PERSON", "ΑNSWERED BY 3RD PERSON", "-")
domain_checks = list(
  "Email Ban"            = c("YES", "NO"),
  "Agent's Gender"       = c("M", "F", "-"),
  "Response"             = response_domain,
  "Response 2nd attempt" = response_domain
)

# Collect per-tab findings into one list, shared by the log and the summary.
quality_rows = list()
record_bad = function(col_label, v, bad) {
  if (!any(bad)) return(invisible(NULL))
  tabs = clean_outbound_data[["Source Tab"]][bad]
  vals = v[bad]
  for (tt in unique(tabs)) {
    sel = tabs == tt
    ex  = head(names(sort(table(vals[sel]), decreasing = TRUE)), 3)
    quality_rows[[length(quality_rows) + 1]] <<- data.frame(
      Column = col_label, Tab = tt, Bad = sum(sel),
      Examples = paste(ex, collapse = ", "), stringsAsFactors = FALSE
    )
  }
}

# (a) Tight-domain columns - any value outside the set is likely a column shift.
for (col in names(domain_checks)) {
  v = enc2utf8(toupper(trimws(as.character(clean_outbound_data[[col]]))))
  record_bad(col, v, !is.na(v) & !(v %in% enc2utf8(domain_checks[[col]])))
}

# (b) VIP Category <-> Type. After the auto-swap above, VIP Category should no
# longer hold a player-type and Type should no longer hold a tier. Anything left
# is a row the swap could not safely fix (only one side was wrong) - reported, not
# changed.
vip_u  = toupper(trimws(as.character(clean_outbound_data[["VIP Category"]])))
type_u = toupper(trimws(as.character(clean_outbound_data[["Type"]])))
record_bad("VIP Category (player-type value)", vip_u,  vip_u  %in% vip_player_type_values)
record_bad("Type (tier value)",                type_u, type_u %in% vip_tier_values)

# One line on the auto-swap, whether or not anything was corrected.
swap_note = if (n_vip_type_swapped > 0)
  sprintf("Auto-corrected %d VIP Category <-> Type swap(s) across %d tab(s).",
          n_vip_type_swapped, length(vip_type_swap_tabs)) else
  "No VIP Category <-> Type swaps needed."

message("Data-quality guard: ", swap_note)
if (length(quality_rows) == 0) {
  message("Data-quality guard: all checked columns clean across every tab.")
} else {
  qr = do.call(rbind, quality_rows); qr = qr[order(-qr$Bad), ]
  message("Data-quality guard: ", sum(qr$Bad), " out-of-domain value(s) remaining -")
  for (i in seq_len(nrow(qr)))
    message(sprintf("  %-34s %-30s %6d  (e.g. %s)",
                    qr$Column[i], qr$Tab[i], qr$Bad[i], qr$Examples[i]))
}

# In GitHub Actions, also write the verdict to the run's summary page, so it is
# visible without opening the step log.
gh_summary = Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(gh_summary)) {
  add = function(x = "") cat(x, "\n", sep = "", file = gh_summary, append = TRUE)
  add(paste0("## Data-quality guard - ", format(Sys.Date(), "%Y-%m-%d")))
  add(paste0("- ", swap_note))
  if (length(quality_rows) == 0) {
    add("- OK: Email Ban, Agent's Gender, Response, Response 2nd attempt, VIP Category and Type are clean across every tab.")
  } else {
    qr = do.call(rbind, quality_rows); qr = qr[order(-qr$Bad), ]
    add(paste0("- WARNING: ", sum(qr$Bad),
               " out-of-domain value(s) remaining - likely a column shift in the tab(s) below. Check the source worksheet."))
    add()
    add("| Column | Tab | Bad rows | Examples |")
    add("| --- | --- | --- | --- |")
    for (i in seq_len(nrow(qr)))
      add(sprintf("| %s | %s | %d | %s |", qr$Column[i], qr$Tab[i], qr$Bad[i], qr$Examples[i]))
  }
  add()
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' [ =============== CQS LIST + RDS BUNDLE =============== ]

# CQS exclusion Profile IDs, as trimmed text (matches how the app compares them).
exclusion_cqs_ids = suppressMessages(read_sheet(exclusion_url)) %>%
  select(Account_Number) %>% pull() %>% unlist() %>% as.character() %>% trimws()
message("CQS list: ", length(exclusion_cqs_ids), " Profile IDs")

# One typed bundle the app loads in a single readRDS - far faster than parsing the
# CSV, and it carries the CQS list so the app never has to read Google Sheets live.
# `built_at` feeds the app's "Data last updated" stamp (no GitHub API call needed).
saveRDS(
  list(
    outbound = clean_outbound_data,
    cqs      = exclusion_cqs_ids,
    built_at = Sys.time()
  ),
  "misc/outbound-clean-data.rds"
)
message("Wrote misc/outbound-clean-data.rds")
