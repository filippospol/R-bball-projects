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
              "Promo Code Credited", "Feedback", "Loyalty")

# The minimum a tab must contain to count as campaign data. Anything missing these
# (the "1" template tab, Data / Black List helper tabs, pivots, scratch tabs, ...)
# is skipped automatically, so we can safely read EVERY worksheet.
required_cols = c("Campaign", "Profile ID", "Response", "Date of Call")

# Flatten one (possibly list-) column to a plain character vector.
to_text = function(x) {
  map_chr(x, function(v) if (length(v) == 0 || is.null(v)) NA_character_ else as.character(v[[1]]))
}

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
    `Agent's Gender` = coalesce(`Agent's Gender`, "-"),
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
