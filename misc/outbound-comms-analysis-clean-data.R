# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script creates a .csv file with the clean data for the Outbound Comms app.
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
outbound_url = "https://docs.google.com/spreadsheets/d/1hBLcuMiPDyy-z0av1d-gKaRKewkQZYnoxYBc3nZTIsw/edit?gid=0#gid=0"

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' [ =============== GET DATA =============== ]

# Function that imports data:
load_sheet = function(sheet_num, sheet_name) {
  df = suppressMessages(read_sheet(outbound_url, sheet = sheet_num))
  
  if (sheet_num == 5) {
    df = df %>%
      select(-9) %>%
      rename(`Agent's Gender` = 8)
  }
  
  if (sheet_num == 12) {
    df = df %>% 
      mutate(Campaign = paste0("WC 2026 - ", Campaign))
  }
  
  df = df %>%
    mutate(
      Username = map(Username, as.character),
      Comments = map(Comments, as.character),
      `Profile ID` = map(`Profile ID`, as.character),
      `Date of Call` = map(`Date of Call`, as.character),
      `Time of Call` = map(`Time of Call`, as.character),
      `Promo Code Credited` = map(`Promo Code Credited`, as.character) 
    ) %>%
    unnest(cols = c(Username, Comments, `Profile ID`,`Date of Call`,`Time of Call`,`Promo Code Credited`), keep_empty = TRUE) %>%
    filter(Username != "Username")
  
  # showNotification(paste("✅ Loaded", sheet_name), type = "message", duration = 3)
  print(paste("Successfully loaded", sheet_name))
  Sys.sleep(1)
  return(df)
}

# Function that imports data:

raw_outbound_data = suppressWarnings(
  bind_rows(
    load_sheet(2, "GR HGC"),
    load_sheet(3, "Close Accounts"),
    load_sheet(4, "KYC"),
    load_sheet(5, "GR Non Dep"),
    load_sheet(6, "Self Exclusion"),
    load_sheet(11, "Reactivation 60"),
    load_sheet(7, "VIP Close Account"),
    load_sheet(8, "VIP Day 10 Avg Dep >100"),
    load_sheet(9, "VIP Day 10 Avg Dep <100"),
    load_sheet(10, "VIP Day 30"),
    load_sheet(12, "World Cup 2026")
  )
)

clean_outbound_data = raw_outbound_data %>% 
  mutate(
    `Email Ban` = ifelse(is.na(`Email Ban`), "No", `Email Ban`),
    `Account Manager(VIP) / Black List` = ifelse(is.na(`Account Manager(VIP) / Black List`), "NONE", `Account Manager(VIP) / Black List`)
  ) %>% 
  rename(`VIP Category` = Type, Type = `VIP Category`) %>% 
  mutate(
    `VIP Category` = ifelse(is.na(`VIP Category`), "NON VIP", `VIP Category`),
    Type = ifelse(is.na(Type), "No Type", Type),
    `Agent's Gender` = ifelse(is.na(`Agent's Gender`), "-", `Agent's Gender`),
    Agent = ifelse(is.na(Agent), "-", Agent),
    `Date of Call` = format(as.Date(`Date of Call`), "%d-%b-%Y"),
    `Time of Call`=str_sub(`Time of Call`,-8),
    `Time of Call` = ifelse(is.na(`Time of Call`), "-",
                            format(as.POSIXct(`Time of Call`, format="%H:%M:%S"), "%H:%M")),
    `Month of Call` = format(dmy(`Date of Call`), "%Y-%m"),
    Attitude = ifelse(is.na(Attitude), "-", Attitude),
    Reason = ifelse(is.na(Reason), "-", Reason),
    `Promo Code Credited` = ifelse(is.na(`Promo Code Credited`), "-", `Promo Code Credited`),
    Comments = ifelse(is.na(Comments), "-", Comments),
    Loyalty = ifelse(is.na(Loyalty), "-", Loyalty),
    Feedback = ifelse(is.na(Feedback), "-", Feedback),
    `Agent 2nd attempt` = ifelse(is.na(`Agent 2nd attempt`), "-", `Agent 2nd attempt`),
    `Response 2nd attempt` = ifelse(is.na(`Response 2nd attempt`), "-", `Response 2nd attempt`),
    `Attitude 2nd attempt` = ifelse(is.na(`Attitude 2nd attempt`), "-", `Attitude 2nd attempt`),
    `Reason 2nd attempt` = ifelse(is.na(`Reason 2nd attempt`), "-", `Reason 2nd attempt`),
    Response = ifelse(is.na(Response), "-", Response) %>% toupper(),
    `Response 2nd attempt` = ifelse(is.na(`Response 2nd attempt`), "-", `Response 2nd attempt`) %>% toupper()
  ) %>% 
  filter((`Month of Call` %in% c("2026-05","2026-06", "2026-07"))) %>% 
  select(c(1:12, 24, 13:23))

write_csv(clean_outbound_data,"misc/outbound-clean-data.csv")
