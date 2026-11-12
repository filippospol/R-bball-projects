# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# All Basketball Reference game ids played on a given calendar date.
# Feed the ids to bbref_game_log() to inspect box scores / player status.
#
#   bbref_gameid_date(2025, 4, 13)   # year, month, day
#
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".bbref_read_defined"))
  source("https://raw.githubusercontent.com/filippospol/bball-projects/refs/heads/main/scraper%20functions/bbref/bbref_helpers.R")

bbref_gameid_date = function(year, month, day) {
  url = glue("https://www.basketball-reference.com/boxscores/?month={month}&day={day}&year={year}")
  bbref_read(url) %>%
    html_elements("a") %>% html_attr("href") %>%
    str_subset("^/boxscores/[0-9]{9}[A-Z]{3}\\.html$") %>%
    str_remove("^/boxscores/") %>% str_remove("\\.html$") %>%
    unique()
}
