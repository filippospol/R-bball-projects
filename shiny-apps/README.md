#### Shiny apps

The code behind a couple of the R Shiny apps on the [site](https://filippospol.github.io/bball-projects/). They're deployed on shinyapps.io and read their data from this folder over a raw GitHub link, so the data and the app live in the same place.

**Adjusted Era Stats** — [live app](https://filippos-pol.shinyapps.io/adjusted-era-stats/)
Puts scoring and efficiency in the context of the league a player played in, so numbers from different eras line up. The app is `aes-app.R` and it reads `aes-raw-NEW.csv`.

**Modern Playoff Scoring Runs** — [live app](https://filippos-pol.shinyapps.io/modern-playoff-scoring-runs/)
Looks at how scoring runs shape playoff games: when they come, how long they last, and how often one swings the result. The app is `mpsr-app.R` and it reads `mpsr-raw.csv`. `mpsr-get-data.R` is the script that scrapes the playoff history off Basketball Reference and rebuilds that CSV, and `mpsr-logo.png` is the app's logo.

To refresh the playoff data, run `mpsr-get-data.R`, which writes a new `mpsr-raw.csv`, then push it here. The apps pick up the new file the next time they load.
