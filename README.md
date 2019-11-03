## Scripts for <a href="https://evolving-hockey.com/" target="_blank">Evolving-Hockey.com</a>

<br>
s
### Scraper Walkthrough

The `sc.scrape_pbp` function is used to scrape one or more games from the NHL's publicly available data. A list is returned with data that is requested. 

Example:

``` {r}

## Dependencies
library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
library(lubridate)
library(tidyverse) -- specifically: stringr, readr, tidyr, and dplyr

## Source scraper functions from GitHub
devtools::source_url("https://raw.githubusercontent.com/evolvingwild/evolving-hockey/master/EH_scrape_functions.R")

## Scrape games
pbp_scrape <- sc.scrape_pbp(games = c("2018020001", "2018020002"))
```

#### Function Arguments

games:
- a vector of full NHL game IDs (one or more may be provided)
- example: 2018020001

scrape_type:
- "full": all data returned
- "event_summary": only event summary, rosters, and scratches information returned
- "rosters": only rosters and scratches information returned
- default is "full"

live_scrape:
- FALSE = function adjusts incorrect player & goalie shifts
- TRUE = function does not adjust incorrect player & goalie shifts (this should be used when scraping games that are in progress)
- default is FALSE

verbose:
- TRUE = print the system time for each game scraped
- default is TRUE

sleep:
- time to wait between each game being scraped (in seconds)
- default is 0

<br>

### Full Scrape Example:

``` {r}
## Scrape the first 100 games from the 20182019 regular season

games_vec <- c(as.character(seq(2018020001, 2018020100, by = 1)))

pbp_scrape <- sc.scrape_pbp(games = games_vec)

## Pull out of list
game_info_df_new <-     pbp_scrape$game_info_df       ## game information data
pbp_base_new <-         pbp_scrape$pbp_base           ## main play-by-play data
pbp_extras_new <-       pbp_scrape$pbp_extras         ## extra play-by-play data
player_shifts_new <-    pbp_scrape$player_shifts      ## full player shifts data
player_periods_new <-   pbp_scrape$player_periods     ## player TOI sums per period
roster_df_new <-        pbp_scrape$roster_df          ## roster data
scratches_df_new <-     pbp_scrape$scratches_df       ## scratches data
event_summary_df_new <- pbp_scrape$events_summary_df  ## event summary data
scrape_report <-        pbp_scrape$report             ## scrape report
```

<br>


### Scrape Schedule Example:

``` {r} 
## Get yesterday's schedule
schedule_current <- sc.scrape_schedule(start_date = Sys.Date() - 1, end_date =   Sys.Date() - 1)
```


<br>
<br>

  
  
  
  