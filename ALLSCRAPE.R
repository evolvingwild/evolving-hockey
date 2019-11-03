#################################################################################
#####           ALLSCRAPE LOAD            ||            10/05/18            #####
#################################################################################


## ***  this Script is a slightly modified version of Manny Perry's Dry Scrape NHL scraper
## ***  the original source code can be found here: https://github.com/mannyelk/corsica/blob/master/modules/dryscrape.R


## Dependencies - moved to source
#library(RCurl); library(rjson); #library(dplyr); 
#library(lubridate); library(doMC); library(rvest)



### DRYSCRAPE ###

###############################################################################################################

## Description
# Dryscrape contains all functions and tools related to scraping data for Corsica 
# Dependencies: Rcurl, rjson, dplyr, lubridate, doMC, user_functions, rvest


## Objects
c(20001:21230,
  30111:30117, 30121:30127, 30131:30137, 30141:30147, 30151:30157, 30161:30167, 30171:30177, 30181:30187,
  30211:30217, 30221:30227, 30231:30237, 30241:30247,
  30311:30317, 30321:30327,
  30411:30417
  ) %>% 
  as.character() ->
  ds.all_games

c("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36",
  "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36",
  "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.5.2171.95 Safari/537.36",
  "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36"
  ) -> ds.user_agents

c("season", "game_id", "game_date", "session", 
  "event_index", "game_period", "game_seconds",
  "event_type", "event_description", "event_detail",
  "event_team", "event_player_1", "event_player_2", "event_player_3",
  "event_length", "coords_x", "coords_y", "players_substituted",
  "home_on_1", "home_on_2", "home_on_3", "home_on_4", "home_on_5", "home_on_6", 
  "away_on_1", "away_on_2", "away_on_3", "away_on_4", "away_on_5", "away_on_6", 
  "home_goalie", "away_goalie", "home_team", "away_team",
  "home_skaters", "away_skaters", "home_score", "away_score",
  "game_score_state", "game_strength_state", "highlight_code"
  ) -> ds.pbp_colnames

c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ",
  "CGY", "CHI", "COL", "DAL", "DET", "EDM",
  "FLA", "L.A", "MIN", "MTL", "N.J", "NSH",
  "NYI", "NYR", "OTT", "PHI", "PIT", "S.J",
  "STL", "T.B", "TOR", "VAN", "WPG", "WSH",
  "PHX", "ATL", "VGK", "L.V"
  ) -> ds.team_list

data.frame(event = c("FAC", "HIT", "GvTk", "GOAL", "SHOT", "MISS", "BLOCK", "PENL",
                     "STOP", "PRDY", "PSTR", "PEND", "PERD", "SOC", "GEnd", "SOut",
                     "error", "TAKE", "GIVE", "early intermission", "nothing", "nothing"
                     ),
code = as.character(c(502, 503, 504, 505, 506, 507, 508, 509,
                      516, 517, 518, 519, 520, 521, 522, 0, 
                      9999, 1401, 1402, -2147483648, 1, 5)
                    )) -> ds.espn_codes


## Meta Functions
# Get PBP
ds.get_pbp <- function(season, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_pbp() imports the PBP page corresponding to a given year and game ID and returns a list object
  
  url <- paste("http://www.nhl.com/scores/htmlreports/",
               as.character(season),
               "/PL0",
               as.character(game_id),
               ".HTM",
               sep = ""
               )
  
  raw_text <- NULL
  
  while(class(raw_text) != "character" & try_tolerance > 0) {
    
    try(
      url %>%
        getURL(header = FALSE,
               .opts = curlOptions(referer = "nhl.com",
                                   verbose = FALSE,
                                   followLocation = TRUE,
                                   useragent = agents[sample(1:length(agents), 1)]
                                   )
               )
      ) -> 
      raw_text
    
    try_tolerance <- try_tolerance - 1
    
    }
  
  html <- read_html(raw_text)
  
  all <- html_nodes(html, "td")
  body <- html_nodes(html, ".bborder")
  full_text <- html_text(all)
  body_text <- html_text(body)
  
  pbp_list <- list(full_text, body_text)
  
  return(pbp_list)
  }

# Get Shifts
ds.get_shifts <- function(season, game_id, venue, source, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_shifts() imports the shift report page corresponding to a given year, game ID and venue and returns a list object
  
  if(tolower(source) == "htm") {
    
    if(tolower(venue) == "home") {
      
      url <- paste("http://www.nhl.com/scores/htmlreports/", 
                   season, 
                   "/TH0", 
                   game_id, 
                   ".HTM", 
                   sep = ""
                   )
      
      
      } 
    
    else if(tolower(venue) == "away") {
      
      url <- paste("http://www.nhl.com/scores/htmlreports/", 
                   season, 
                   "/TV0", 
                   game_id, 
                   ".HTM", 
                   sep = ""
                   )
      
      }
    
    raw_text <- NULL
    
    while(class(raw_text) != "character" & try_tolerance > 0) {
      
      try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "nhl.com",
                                     verbose = FALSE,
                                     followLocation = TRUE,
                                     useragent = agents[sample(1:length(agents), 1)]
                                     )
                 )
        ) ->
        raw_text
      
      try_tolerance <- try_tolerance - 1
      
      }
    
    html <- read_html(raw_text)
    
    outer_text <- html_text(html_nodes(html, ".border"))
    inner_text <- html_text(html_nodes(html, ".bborder"))
    
    shifts_list <- list(outer_text, inner_text)
    
    return(shifts_list)
    
    } 
  
  else if(tolower(source) == "json") {
    
    year <- substr(season, 0, 4)
    
    url <- paste("http://www.nhl.com/stats/rest/shiftcharts?cayenneExp=gameId=",
                 as.character(year),
                 "0",
                 as.character(game_id),
                 sep = ""
                 )
    
    raw_text <- NULL
    json_check <- NULL
    
    while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
      
      try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "nhl.com",
                                     verbose = FALSE,
                                     followLocation = TRUE,
                                     useragent = agents[sample(1:length(agents), 1)]
                                     )
                 )
        ) ->
        raw_text
      
      json_check <- try(fromJSON(raw_text), silent = TRUE)
      
      try_tolerance <- try_tolerance - 1
      
      }
    
    raw_json <- try(fromJSON(raw_text), silent = TRUE)
    
    if(class(raw_json) == "try-error") {raw_json <- NULL}
    
    return(raw_json)
    
    }
  
  }

# Get Roster
ds.get_roster <- function(season, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_roster() imports the Roster page corresponding to a given year and game ID and returns a character vector
  
  url <- paste("http://www.nhl.com/scores/htmlreports/",
               as.character(season),
               "/RO0",
               as.character(game_id),
               ".HTM",
               sep = ""
               )
  
  raw_text <- NULL
  
  while(class(raw_text) != "character" & try_tolerance > 0) {
    
    try(
      url %>%
        getURL(header = FALSE,
               .opts = curlOptions(referer = "nhl.com",
                                   verbose = FALSE,
                                   followLocation = TRUE,
                                   useragent = agents[sample(1:length(agents), 1)]
                                   )
               )
      ) ->
      raw_text
    
    try_tolerance <- try_tolerance - 1
    
    }
  
  html <- read_html(raw_text)
  
  all <- html_nodes(html, "tr")
  full_text <- html_text(all)
  
  return(full_text)
  
  }

# Get Highlights
ds.get_highlights <- function(season, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_highlights() imports the highlights page corresponding to a given year and game ID and returns a JSON list object
  
  url <- paste("http://live.nhle.com/GameData/",
               as.character(season),
               "/",
               substr(as.character(season), 0, 4),
               "0",
               as.character(game_id),
               "/gc/gcgm.jsonp",
               sep = ""
               )
  
  raw_text <- NULL
  json_check <- NULL
  
  while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
    
    try(
      url %>%
        getURL(header = FALSE,
               .opts = curlOptions(referer = "nhl.com",
                                   verbose = FALSE,
                                   followLocation = TRUE,
                                   useragent = agents[sample(1:length(agents), 1)]
                                   )
               )
      ) ->
      raw_text
    
    clean_text <- gsub("^.+?\\(\\{", "\\{", raw_text)
    json_check <- try(fromJSON(clean_text), silent = TRUE)
    
    try_tolerance <- try_tolerance - 1
    
    }
  
  clean_text <- gsub("^.+?\\(\\{", "\\{", raw_text)
  
  raw_json <- try(fromJSON(clean_text), silent = TRUE)
  
  if(class(raw_json) == "try-error") {raw_json <- NULL}
  
  return(raw_json)
  
  }

# Get Coordinates
ds.get_coordinates <- function(season, game_id, source, date, away_team, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_coordinates() imports the event coordinates corresponding to a given year and game ID and returns a list object
  
  if(tolower(source) == "espn") {
    
    day <- gsub("-", "", as.character(date))
    
    url <- paste("http://scores.espn.go.com/nhl/scoreboard?date=", 
                 day, 
                 sep = ""
                 )
    
    raw_text <- NULL
    
    while(class(raw_text) != "character" & try_tolerance > 0) {
      
      try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "sports.espn.go.com",
                                     verbose = FALSE,
                                     followLocation = TRUE,
                                     useragent = agents[sample(1:length(agents), 1)]
                                     )
                 )
        ) ->
        raw_text
      
      try_tolerance <- try_tolerance - 1
      
      }
    
    game_ids <- unique(unlist(regmatches(raw_text, gregexpr("gameId=[0-9]+", raw_text))))
    teams <- toupper(gsub("team/_/name/|>|</div>", "", unique(unlist(regmatches(raw_text, gregexpr("team/_/name/[a-zA-Z]+|>(Coyotes|Thrashers)</div>", raw_text))))))
    
    teams[which(teams == "PHX")] <- "ARI"
    teams[which(teams == "TB")] <- "T.B"
    teams[which(teams == "NJ")] <- "N.J"
    teams[which(teams == "SJ")] <- "S.J"
    teams[which(teams == "LA")] <- "L.A"
    teams[which(teams == "COYOTES")] <- "ARI"
    teams[which(teams == "THRASHERS")] <- "ATL"
    
    if (as.numeric(season) < 20110000) {teams[which(teams == "WPG")] <- "ATL"}
    
    matrix(unique(teams), 
           byrow = TRUE, 
           ncol = 2
           ) %>% 
      data.frame() ->
      team_mat
    
    cbind(game_ids, 
          team_mat
          ) %>% 
      data.frame() %>% 
      rename(awayteam = X1, 
             hometeam = X2
             ) ->
      url_match
    
    game_url <- first(as.character(url_match$game_ids[which(as.character(url_match$awayteam) == as.character(away_team) | as.character(url_match$hometeam) == as.character(away_team))]))
    
    url <- paste("http://sports.espn.go.com/nhl/gamecast/data/masterFeed?lang=en&isAll=true&rand=0&", 
                 game_url, 
                 sep = ""
                 )
    
    raw_text <- NULL
    
    while(class(raw_text) != "character" & try_tolerance > 0) {
      
      try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "sports.espn.go.com",
                                     verbose = FALSE,
                                     followLocation = TRUE,
                                     useragent = agents[sample(1:length(agents), 1)]
                                     )
                 )
        ) ->
        raw_text
      
      try_tolerance <- try_tolerance - 1
      
      }
    
    events <- unlist(regmatches(raw_text, gregexpr("<Play.*?/Play>", raw_text)))
    
    if(length(events) > 0) {
      
      do.call(cbind, 
              strsplit(events, "[\\[~]")
              ) %>%
        t() %>%
        data.frame() %>%
        select(5, 3, 4, 6, 7, 11) ->
        event_mat
      
      colnames(event_mat) <- c("event_code",
                               "xcoord",
                               "ycoord",
                               "time",
                               "period",
                               "description"
                               )
      
      event_mat$event_type <- ds.espn_codes$event[match(event_mat$event_code, ds.espn_codes$code)]
      event_mat$seconds <- 1200*(nabs(event_mat$period) - 1) + ds.seconds_from_ms(event_mat$time)
      
      return(event_mat)
      
      } else {return(NULL)}
    
    } else if(tolower(source) == "nhl") {
    
    year <- substr(season, 0, 4)
    
    url <- paste("https://statsapi.web.nhl.com/api/v1/game/",
                 as.character(year),
                 "0",
                 as.character(game_id),
                 "/feed/live?site=en_nhl",
                 sep = ""
                 )
    
    raw_text <- NULL
    json_check <- NULL
    
    while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
      
      try(
        url %>%
          getURL(header = FALSE,
                 .opts = curlOptions(referer = "nhl.com",
                                     verbose = FALSE,
                                     followLocation = TRUE,
                                     useragent = agents[sample(1:length(agents), 1)]
                                     )
                 )
        ) ->
        raw_text
      
      json_check <- try(fromJSON(raw_text), silent = TRUE)
      
      try_tolerance <- try_tolerance - 1
      
      }
    
    raw_json <- try(fromJSON(raw_text), silent = TRUE)
    
    if(class(raw_json) == "try-error") {
      
      return(NULL)
      
      } else {
      
      event_mat <- dcapply(raw_json$liveData$plays$allPlays, 
                           ds.parse_event, 
                           "rbind", 
                           cores = 1
                           )
      
      event_mat$game_id <- na_if_null(nabs(raw_json$gameData$game$pk))
      
      return(event_mat)
      
      }
    }
  }

# Get Team Profile
ds.get_team_profile <- function(team_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_team_profile() imports the team profile page corresponding to a given team ID and returns a JSON list object
  
  url <- paste("https://statsapi.web.nhl.com/api/v1/teams/",
               as.character(team_id),
               sep = ""
               )
  
  raw_text <- NULL
  json_check <- NULL
  
  while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
    
    try(
      url %>%
        getURL(header = FALSE,
               .opts = curlOptions(referer = "nhl.com",
                                   verbose = FALSE,
                                   followLocation = TRUE,
                                   useragent = agents[sample(1:length(agents), 1)]
                                   )
               )
      ) ->
      raw_text
    
    json_check <- try(fromJSON(raw_text))
    
    try_tolerance <- try_tolerance - 1
    
    }
  
  raw_json <- try(fromJSON(raw_text))
  
  if(class(raw_json) == "try-error") {raw_json <- NULL}
  
  return(raw_json)
  
  }

# Get Player Profile
ds.get_player_profile <- function(player_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_player_profile() imports the player profile page corresponding to a given player ID and returns a JSON list object
  
  url <- paste("https://statsapi.web.nhl.com/api/v1/people/",
               as.character(player_id),
               sep = ""
               )
  
  raw_text <- NULL
  json_check <- NULL
  
  while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
    
    try(
      url %>%
        getURL(header = FALSE,
               .opts = curlOptions(referer = "nhl.com",
                                   verbose = FALSE,
                                   followLocation = TRUE,
                                   useragent = agents[sample(1:length(agents), 1)]
                                   )
               )
      ) ->
      raw_text
    
    json_check <- try(fromJSON(raw_text))
    
    try_tolerance <- try_tolerance - 1
    
    }
  
  raw_json <- try(fromJSON(raw_text))
  
  if(class(raw_json) == "try-error") {raw_json <- NULL}
  
  return(raw_json)
  
  }

# Get Schedule
ds.get_schedule <- function(start, end, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # get_schedule() imports the schedule page corresponding to a given date range and returns a JSON list object
  
  url <- paste("https://statsapi.web.nhl.com/api/v1/schedule?startDate=",
               as.character(start),
               "&endDate=",
               as.character(end),
               sep = ""
               )
  
  raw_text <- NULL
  json_check <- NULL
  
  while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
    
    try(
      url %>%
        getURL(header = FALSE,
               .opts = curlOptions(referer = "nhl.com",
                                   verbose = FALSE,
                                   followLocation = TRUE,
                                   useragent = agents[sample(1:length(agents), 1)]
                                   )
               )
      ) ->
      raw_text
    
    json_check <- try(fromJSON(raw_text))
    
    try_tolerance <- try_tolerance - 1
    
    }
  
  raw_json <- try(fromJSON(raw_text))
  
  if(class(raw_json) == "try-error") {raw_json <- NULL}
  
  return(raw_json)
  
  }

# Parse PBP Event
ds.parse_event <- function(x) {
  
  ## Description
  # parse_event() parses a single event from the PBP JSON object and returns a data frame
  
  x$players %>%
    sapply(function(p) as.character(p$player$id)) %>%
    unlist() %>%
    c(rep(NA, 
          times = (4 - length(x$players))
          )
      ) ->
    player_ids
  
  data.frame(game_date = NA,
             game_id = NA,
             season = NA,
             session = NA,
             event_id = na_if_null(nabs(x$about$eventIdx)),
             event_code = na_if_null(as.character(x$result$eventCode)),
             event_type = na_if_null(as.character(x$result$eventTypeId)),
             event_description = na_if_null(as.character(x$result$description)),
             event_detail = na_if_null(as.character(x$result$secondaryType)),
             datetime = na_if_null(as.character(parse_date_time(x$about$dateTime, "y-m-d.H:M:S."))),
             game_period = na_if_null(nabs(x$about$period)),
             period_time_elapsed = na_if_null(as.character(x$about$periodTime)),
             period_time_remaining = na_if_null(as.character(x$about$periodTimeRemaining)),
             event_team = na_if_null(as.character(x$team$id)),
             event_player_1 = na_if_null(player_ids[1]),
             event_player_2 = na_if_null(player_ids[2]),
             event_player_3 = na_if_null(player_ids[3]),
             event_player_4 = na_if_null(player_ids[4]),
             coords_x = na_if_null(x$coordinates$x),
             coords_y = na_if_null(x$coordinates$y),
             highlight_id = na_if_null(nabs(x$about$eventId))
             ) ->
    event_df
  
  return(event_df)
  
  }

# Parse Highlight
ds.parse_highlight <- function(x) {
  
  ## Description
  # parse_highlight() parses a single highlight from the Highlights JSON object and returns a data frame
  
  data.frame(game_date = NA,
             game_id = NA,
             season = NA,
             session = NA,
             event_id = na_if_null(x$id),
             highlight_id = na_if_null(x$feeds[[1]]$neulionId),
             event_team_1 = na_if_null(x$t1),
             event_team_2 = na_if_null(x$t2),
             event_period = na_if_null(x$p),
             event_seconds = na_if_null(x$sip),
             event_type = na_if_null(x$type)
             ) ->
    highlight_df
  
  return(highlight_df)
  
}

# Parse Game
ds.parse_game <- function(x) {
  
  ## Description
  # parse_game() parses a single game from the Schedule >> Date JSON object and returns a data frame
  # parse_game() is an inner function for parse_date()
  
  data.frame(game_id = na_if_null(nabs(x$gamePk)),
             game_date = na_if_null(as.character(as.Date(x$gameDate))),
             season = na_if_null(as.character(x$season)),
             session = na_if_null(as.character(x$gameType)),
             game_status = na_if_null(as.character(x$status$detailedState)),
             away_team_id = na_if_null(nabs(x$teams$away$team$id)),
             home_team_id = na_if_null(nabs(x$teams$home$team$id)),
             game_venue = na_if_null(as.character(x$venue$name)),
             game_datetime = na_if_null(as.character(parse_date_time(x$gameDate, "y-m-d.H:M:S.")))
             ) ->
    game_df
  
  return(game_df)
  
}

# Parse Date
ds.parse_date <- function(x) {
  
  ## Description
  # parse_date() parses a single date from the Schedule JSON object and returns a data frame
  # parse_date() uses an inner function parse_game()
  
  date_df <- dcapply(x$games,
                     ds.parse_game,
                     "rbind",
                     cores = 1
                     )
  
  return(date_df)
  
  }

# Parse Player
ds.parse_player <- function(x) {
  
  ## Description
  # parse_player() parses a single player from the PBP JSON object and returns a data frame
  
  data.frame(player_id = x$person$id,
             player_name = x$person$fullName,
             player_number = x$jerseyNumber,
             position = x$position$code
             ) ->
    player_df
  
  return(player_df)
  
  }

# Seconds from MS
ds.seconds_from_ms <- function(ms) {
  
  ## Description
  # seconds_from_ms() returns a numeric vector of representation in seconds of a given vector in M:S format
  
  strsplit(as.character(ms), ":") %>%
    unlist() %>%
    nabs() %>%
    matrix(ncol = 2,
           byrow = TRUE
           ) ->
    time_mat
  
  seconds <- 60*time_mat[, 1] + time_mat[, 2]
  
  return(seconds)
  
  }

# Clean Nums
ds.clean.nums <- function(x) {
  
  ## Description
  # clean_nums() returns a list of player number identifiers for a given event description
  
  t <- gsub("#|ONGOAL - ", "", as.character(unlist(x)))
  t2 <- list(c(t, rep(NA, times = (3 - length(t)))))
  return(t2)
  
  }

#Parse Shifts
ds.parse_shifts <- function(player, venue, inner, outer) {
  
  ## Description
  # parse_shifts() returns a matrix containing shift information for a single player 
  
  if(tolower(venue) == "home") {
    
    index <- which(outer[-1] == player)
    
    inner[which(inner == "Shift #" | inner == "Présence #Shift #")[index]:(which(inner == "SHF" | inner == "PR/SHF")[index]-3)] %>%
      matrix(ncol = 6,
             byrow = TRUE
             ) %>%
      data.frame() %>%
      mutate(num_first_last = player,
             venue = venue
             ) %>%
      filter(X2 != "Per") %>%
      data.frame() ->
      shift_mat
    
    } else if(tolower(venue) == "away") {
    
    index <- which(outer[-1] == player)
    
    inner[which(inner == "Shift #" | inner == "Présence #Shift #")[index]:(which(inner == "SHF" | inner == "PR/SHF")[index]-3)] %>%
      matrix(ncol = 6,
             byrow = TRUE
             ) %>%
      data.frame() %>%
      mutate(num_first_last = player,
             venue = venue
             ) %>%
      filter(X2 != "Per") %>%
      data.frame() ->
      shift_mat
    
    }
  
  return(shift_mat)
  
  }

# Parse Shift
ds.parse_shift <- function(x) {
  
  ## Description
  # parse_shift() parses a single shift from the Shifts JSON object and returns a data frame
  
  data.frame(game_date = NA,
             game_id = na_if_null(nabs(x$gameId)),
             season = NA,
             session = NA,
             shift_number = na_if_null(nabs(x$eventNumber)),
             shift_period = na_if_null(nabs(x$period)),
             shift_start = na_if_null(as.character(x$startTime)),
             shift_end = na_if_null(as.character(x$endTime)),
             shift_duration = na_if_null(as.character(x$duration)),
             team = na_if_null(as.character(x$teamAbbrev)),
             player_id = na_if_null(as.character(x$playerId)),
             player_name_fist = na_if_null(as.character(x$firstName)),
             player_name_last = na_if_null(as.character(x$lastName))
             ) ->
    shift_df
  
  return(shift_df)
  
  }

# Is On
ds.is_on <- function(player, pbp, venue) {
  
  ## Description
  # is_on() returns a numeric vector indicating 1 if a given player is on ice during the event corresponding to the \
  # row index in the given PBP pbject
  
  regex <- paste(player, 
                 ",|",
                 player,
                 "$",
                 sep = ""
                 )
  
  if(venue == "Home") {
    
    data.frame(cumsum(1*(grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "ON" & pbp$event_team == pbp$home_team) - 
                        1*(grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "OFF" & pbp$event_team == pbp$home_team)
                      )
               ) ->
      is_on
    
    } else if(venue == "Away") {
    
    data.frame(cumsum(1*(grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "ON" & pbp$event_team == pbp$away_team) - 
                        1*(grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "OFF" & pbp$event_team == pbp$away_team)
                      )
               ) ->
        is_on
      }
  
  colnames(is_on) <- player
  
  return(is_on)
  
  }

# Is On
ds.find_goalie <- function(players, roster) {
  
  ## Description
  # find_goalie() returns a vector containing all goaltenders in a given player vector
  
  index <- which(players %in% roster$team_num[which(roster$player_position == "G")])
  goalie <- na_if_null(players[index])
  
  return(goalie)
  
  }

# Fix Names
ds.fix_names <- function(name_vect) {
  
  ## Description
  # fix_names() returns a vector of player names corrected for multiple spelling variants
  
  name_vect[which(name_vect == "PK.SUBBAN" | name_vect == "P.K.SUBBAN")] <- "P.K..SUBBAN"
  name_vect[which(name_vect == "TJ.OSHIE" | name_vect == "T.J.OSHIE")] <- "T.J..OSHIE"
  name_vect[which(name_vect == "BJ.CROMBEEN" | name_vect == "B.J.CROMBEEN" | name_vect == "BRANDON.CROMBEEN")] <- "B.J..CROMBEEN"
  name_vect[which(name_vect == "ILJA.BRYZGALOV")] <- "ILYA.BRYZGALOV"
  name_vect[which(name_vect == "CAMERON.BARKER")] <- "CAM.BARKER"
  name_vect[which(name_vect == "CHRIS.VANDE VELDE")] <- "CHRIS.VANDEVELDE"
  name_vect[which(name_vect == "DANIEL.CARCILLO")] <- "DAN.CARCILLO"
  name_vect[which(name_vect == "DANIEL.CLEARY")] <- "DAN.CLEARY"
  name_vect[which(name_vect == "DANIEL.GIRARDI")] <- "DAN.GIRARDI"
  name_vect[which(name_vect == "DAVID JOHNNY.ODUYA")] <- "JOHNNY.ODUYA"
  name_vect[which(name_vect == "DAVID.BOLLAND")] <- "DAVE.BOLLAND"
  name_vect[which(name_vect == "DWAYNE.KING")] <- "DJ.KING"
  name_vect[which(name_vect == "EVGENII.DADONOV")] <- "EVGENY.DADONOV"
  name_vect[which(name_vect == "FREDDY.MODIN")] <- "FREDRIK.MODIN"
  name_vect[which(name_vect == "HARRISON.ZOLNIERCZYK")] <- "HARRY.ZOLNIERCZYK"
  name_vect[which(name_vect == "J P.DUMONT" | name_vect == "JEAN-PIERRE.DUMONT")] <- "J-P.DUMONT"
  name_vect[which(name_vect == "JEAN-FRANCOIS.JACQUES")] <- "J-F.JACQUES"
  name_vect[which(name_vect == "JONATHAN.AUDY-MARCHESSAULT")] <- "JONATHAN.MARCHESSAULT"
  name_vect[which(name_vect == "JOSHUA.HENNESSY")] <- "JOSH.HENNESSY"
  name_vect[which(name_vect == "KRISTOPHER.LETANG")] <- "KRIS.LETANG"
  name_vect[which(name_vect == "KRYSTOFER.BARCH")] <- "KRYS.BARCH"
  name_vect[which(name_vect == "MARTIN.ST LOUIS")] <- "MARTIN.ST. LOUIS"
  name_vect[which(name_vect == "MATTHEW.CARLE")] <- "MATT.CARLE"
  name_vect[which(name_vect == "MATTHEW.DUMBA")] <- "MATT.DUMBA"
  name_vect[which(name_vect == "JOSEPH.CORVO")] <- "JOE.CORVO"
  name_vect[which(name_vect == "TOBY.ENSTROM")] <- "TOBIAS.ENSTROM"
  name_vect[which(name_vect == "MICHAEL.SANTORELLI")] <- "MIKE.SANTORELLI"
  name_vect[which(name_vect == "MICHAEL.CAMMALLERI")] <- "MIKE.CAMMALLERI"
  name_vect[which(name_vect == "MICHAEL.FERLAND")] <- "MICHEAL.FERLAND"
  name_vect[which(name_vect == "PIERRE.PARENTEAU" | name_vect == "PIERRE-ALEX.PARENTEAU" | name_vect == "PA.PARENTEAU" | name_vect == "P.A.PARENTEAU" | name_vect == "P-A.PARENTEAU")] <- "P.A..PARENTEAU"
  name_vect <- gsub("ALEXANDER.|ALEXANDRE.", "ALEX.", name_vect)
  name_vect <- gsub("CHRISTOPHER.", "CHRIS.", name_vect)
  name_vect[which(name_vect == "NICOLAS.PETAN")] <- "NIC.PETAN"
  name_vect[which(name_vect == "NIKOLAI.KULEMIN")] <- "NIKOLAY.KULEMIN"
  name_vect[which(name_vect == "MATTHEW.BENNING")] <- "MATT.BENNING"
  name_vect[which(name_vect == "JAMES.HOWARD")] <- "JIMMY.HOWARD"
  name_vect[which(name_vect == "EMMANUEL.FERNANDEZ")] <- "MANNY.FERNANDEZ"
  name_vect[which(name_vect == "EMMANUEL.LEGACE")] <- "MANNY.LEGACE"
  name_vect[which(name_vect == "SIMEON.VARLAMOV")] <- "SEMYON.VARLAMOV"
  name_vect[which(name_vect == "MAXIME.TALBOT")] <- "MAX.TALBOT"
  name_vect[which(name_vect == "MITCHELL.MARNER")] <- "MITCH.MARNER"
  name_vect[which(name_vect == "ANDREW.MILLER")] <- "DREW.MILLER"
  name_vect[which(name_vect == "EDWARD.PURCELL")] <- "TEDDY.PURCELL"
  name_vect[which(name_vect == "NICKLAS.GROSSMAN")] <- "NICKLAS.GROSSMANN"
  
  return(name_vect)
  
  }


## General Functions
# Who
ds.who <- function(player_id) {
  
  ## Description
  # who() searches a given player ID and returns the player's full name
  
  player <- ds.get_player_profile(player_id)
  
  full_name <- player$people[[1]]$fullName
  
  return(full_name)
  
  }

# Scrape Team Profile
ds.scrape_team_profile <- function(team_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # scrape_team_profile() collects and parses the data for a team corresponsing to a given team ID
  # A data frame is returned
  
  team_id_ <- nabs(team_id)
  
  team <- ds.get_team_profile(team_id_, try_tolerance, agents)
  
  data.frame(team_id = na_if_null(nabs(team$teams[[1]]$id)),
             team_name = na_if_null(team$teams[[1]]$name),
             team_alias = na_if_null(team$teams[[1]]$abbreviation),
             team_venue = na_if_null(team$teams[[1]]$venue$name),
             team_location = na_if_null(team$teams[[1]]$locationName),
             team_city = na_if_null(team$teams[[1]]$venue$city),
             team_division_id = na_if_null(nabs(team$teams[[1]]$division$id)),
             team_division_name = na_if_null(team$teams[[1]]$division$name),
             team_conference_id = na_if_null(nabs(team$teams[[1]]$conference$id)),
             team_conference_name = na_if_null(team$teams[[1]]$conference$name),
             franchise_id = na_if_null(nabs(team$teams[[1]]$franchiseId)),
             is_active = na_if_null(as.logical(team$teams[[1]]$active))
             ) ->
    team_df
  
  return(team_df)
  
}

# Scrape Player Profile
ds.scrape_player_profile <- function(player_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # scrape_player_profile() collects and parses the data for a player corresponsing to a given player ID
  # A data frame is returned
  
  player_id_ <- nabs(player_id)
  
  player <- ds.get_player_profile(player_id_, try_tolerance, agents)
  
  data.frame(player_id = na_if_null(nabs(player$people[[1]]$id)),
             player_name_first = na_if_null(as.character(player$people[[1]]$firstName)),
             player_name_last = na_if_null(as.character(player$people[[1]]$lastName)),
             player_name_full = na_if_null(as.character(player$people[[1]]$fullName)),
             player_jerseynum = na_if_null(nabs(player$people[[1]]$primaryNumber)),
             player_position = na_if_null(as.character(player$people[[1]]$primaryPosition$code)),
             player_birth_date = na_if_null(as.character(as.Date(player$people[[1]]$birthDate))),
             player_birth_city = na_if_null(as.character(player$people[[1]]$birthCity)),
             player_birth_country = na_if_null(as.character(player$people[[1]]$birthCountry)),
             player_nationality = na_if_null(as.character(player$people[[1]]$nationality)),
             player_height = na_if_null(as.character(player$people[[1]]$height)),
             player_weight = na_if_null(nabs(player$people[[1]]$weight)),
             player_handedness = na_if_null(as.character(player$people[[1]]$shootsCatches)),
             is_active = na_if_null(as.logical(player$people[[1]]$active)),
             is_rookie = na_if_null(as.logical(player$people[[1]]$rookie))
             ) ->
    player_df
  
  return(player_df)
  
  }

# Scrape Schedule
ds.scrape_schedule <- function(start, end, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # scrape_schedule() collects and parses the schedule data for a range corresponsing to a given start and end date
  # A data frame is returned
  
  start_ <- as.character(start); end_ <- as.character(end)
  
  sched <- ds.get_schedule(start_, end_, try_tolerance, agents)
  
  sched_df <- dcapply(sched$dates,
                      ds.parse_date, 
                      "rbind", 
                      cores = 1
                      )
  
  return(sched_df)
  
  }

# Scrape Game
ds.scrape_game <- function(season, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # scrape_game() collects and parses the data for a game corresponsing to a given season and game ID
  # A list object containing c([[1]] = PBP, [[2]] = Shifts, [[3]] = Highlights) is returned
  
  season_ <- as.character(season)
  game_id_ <- as.character(game_id)
  
  pbp <- ds.get_pbp(season_, game_id_, try_tolerance, agents)
  home_shifts <- ds.get_shifts(season_, game_id_, venue = "home", source = "htm", try_tolerance, agents)
  away_shifts <- ds.get_shifts(season_, game_id_, venue = "away", source = "htm", try_tolerance, agents)
  roster <- ds.get_roster(season, game_id_, try_tolerance, agents)
  highlights <- ds.get_highlights(season_, game_id_, try_tolerance, agents)
  
  pbp_full <- pbp[[1]]
  pbp_body <- pbp[[2]]
  
  home_shifts_outer <- home_shifts[[1]]
  home_shifts_inner <- home_shifts[[2]]
  away_shifts_outer <- away_shifts[[1]]
  away_shifts_inner <- away_shifts[[2]]
  
  highlight_df <- dcapply(highlights$video$events,
                          ds.parse_highlight,
                          "rbind",
                          cores = 1
                          )
  
  matrix(pbp_body, 
         byrow = TRUE, 
         ncol = 8
         ) %>% 
    data.frame() %>% 
    filter(X2 != "Per") ->
    pbp_raw
  
  highlight_df <- dcapply(highlights$video$events,
                          ds.parse_highlight,
                          "rbind",
                          cores = 1
                          )
  
  if(!is.null(pbp_raw) & nrow(pbp_raw) > 0) {
    
    gsub("^[a-zA-Z]*, ", "", pbp_full[grep("^[a-zA-Z]*, ", pbp_full)]) %>% 
      as.Date(format = "%B %d, %Y") %>% 
      first() %>% 
      as.character() ->
      game_date_
    
    game_id_unique <- paste(substr(season_, 0, 4), 
                            "0", 
                            as.character(game_id_),
                            sep = ""
                            )
    
    session_ <- ifelse(nabs(game_id_) > 30000,
                       "P",
                       "R"
                       )
    
    home_team_ <- gsub(" On Ice", "", pbp_body[8])
    away_team_ <- gsub(" On Ice", "", pbp_body[7])
    
    # Removing this
    home_team_[which(home_team_ == "PHX")] <- "ARI" 
    away_team_[which(away_team_ == "PHX")] <- "ARI"
    
    coordinates_df <- ds.get_coordinates(season_, game_id_, source = "espn", date = game_date_, away_team = away_team_, try_tolerance, agents)
    #coordinates_df <- NULL  # to force coordinate to scrape from NHL
    
    if(!is.null(coordinates_df)) {
      
      coordinates_df %>%
        filter(nabs(period) < 5,
               event_type == "GOAL"
               ) %>%
        group_by(seconds) %>%
        summarise(dupes = n()) %>%
        filter(dupes > 1) %>%
        data.frame() ->
        dupe_check
      
      } else {dupe_check <- data.frame()}
    
    if(is.null(coordinates_df) == TRUE | nrow(dupe_check) > 0) {
      
      coordinates_df <- ds.get_coordinates(season_, game_id_, source = "nhl", date = game_date_, away_team = away_team_, try_tolerance, agents)
      
      coordinates_df %>%
        rename(time = period_time_elapsed,
               xcoord = coords_x,
               ycoord = coords_y,
               period = game_period,
               description = event_description
               ) %>%
        mutate(event_code = NA,
               seconds = 1200*(nabs(period) - 1) + ds.seconds_from_ms(time),
               event_type = as.character(event_type)
               ) %>%
        select(event_code,
               xcoord,
               ycoord,
               time,
               period,
               description,
               event_type,
               seconds
               ) %>%
        data.frame() ->
        coordinates_df
      
      coordinates_df$event_type[which(coordinates_df$event_type == "MISSED_SHOT")] <- "MISS"
      coordinates_df$event_type[which(coordinates_df$event_type == "BLOCKED_SHOT")] <- "BLOCK"
      coordinates_df$event_type[which(coordinates_df$event_type == "FACEOFF")] <- "FAC"
      coordinates_df$event_type[which(coordinates_df$event_type == "GIVEAWAY")] <- "GIVE"
      coordinates_df$event_type[which(coordinates_df$event_type == "TAKEAWAY")] <- "TAKE"
      coordinates_df$event_type[which(coordinates_df$event_type == "PENALTY")] <- "PENL"
      
      coordinates_df %>%
        filter(nabs(period) < 5,
               event_type == "GOAL"
               ) %>%
        group_by(seconds) %>%
        summarise(dupes = n()) %>%
        filter(dupes > 1) %>%
        data.frame() ->
        dupe_check
      
      if(nrow(dupe_check) > 0) {coordinates_df <- NULL}
      
      }
    
    pbp_raw %>%
      filter(X4 != "",
             X2 != ""
             ) %>%
      mutate(game_date = game_date_,
             game_id = game_id_unique,
             season = as.character(season_),
             session = session_,
             home_team = home_team_,
             away_team = away_team_,
             time_elapsed = regmatches(X4, regexpr("[0-9]+:[0-9]{2}", X4)),
             game_seconds = 1200*(nabs(X2) - 1) + ds.seconds_from_ms(time_elapsed),
             event_team = unlist(lapply(regmatches(as.character(X6), gregexpr(paste("(^", paste(ds.team_list, collapse = "|^"), ")", sep = ""), as.character(X6))), na_if_null)),
             event_player_1 = unlist(lapply(regmatches(as.character(X6), gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(X6))), ds.clean.nums))[seq(1, 3*length(X6), 3)],
             event_player_2 = unlist(lapply(regmatches(as.character(X6), gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(X6))), ds.clean.nums))[seq(2, 3*length(X6), 3)],
             event_player_3 = unlist(lapply(regmatches(as.character(X6), gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(X6))), ds.clean.nums))[seq(3, 3*length(X6), 3)],
             event_zone = gsub(". [zZ]one", "", unlist(lapply(regmatches(as.character(X6), gregexpr("[a-zA-Z]{3}. [zZ]one", as.character(X6))), na_if_null))),
             event_detail = gsub(",|, |[A-Z]+ |#[0-9]+ |[A-Z]{2,}.", "", unlist(lapply(regmatches(as.character(X6), gregexpr(", [a-zA-Z|-]+,|[A-Z] .+[(].{4,}[)],|[A-Z] .+[(][a-zA-Z]{3,}[)],", as.character(X6))), na_if_null)))
             ) %>%
      rename(game_period = X2,
             event_type = X5,
             event_description = X6
             ) %>%
      select(game_period,
             event_type,
             event_description,
             game_date:event_detail
             ) %>%
      data.frame() ->
      pbp_df
    
    bind_rows(
      pbp_df %>%
        filter(event_type == "FAC") %>%
        mutate(event_player_1 = paste(away_team, event_player_1, sep = ""),
               event_player_2 = paste(home_team, event_player_2, sep = ""),
               event_player_3 = NA
               ),
      
      pbp_df %>%
        filter(event_type %in% c("HIT", "BLOCK", "PENL")) %>%
        group_by(event_team) %>%
        mutate(event_player_1 = paste(first(event_team), event_player_1, sep = ""),
               event_player_2 = paste(unique(c(home_team, away_team))[which(unique(c(home_team, away_team)) != first(event_team))], event_player_2, sep = ""),
               event_player_3 = NA
               ),
      
      pbp_df %>%
        filter(event_type %in% c("SHOT", "MISS", "GIVE", "TAKE")) %>%
        group_by(event_team) %>%
        mutate(event_player_1 = paste(first(event_team), event_player_1, sep = ""),
               event_player_2 = NA,
               event_player_3 = NA
               ),
      
      pbp_df %>%
        filter(event_type %in% c("GOAL")) %>%
        group_by(event_team) %>%
        mutate(event_player_1 = paste(first(event_team), event_player_1, sep = ""),
               event_player_2 = paste(first(event_team), event_player_2, sep = ""),
               event_player_3 = paste(first(event_team), event_player_3, sep = "")
               ),
      
      pbp_df %>%
        filter(event_type %in% c("FAC", "HIT", "BLOCK", "PENL", "SHOT", "MISS", "GIVE", "TAKE", "GOAL") == FALSE) %>%
        data.frame()
      ) %>%
      mutate(event_player_1 = gsub(paste(paste(ds.team_list, collapse = "NA|"), "NA", sep = ""), NA, event_player_1),
             event_player_2 = gsub(paste(paste(ds.team_list, collapse = "NA|"), "NA", sep = ""), NA, event_player_2),
             event_player_3 = gsub(paste(paste(ds.team_list, collapse = "NA|"), "NA", sep = ""), NA, event_player_3)
             ) %>%
      data.frame() ->
      pbp_df
    
    } else {return(list(NULL, NULL, NULL, NULL, NULL))}
  
  if(!is.null(roster)) {
    
    regmatches(as.character(roster[1]), gregexpr("[0-9]+(\\\r\\\n|\\\n)[A-Z]+(\\\r\\\n|\\\n)[A-Z )(-]+(\\\r\\\n|\\\n)", as.character(roster[1]))) %>%
      unlist() %>%
      strsplit("(\\\r\\\n|\\\n)") %>%
      unlist() %>%
      matrix(ncol = 3,
             byrow = TRUE
             ) %>%
      data.frame() %>%
      rename(player_number = X1,
             player_position = X2,
             player_name = X3
             ) ->
      pos_match
    
    pos_match$name_match <- gsub("[^A-Z]|\\([A-Z]+\\)", "", pos_match$player_name)
    
    }
  
  if(!is.null(home_shifts) & !is.null(away_shifts) & length(home_shifts_outer[-1]) > 0 & length(away_shifts_outer[-1]) > 0) {
    
    bind_rows(
      data.frame(team_name = home_shifts_outer[1],
                 team = home_team_,
                 venue = "Home",
                 num_first_last = home_shifts_outer[-1]),
      
      data.frame(team_name = away_shifts_outer[1],
                 team = away_team_,
                 venue = "Away",
                 num_first_last = away_shifts_outer[-1])
      ) %>%
      data.frame() %>%
      filter(grepl("[A-Z0-9]", num_first_last) == TRUE) %>%
      mutate(game_date = game_date_,
             game_id = game_id_unique,
             season = as.character(season_),
             session = session_,
             player_number = unlist(regmatches(as.character(num_first_last), gregexpr("^[0-9]+", as.character(num_first_last)))),
             team_num = paste(team, player_number, sep = "")
             ) %>%
      data.frame() ->
      roster_df
    
    strsplit(gsub("^[0-9]+ ", 
                  "", 
                  roster_df$num_first_last
                  ), 
             ", "
             ) %>%
      unlist() %>%
      as.character() %>%
      matrix(ncol = 2,
             byrow = TRUE
             ) %>%
      data.frame() ->
      name_mat
    
    roster_df$first_name <- name_mat[, 2]
    roster_df$last_name <- name_mat[, 1]
    roster_df$player_name <- paste(roster_df$first_name, roster_df$last_name, sep = ".")
    roster_df$name_match <- gsub("[^A-Z]|\\([A-Z]+\\)", "", roster_df$player_name)
    roster_df$player_position <- pos_match$player_position[match(roster_df$name_match, pos_match$name_match)]
    
    bind_rows(
      do.call(rbind,
              lapply(as.list(home_shifts_outer[-1]),
                     ds.parse_shifts,
                     venue = "Home",
                     outer = home_shifts_outer,
                     inner = home_shifts_inner
                     )
              ) %>%
        data.frame() %>%
        mutate(team = home_team_),
      
      do.call(rbind,
              lapply(as.list(away_shifts_outer[-1]),
                     ds.parse_shifts,
                     venue = "Away",
                     outer = away_shifts_outer,
                     inner = away_shifts_inner
                     )
              ) %>%
        data.frame() %>%
        mutate(team = away_team_)
      ) %>%
      data.frame() %>%
      rename(shift_number = X1,
             game_period = X2,
             shift_start = X3,
             shift_end = X4,
             shift_duration = X5
             ) %>%
      select(shift_number:shift_duration,
             num_first_last,
             team,
             venue
             ) %>%
      mutate(game_date = game_date_,
             game_id = game_id_unique,
             season = as.character(season_),
             session = session_,
             home_team = home_team_,
             away_team = away_team_
             ) %>%
      data.frame() ->
      shifts_df
    
    shifts_df$player_name <- roster_df$player_name[match(shifts_df$num_first_last, roster_df$num_first_last)]
    shifts_df$game_period <- as.character(shifts_df$game_period)
    shifts_df$game_period[which(shifts_df$game_period == "OT")] <- "4"
    shifts_df$team_num <- paste(shifts_df$team, gsub("[^0-9]", "", shifts_df$num_first_last), sep = "")
    
    do.call(rbind,
            strsplit(as.character(shifts_df$shift_start), " / ")
            ) %>%
      data.frame() ->
      start_mat
    
    do.call(rbind,
            strsplit(as.character(shifts_df$shift_end), " / ")
            ) %>%
      data.frame() ->
      end_mat
    
    shifts_df$start_seconds <- 1200*(nabs(shifts_df$game_period) - 1) + ds.seconds_from_ms(start_mat[, 1])
    shifts_df$end_seconds <- 1200*(nabs(shifts_df$game_period) - 1) + ds.seconds_from_ms(end_mat[, 1])
    
    shifts_df$end_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] <- shifts_df$start_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] + ds.seconds_from_ms(shifts_df$shift_duration[which(shifts_df$end_seconds < shifts_df$start_seconds)])
    
    shifts_df %>%
      filter(ds.seconds_from_ms(shift_duration) + (start_seconds - 1200*(nabs(game_period) - 1)) <= 1200) %>%
      data.frame() ->
      shifts_df
    
    } else {
    
    shifts <- ds.get_shifts(season_, game_id_, venue = NULL, source = "json", try_tolerance, agents)
    
    shifts_df <- dcapply(shifts$data,
                         ds.parse_shift,
                         "rbind",
                         cores = 1
                         )
    
    if(!is.null(shifts_df)) {
      
      shifts_df %>%
        mutate(game_date = game_date_,
               season = as.character(season_),
               session = session_,
               home_team = home_team_,
               away_team = away_team_
               ) %>%
        data.frame() ->
        shifts_df
      
      year <- substr(season, 0, 4)
      
      url <- paste("https://statsapi.web.nhl.com/api/v1/game/",
                   as.character(year),
                   "0",
                   as.character(game_id),
                   "/feed/live?site=en_nhl",
                   sep = ""
                   )
      
      raw_text <- NULL
      json_check <- NULL
      
      while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
        
        try(
          url %>%
            getURL(header = FALSE,
                   .opts = curlOptions(referer = "nhl.com",
                                       verbose = FALSE,
                                       followLocation = TRUE,
                                       useragent = agents[sample(1:length(agents), 1)]
                                       )
                   )
          ) ->
          raw_text
        
        json_check <- try(fromJSON(raw_text), silent = TRUE)
        
        try_tolerance <- try_tolerance - 1
        
        }
      
      raw_json <- try(fromJSON(raw_text), silent = TRUE)
      
      if(class(raw_json) == "try-error") {raw_json <- NULL}
      
      home_roster <- raw_json$liveData$boxscore$teams$home
      away_roster <- raw_json$liveData$boxscore$teams$away
      
      home_player_data <- dcapply(home_roster$players,
                                  ds.parse_player,
                                  "rbind",
                                  cores = 1
                                  )
      
      away_player_data <- dcapply(away_roster$players,
                                  ds.parse_player,
                                  "rbind",
                                  cores = 1
                                  )
      
      bind_rows(
        home_player_data %>%
          mutate(team = home_roster$team$abbreviation,
                 team_name = toupper(home_roster$team$name),
                 venue = "Home"
                 ),
        
        away_player_data %>%
          mutate(team = away_roster$team$abbreviation,
                 team_name = toupper(away_roster$team$name),
                 venue = "Away"
                 )
        ) %>%
        data.frame() ->
        player_data
      
      player_data$team_num <- paste(player_data$team, player_data$player_number, sep = "")
      
      name_match <- dcapply(player_data$player_id,
                            ds.scrape_player_profile,
                            "rbind",
                            cores = 1
                            )
      
      player_data %>%
        mutate(first_name = toupper(name_match$player_name_first[match(player_id, name_match$player_id)]),
               last_name = toupper(name_match$player_name_last[match(player_id, name_match$player_id)]),
               num_first_last = NA,
               game_date = game_date_,
               game_id = game_id_unique,
               season = as.character(season_),
               session = session_,
               home_team = home_team_,
               away_team = away_team_,
               player_name = paste(first_name, last_name, sep = "."),
               name_match = gsub("[^A-Z]|\\([A-Z]+\\)", "", player_name),
               player_position = substr(position, 0, 1)
               ) %>%
        select(team_name,
               team,
               venue,
               num_first_last,
               game_date,
               game_id,
               season,
               session,
               player_number,
               team_num,
               first_name,
               last_name,
               player_name,
               name_match,
               player_position
               ) %>%
        data.frame() ->
        roster_df
      
      shifts_df %>%
        rename(game_period = shift_period) %>%
        mutate(num_first_last = NA,
               venue = ifelse(team == home_team_,
                              "Home",
                              "Away"
                              ),
               game_date = game_date_,
               game_id = game_id_unique,
               season = as.character(season_),
               session = session_,
               home_team = home_team_,
               away_team = away_team_,
               player_name = player_data$player_name[match(player_id, player_data$player_id)],
               team_num = player_data$team_num[match(player_id, player_data$player_id)],
               start_seconds = 1200*(nabs(game_period) - 1) + ds.seconds_from_ms(shift_start),
               end_seconds = 1200*(nabs(game_period) - 1) + ds.seconds_from_ms(shift_end)
               ) %>%
        select(shift_number,
               game_period,
               shift_start,
               shift_end,
               shift_duration,
               num_first_last,
               team,
               venue,
               game_date,
               game_id,
               season,
               session,
               home_team,
               away_team,
               player_name,
               team_num,
               start_seconds,
               end_seconds
               ) %>%
        data.frame() ->
        shifts_df
      
      shifts_df$end_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] <- shifts_df$start_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] + ds.seconds_from_ms(shifts_df$shift_duration[which(shifts_df$end_seconds < shifts_df$start_seconds)])
      
      shifts_df %>%
        filter(ds.seconds_from_ms(shift_duration) + (start_seconds - 1200*(nabs(game_period) - 1)) <= 1200) %>%
        data.frame() ->
        shifts_df
      
      } else {return(list(NULL, NULL, NULL, NULL, NULL))}
    
    }
  
  if(!is.null(highlight_df)) {
    
    highlight_df %>%
      filter(nabs(event_period) < 5,
             event_type == 505
             ) %>%
      group_by(event_id) %>%
      summarise(dupes = n()) %>%
      filter(dupes > 1) %>%
      data.frame() ->
      dupe_check
    
    if(nrow(dupe_check) > 0) {
      
      highlight_df <- NULL
      
      } else {
      
      highlight_df %>%
        mutate(game_date = game_date_,
               game_id = game_id_unique,
               season = as.character(season_),
               session = session_,
               home_team = home_team_,
               away_team = away_team_
               ) %>%
        data.frame() ->
        highlight_df
      
        }
    
    }
  
  if(!is.null(coordinates_df)) {
    
    coordinates_df %>%
      mutate(game_date = game_date_,
             game_id = game_id_unique,
             season = as.character(season_),
             session = session_,
             home_team = home_team_,
             away_team = away_team_
             ) %>%
      data.frame() ->
      coordinates_df
    
    }
  
  game_list <- list(pbp_df,
                    roster_df,
                    shifts_df,
                    highlight_df,
                    coordinates_df
                    )
  
  return(game_list)
  
  }

# Compile Games
ds.compile_games <- function(games, season, pause = 1, try_tolerance = 3, 
                             agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
  
  ## Description
  # compile_games() collects, parses and compiles all game data corresponding to a given vector of game IDs and season
  # A list object containing c([[1]] = PBP, [[2]] = Roster, [[3]] = Shifts) is returned
  
  foreach(g = as.character(games)) %do% {
    
    cat(g,
        "...",
        "\n",
        sep = ""
        )
    
    inner <- ds.scrape_game(season, g, try_tolerance, agents)
    Sys.sleep(pause)
    
    return(inner)
    
    } -> nested_games
  
  unpacked <- do.call(Map, c(rbind, nested_games))
  
  pbp <- unpacked[[1]]
  roster <- unpacked[[2]]
  shifts <- unpacked[[3]]
  highlights <- unpacked[[4]]
  coords <- unpacked[[5]]
  
  roster$player_name <- ds.fix_names(roster$player_name)
  shifts$player_name <- ds.fix_names(shifts$player_name)
  
  bind_rows(
    shifts %>%
      filter(!is.na(shift_duration)) %>%
      group_by(game_id,
               game_date,
               season,
               session,
               home_team,
               away_team,
               team,
               game_period,
               start_seconds
               ) %>%
      rename(game_seconds = start_seconds, event_team = team) %>%
      summarise(event_type = "ON",
                players_substituted = paste(unique(team_num), collapse = ", ")
                ) %>%
      data.frame(),
    
    shifts %>%
      filter(!is.na(shift_duration)) %>%
      group_by(game_id,
               game_date,
               season,
               session,
               home_team,
               away_team,
               team,
               game_period,
               end_seconds
               ) %>%
      rename(game_seconds = end_seconds, event_team = team) %>%
      summarise(event_type = "OFF",
                players_substituted = paste(unique(team_num), collapse = ", ")
                ) %>%
      data.frame()
    ) -> shift_summary
  
  if(!is.null(highlights)) {
    
    highlights$event_match <- ifelse(highlights$event_type == 505,
                                     "GOAL",
                                     "SHOT"
                                     )
    
    left_join(pbp,
              highlights %>%
                mutate(game_seconds = 1200*(nabs(event_period) - 1) + nabs(event_seconds)) %>%
                rename(highlight_code = highlight_id) %>%
                select(game_id, game_seconds, event_match, highlight_code) %>%
                data.frame(),
              by = c("game_id" = "game_id", "game_seconds" = "game_seconds", "event_type" = "event_match")
              ) %>%
      data.frame() ->
      new_pbp
    
    } else {
    
    pbp %>%
      mutate(highlight_code = NA
             ) %>%
      data.frame() ->
      new_pbp
    
      }
  
  if(!is.null(coords)) {
    
    left_join(new_pbp,
              coords %>%
                rename(coords_x = xcoord,
                       coords_y = ycoord
                       ) %>%
                select(game_id, seconds, event_type, coords_x, coords_y) %>%
                data.frame(),
              by = c("game_id" = "game_id", "game_seconds" = "seconds", "event_type" = "event_type")
              ) %>%
      data.frame() ->
      new_pbp
    
    } else {
    
    new_pbp %>%
      mutate(coords_x = NA,
             coords_y = NA
             ) %>%
      data.frame() ->
      new_pbp
    
      }
  
  new_pbp %>%
    group_by(game_id, game_seconds, event_description) %>%
    slice(1) %>%
    data.frame() ->
    new_pbp
  
  new_pbp$game_period <- nabs(new_pbp$game_period)
  shift_summary$game_period <- nabs(shift_summary$game_period)
  
  bind_rows(new_pbp,
            shift_summary
            ) %>%
    mutate(priority = 1*(event_type %in% c("TAKE", "GIVE", "MISS", "HIT", "SHOT", "BLOCK")) +
             2*(event_type == "GOAL") +
             3*(event_type == "STOP") +
             4*(event_type == "PENL") +
             5*(event_type == "OFF") +
             6*(event_type == "ON") +
             7*(event_type == "FAC")
           ) %>% 
    group_by(game_id) %>%
    arrange(game_period,
            game_seconds,
            priority
            ) %>%
    mutate(event_index = cumsum(!is.na(game_id))) %>%
    data.frame() ->
    new_pbp
  
  home_on_mat <- dcapply(as.list(unique(shifts$team_num)),
                         ds.is_on,
                         "cbind",
                         cores = 1,
                         pbp = arrange(new_pbp,
                                       game_id,
                                       event_index
                                       ),
                         venue = "Home"
                         )
  
  away_on_mat <- dcapply(as.list(unique(shifts$team_num)),
                         ds.is_on,
                         "cbind",
                         cores = 1,
                         pbp = arrange(new_pbp,
                                       game_id,
                                       event_index
                                       ),
                         venue = "Away"
                         )
  
  which(home_on_mat == 1, 
        arr.ind = TRUE
        ) %>%
    data.frame() %>%
    group_by(row) %>%
    summarise(home_on_1 = colnames(home_on_mat)[unique(col)[1]],
              home_on_2 = colnames(home_on_mat)[unique(col)[2]],
              home_on_3 = colnames(home_on_mat)[unique(col)[3]],
              home_on_4 = colnames(home_on_mat)[unique(col)[4]],
              home_on_5 = colnames(home_on_mat)[unique(col)[5]],
              home_on_6 = colnames(home_on_mat)[unique(col)[6]]
              ) %>%
    data.frame() ->
    home_on_df
  
  which(away_on_mat == 1, 
        arr.ind = TRUE
        ) %>%
    data.frame() %>%
    group_by(row) %>%
    summarise(away_on_1 = colnames(away_on_mat)[unique(col)[1]],
              away_on_2 = colnames(away_on_mat)[unique(col)[2]],
              away_on_3 = colnames(away_on_mat)[unique(col)[3]],
              away_on_4 = colnames(away_on_mat)[unique(col)[4]],
              away_on_5 = colnames(away_on_mat)[unique(col)[5]],
              away_on_6 = colnames(away_on_mat)[unique(col)[6]]
              ) %>%
    data.frame() ->
    away_on_df
  
  do.call(c,
          home_on_df[, -1] %>%
            split(1:nrow(home_on_df)) %>%
            lapply(ds.find_goalie,
                   roster
                   )
          ) %>%
    as.character() ->
    home_goalie
  
  do.call(c,
          away_on_df[, -1] %>%
            split(1:nrow(away_on_df)) %>%
            lapply(ds.find_goalie,
                   roster
                   )
          ) %>%
    as.character() ->
    away_goalie
  
  new_pbp %>%
    arrange(game_id,
            event_index
            ) %>%
    mutate(home_on_1 = NA,
           home_on_2 = NA,
           home_on_3 = NA,
           home_on_4 = NA,
           home_on_5 = NA,
           home_on_6 = NA,
           home_goalie = NA,
           away_on_1 = NA,
           away_on_2 = NA,
           away_on_3 = NA,
           away_on_4 = NA,
           away_on_5 = NA,
           away_on_6 = NA,
           away_goalie = NA
           ) ->
    full_pbp
  
  full_pbp$home_on_1[home_on_df$row] <- home_on_df$home_on_1
  full_pbp$home_on_2[home_on_df$row] <- home_on_df$home_on_2
  full_pbp$home_on_3[home_on_df$row] <- home_on_df$home_on_3
  full_pbp$home_on_4[home_on_df$row] <- home_on_df$home_on_4
  full_pbp$home_on_5[home_on_df$row] <- home_on_df$home_on_5
  full_pbp$home_on_6[home_on_df$row] <- home_on_df$home_on_6
  full_pbp$home_goalie[home_on_df$row] <- home_goalie
  
  full_pbp$away_on_1[away_on_df$row] <- away_on_df$away_on_1
  full_pbp$away_on_2[away_on_df$row] <- away_on_df$away_on_2
  full_pbp$away_on_3[away_on_df$row] <- away_on_df$away_on_3
  full_pbp$away_on_4[away_on_df$row] <- away_on_df$away_on_4
  full_pbp$away_on_5[away_on_df$row] <- away_on_df$away_on_5
  full_pbp$away_on_6[away_on_df$row] <- away_on_df$away_on_6
  full_pbp$away_goalie[away_on_df$row] <- away_goalie
  
  full_pbp$event_player_1 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$event_player_1, sep = "."), 
                                                      paste(roster$game_id, roster$team_num, sep = ".")
                                                      )
                                                ]
  full_pbp$event_player_2 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$event_player_2, sep = "."), 
                                                      paste(roster$game_id, roster$team_num, sep = ".")
                                                      )
                                                ]
  full_pbp$event_player_3 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$event_player_3, sep = "."), 
                                                      paste(roster$game_id, roster$team_num, sep = ".")
                                                      )
                                                ]
  full_pbp$home_on_1 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_on_1, sep = "."), 
                                                 paste(roster$game_id, roster$team_num, sep = ".")
                                                 )
                                           ]
  full_pbp$home_on_2 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_on_2, sep = "."), 
                                                 paste(roster$game_id, roster$team_num, sep = ".")
                                                 )
                                           ]
  full_pbp$home_on_3 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_on_3, sep = "."), 
                                                 paste(roster$game_id, roster$team_num, sep = ".")
                                                 )
                                           ]
  full_pbp$home_on_4 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_on_4, sep = "."), 
                                                 paste(roster$game_id, roster$team_num, sep = ".")
                                                 )
                                           ]
  full_pbp$home_on_5 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_on_5, sep = "."), 
                                                 paste(roster$game_id, roster$team_num, sep = ".")
                                                 )
                                           ]
  full_pbp$home_on_6 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_on_6, sep = "."), 
                                                 paste(roster$game_id, roster$team_num, sep = ".")
                                                 )
                                           ]
  full_pbp$away_on_1 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_on_1, sep = "."), 
                                                 paste(roster$game_id, roster$team_num, sep = ".")
                                                 )
                                           ]
  full_pbp$away_on_2 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_on_2, sep = "."), 
                                                 paste(roster$game_id, roster$team_num, sep = ".")
                                                 )
                                           ]
  full_pbp$away_on_3 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_on_3, sep = "."), 
                                                 paste(roster$game_id, roster$team_num, sep = ".")
                                                 )
                                           ]
  full_pbp$away_on_4 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_on_4, sep = "."), 
                                                 paste(roster$game_id, roster$team_num, sep = ".")
                                                 )
                                           ]
  full_pbp$away_on_5 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_on_5, sep = "."), 
                                                 paste(roster$game_id, roster$team_num, sep = ".")
                                                 )
                                           ]
  full_pbp$away_on_6 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_on_6, sep = "."), 
                                                 paste(roster$game_id, roster$team_num, sep = ".")
                                                 )
                                           ]
  full_pbp$home_goalie <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_goalie, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
                                                   )
                                             ]
  full_pbp$away_goalie <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_goalie, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
                                                   )
                                             ]
  
  full_pbp %>%
    group_by(game_id) %>%
    arrange(event_index) %>%
    mutate(home_skaters = 6 - 1 * (is.na(home_on_1) == TRUE) -
             1 * (is.na(home_on_2) == TRUE) -
             1 * (is.na(home_on_3) == TRUE) -
             1 * (is.na(home_on_4) == TRUE) -
             1 * (is.na(home_on_5) == TRUE) -
             1 * (is.na(home_on_6) == TRUE) -
             1 * (!is.na(home_goalie)),
           away_skaters = 6 - 1 * (is.na(away_on_1) == TRUE) -
             1 * (is.na(away_on_2) == TRUE) -
             1 * (is.na(away_on_3) == TRUE) -
             1 * (is.na(away_on_4) == TRUE) -
             1 * (is.na(away_on_5) == TRUE) -
             1 * (is.na(away_on_6) == TRUE) -
             1 * (!is.na(away_goalie)),
           home_score = cumsum(event_type == "GOAL" & event_team == home_team) - 1*(event_type == "GOAL" & event_team == home_team),
           away_score = cumsum(event_type == "GOAL" & event_team == away_team) - 1*(event_type == "GOAL" & event_team == away_team),
           event_length = nabs(lead(game_seconds, 1) - game_seconds)
           ) %>%
    ungroup() %>%
    mutate(game_strength_state = paste(ifelse(is.na(home_goalie) == TRUE,
                                              "E",
                                              home_skaters
                                              ),
                                       ifelse(is.na(away_goalie) == TRUE,
                                              "E",
                                              away_skaters
                                              ),
                                       sep = "v"
                                       ),
           game_score_state = paste(home_score,
                                    away_score,
                                    sep = "v"
                                    )
           ) %>%
    select(one_of(ds.pbp_colnames)) %>%
    arrange(game_id,
            event_index
            ) %>%
    data.frame() ->
    full_pbp
  
  full_pbp$event_team[which(full_pbp$event_team == "PHX")] <- "ARI"
  
  new_game_list <- list(full_pbp,
                        roster,
                        shifts
                        )
  
  return(new_game_list)
  
  }



###############################################################################################################



### USER FUNCTIONS ###

###############################################################################################################

## Description
# User Functions are meta functions and methods for more efficient code writing
# Dependencies: dplyr, doMC


## Dependencies - moved to source
#require(dplyr); require(doMC)


## General Functions
# Numeric Absolute
nabs <- function(x) {
  
  ## Description
  # nabs() returns x after first converting it to class numeric via character
  # Its primary use is converting objects of class factor to numeric
  # It also provides a more concise wrapper for standard numeric conversion
  
  return(as.numeric(as.character(x)))
  
  }

# Logarithmic Loss
log_loss <- function(act, pred, allow_inf = FALSE) {
  
  ## Description
  # log_loss() returns the logarithmic loss obtained from a given prediction and known result
  # The allow_inf parameter controls whether infinite loss is allowed (default is FALSE)
  # Setting allow_inf to FALSE will cause large but finite penalties at the extremes
  
  eps = as.numeric(!allow_inf)*1e-15
  
  pred = matrix(sapply(pred, function(x) max(eps, x)),
                nrow = nrow(pred)
                )      
  pred = matrix(sapply(pred, function(x) min(1 - eps, x)), 
                nrow = nrow(pred)
                )
  
  ll = sum(act*log(pred) + (1 - act)*log(1 - pred))
  ll = -ll/(nrow(act))    
  
  return(ll)
  
  }

# Moving 
moving <- function(x, n = 5) {
  
  ## Description
  # moving() returns a vector of averages obtained from the n elements of x preceding and including the element \
  # at each respective index
  
  if(length(x) < n) {
    
    v <- NA
    
    } else {
    
    stats::filter(x,
                  rep(1/n, n),
                  sides = 1
                  ) -> 
        v
    
    }
  
  return(as.numeric(v))
  
  }

# Brier Score
brier <- function(act, pred) {
  
  ## Description
  # brier() returns the Brier score obtained from a given prediction and known result
  
  bri <- sum((act - pred)^2)/length(act)
  
  return(bri)
  
  }

# NA if NULL
na_if_null <- function(x) {
  
  ## Description
  # na_if_null() returns an object's value if it is not NULL and NA otherwise
  
  return(ifelse(is.null(x) == TRUE,
                NA,
                x)
         )
  
  }

# Do Call Apply
dcapply <- function(x, fun, combine, cores, ...) {
  
  ## Description
  # dcapply() uses do.call() to merge the products of an applied function according to specifications
  # The function will be applied in parallel if cores >= 1
  
  if(cores > 1) {
    
    registerDoMC(cores)
    
    chunks <- split(x, cut(1:length(x), cores))
    
    foreach(i = 1:cores, .combine = c) %dopar% {
      
      chunks[[i]] %>%
        lapply(fun, ...) 
      
      } -> list
    
    combined <- do.call(combine, list)
    
    } else {
    
    
    list <- lapply(x, fun, ...)
    
    combined <- do.call(combine, list)
    
    }
  
  return(combined)
  
  }

# NA as String
na_as_string <- function(x) {
  
  ## Description
  # na_as_string() returns a character vector with NA values replaced as "NA"
  
  x <- as.character(x)
  
  x[which(is.na(x) == TRUE)] <- "NA"
  
  return(x)
  
  }

# NA as String
na_as_zero <- function(x) {
  
  ## Description
  # na_as_zero() returns a numeric vector with NA values replaced as 0
  
  x <- nabs(x)
  
  x[which(is.na(x) == TRUE)] <- 0
  
  return(x)
  
  }

# F Table to Data Frame
ftable2df <- function(mydata) {
  
  ## Description
  # ftable2df() returns a data.frame from an ftable object
  
  ifelse(class(mydata) == "ftable", 
         mydata <- mydata, 
         mydata <- ftable(mydata)
         )
  
  dfrows <- rev(expand.grid(rev(attr(mydata, "row.vars"))))
  
  dfcols <- as.data.frame.matrix(mydata)
  
  do.call(paste, 
          c(rev(expand.grid(rev(attr(mydata, "col.vars")))), 
            sep = "_"
            )
          ) -> names(dfcols)
  
  cbind(dfrows, dfcols)
  
  }


###############################################################################################################





