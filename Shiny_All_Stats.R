#################################################################################
#####          SHINY All Stats            ||             09/22/18           #####
#################################################################################

# ALLSCRAPE Dependencies
library(RCurl); library(rjson); library(lubridate); library(doMC); library(rvest)

# Script Dependencies
library(Matrix); library(RSQLite)
library(arm); library(lme4)
library(caret); library(xgboost); library(glmnet)
library(tidyverse)

options(scipen = 999)
set.seed(123)



install.packages("foreach")



###  Load Object Data
#####################################

## -------- ##
##   Load   ##
## -------- ##

# New skater positions - from NHL JSON data
player_position_historic <- readRDS("data/player_position_new.rds")

# New no_xg object after ARI '07-10 issue was fixed
no_xg <- readRDS("data/no_xG_2.rds")

# NHL Schedule objects
schedule_full <- readRDS("data/team_results_with1617_pl.rds")
schedule_1718 <- readRDS("data/schedule_1718.rds")
schedule_1819 <- readRDS("data/schedule_trim_1819.rds") 

# xG models 
xG_model_XGB_7_EV <- readRDS("data/xG_model_XGB_7yr_EV_final_3.rds")
xG_model_XGB_7_UE <- readRDS("data/xG_model_XGB_7yr_UE_final_1.rds")
xG_model_XGB_10_SH <- readRDS("data/xG_model_XGB_10yr_SH_1.rds")
xG_model_XGB_10_EN <- readRDS("data/xG_model_XGB_10yr_EN_1.rds")

# Score adjustment lists
score_adj_EV <- readRDS("data/score_adj_EV_list.rds") # all together in a list - EV
score_adj_PP <- readRDS("data/score_adj_PP_list.rds") # all together in a list - PP
score_adj_SH <- readRDS("data/score_adj_SH_list.rds") # all together in a list - SH

score_adj_5v5 <- readRDS("data/score_adj_5v5_list.rds") # all together in a list - 5v5
score_adj_4v4 <- readRDS("data/score_adj_4v4_list.rds") # all together in a list - 4v4
score_adj_3v3 <- readRDS("data/score_adj_3v3_list.rds") # all together in a list - 3v3

score_adj_5v4 <- readRDS("data/score_adj_5v4_list.rds") # all together in a list - 5v4
score_adj_5v3 <- readRDS("data/score_adj_5v4_list.rds") # all together in a list - 5v4
score_adj_4v3 <- readRDS("data/score_adj_5v4_list.rds") # all together in a list - 5v4

#score_adj_4v5 <- readRDS("data/score_adj_4v5_list.rds") # all together in a list - 4v5 *** not used


# Penalty Goals Scoring Rates
scoring_rates <- readRDS("data/scoring_rates2.rds") # updated with newest version including '17-18 data
pen_score_adj <- data.frame(readRDS("data/penalty_adj_state.RDS")) # not updated as of now

# SPM Model Lists
mod_list_EVO_F <- readRDS("data/model_list_EVO_F.rds")
mod_list_EVO_D <- readRDS("data/model_list_EVO_D.rds")
mod_list_EVD_F <- readRDS("data/model_list_EVD_F.rds") # not using version # 2 (in directory)
mod_list_EVD_D <- readRDS("data/model_list_EVD_D.rds")
mod_list_PPO_F <- readRDS("data/model_list_PPO_F.rds")
mod_list_PPO_D <- readRDS("data/model_list_PPO_D.rds")
mod_list_SHD_F <- readRDS("data/model_list_SHD_F.rds")
mod_list_SHD_D <- readRDS("data/model_list_SHD_D.rds")


## ----------- ##
##   Process   ##
## ----------- ##

# Schedule / btb object
schedule_full <- schedule_full %>% 
  filter(game_id <= 2016021230)

btb <- rbind(
  schedule_full %>% 
    select(game_id, home_btb, away_btb) %>% 
    mutate(home_btb = ifelse(home_btb > 1, 1, home_btb), 
            away_btb = ifelse(away_btb > 1, 1, away_btb)), 
  
  schedule_1718 %>% 
     select(game_id, home_btb, away_btb) %>% 
     mutate(home_btb = ifelse(home_btb > 1, 1, home_btb), 
            away_btb = ifelse(away_btb > 1, 1, away_btb)), 
  
  schedule_1819 %>% 
     select(game_id, home_btb, away_btb) %>% 
     mutate(home_btb = ifelse(home_btb > 1, 1, home_btb), 
            away_btb = ifelse(away_btb > 1, 1, away_btb))
  )

rm(schedule_full, schedule_1718, schedule_1819)


# Strength State Adjustment - EV (all 11 seasons, including no_xG games)
# Position in list: 1 = 5v5, 2 = 4v4, 3 = 3v3
state_adj_EV <- list(Goals =   c(1, (2.271070 / 2.700524), (2.271070 / 5.787857)), 
                     Shots =   c(1, (29.19077 / 31.36268), (29.19077 / 39.11144)), 
                     Fenwick = c(1, (40.48002 / 42.57123), (40.48002 / 51.89824)), 
                     Corsi =   c(1, (54.21396 / 55.06157), (54.21396 / 61.97019)), 
                     xG =      c(1, (2.222845 / 2.569587), (2.222845 / 5.403740)))

# Strength State Adjustment - PP/SH (all 11 seasons, including no_xG games)
# Position in list: 1 = 5v4, 2 = 5v3, 3 = 4v3
state_adj_PP <- list(Goals =   c(1, (6.251871 / 20.533388), (6.251871 / 12.073648)), 
                     Shots =   c(1, (50.44825 / 95.010960), (50.44825 / 75.990970)), 
                     Fenwick = c(1, (70.55257 / 131.02005), (70.55257 / 105.66536)), 
                     Corsi =   c(1, (94.36919 / 165.28401), (94.36919 / 138.24361)), 
                     xG =      c(1, (6.131036 / 19.581912), (6.131036 / 11.051610)))


# Make penalty score state data
fun.pen_GF <- function(data) {
  
  penrate_GF <- data.frame(matrix(ncol = 10, nrow = 9))
  penrate_GF$X1 <- c("5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  names(penrate_GF) <- c("x", "5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  
  penrate_GF[1, ] <- c("5v5", 0, 0, 0, data[4, 4] - data[1, 4], 0, 0, 0, 0, 0)
  penrate_GF[2, ] <- c("4v4", 0, 0, 0, 0, 0, data[6, 4] - data[2, 4], 0, 0, 0)                      
  penrate_GF[3, ] <- c("3v3", 0, 0, 0, 0, 0, data[6, 4] - data[3, 4], 0, 0, 0) 
  penrate_GF[4, ] <- c("5v4", 0, data[2, 4] - data[4, 4], 0, 0, data[5, 4] - data[4, 4], 0, 0, 0, 0) 
  penrate_GF[5, ] <- c("5v3", 0, 0, 0, 0, 0, data[6, 4] - data[5, 4], 0, 0, 0) 
  penrate_GF[6, ] <- c("4v3", 0, 0, data[3, 4] - data[6, 4], 0, 0, 0, 0, 0, 0) 
  penrate_GF[7, ] <- c("6v5", 0, 0, 0, data[4, 4] - data[7, 4], 0, 0, 0, data[8, 4] - data[7, 4], 0)
  penrate_GF[8, ] <- c("6v4", 0, data[2, 4] - data[8, 4], 0, 0, 0, 0, 0, 0, data[9, 4] - data[8, 4])
  penrate_GF[9, ] <- c("6v3", 0, 0, 0, 0, 0, data[6, 4] - data[9, 4], 0, 0, 0)
  
  df <- suppressWarnings(data.frame(sapply(penrate_GF, function(x) as.numeric(x))))
  df$x <- c("5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  names(df) <- c("x", "5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  
  return(df)

  } 
penrate_GF <- fun.pen_GF(data = scoring_rates)

fun.pen_GA <- function(data) {
  
  penrate_GA <- data.frame(matrix(ncol = 10, nrow = 9))
  penrate_GA$X1 <- c("5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  names(penrate_GA) <- c("x", "5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  
  penrate_GA[1, ] <- c("5v5", 0, 0, 0, data[4, 6] - data[1, 6], 0, 0, 0, 0, 0)
  penrate_GA[2, ] <- c("4v4", 0, 0, 0, 0, 0, data[6, 6] - data[2, 6], 0, 0, 0)                      
  penrate_GA[3, ] <- c("3v3", 0, 0, 0, 0, 0, data[6, 6] - data[3, 6], 0, 0, 0) 
  penrate_GA[4, ] <- c("5v4", 0, data[2, 6] - data[4, 6], 0, 0, data[5, 6] - data[4, 6], 0, 0, 0, 0) 
  penrate_GA[5, ] <- c("5v3", 0, 0, 0, 0, 0, data[6, 6] - data[5, 6], 0, 0, 0) 
  penrate_GA[6, ] <- c("4v3", 0, 0, data[3, 6] - data[6, 6], 0, 0, 0, 0, 0, 0) 
  penrate_GA[7, ] <- c("6v5",  0, 0, 0, data[4, 6] - data[7, 6], 0, 0, 0, data[8, 6] - data[7, 6], 0)
  penrate_GA[8, ] <- c("6v4", 0, data[2, 6] - data[8, 6], 0, 0, 0, 0, 0, 0, data[9, 6] - data[8, 6])
  penrate_GA[9, ] <- c("6v3", 0, 0, 0, 0, 0, data[6, 6] - data[9, 6], 0, 0, 0)
  
  df <- suppressWarnings(data.frame(sapply(penrate_GA, function(x) as.numeric(x))))
  df$x <- c("5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  names(df) <- c("x", "5v5", "4v4", "3v3", "5v4", "5v3", "4v3", "6v5", "6v4", "6v3")
  
  
  return(df)

  }
penrate_GA <- fun.pen_GA(data = scoring_rates)

rm(fun.pen_GF, fun.pen_GA)


## Manny Perry's Objects
st.shot_events <- c("SHOT",  "GOAL")
st.fenwick_events <- c("SHOT", "GOAL", "MISS")
st.corsi_events <- c("SHOT", "GOAL", "MISS", "BLOCK" )
st.strength_states <- c("3v3", "5v5", "4v4", "5v4", "4v5", "5v3", "3v5", "4v3", "3v4", "5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor()
st.even_strength <- c("5v5", "4v4", "3v3") %>% as.factor()
st.pp_strength <- c("5v4", "4v5", "5v3", "3v5", "4v3", "3v4") %>% as.factor()
st.empty_net <- c("5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor()


#####################################


# Source Shiny_All_Functions.R & ALLSCRAPE.R Scripts
source("Shiny_All_Functions.R")
source("ALLSCRAPE.R")




## ----------------------- START NEW DATA PREP ------------------------ ##




## ----------------------- ##
##   Scrape New PBP Data   ##
## ----------------------- ##

#############################

# Scrape Schedule of previous games
schedule_current <- fun.schedule(Sys.Date() - 1,
                                 Sys.Date() - 1)

print(schedule_current)

schedule_current <- schedule_current_1819 %>% 
  filter(game_status != "Scheduled")


# Scrape pbp data
fun.scrape_pbp <- function(year) { 
  
  scrape <- gsub(paste0(substr(year, 1, 4), 0), "", as.character(unique(schedule_current$game_id)))
  
  Year <- year
  hold_running <- data.frame()
  new_pbp <- data.frame()
  runs <- as.numeric(length(scrape))
  
  for(i in 1:runs) {
    
    tryCatch({
      
      #game_num <- (as.numeric(scrape[1]) - 1) + i
      game_num <- scrape[i]
      
      pbp_list <- ds.compile_games(games = as.character(game_num),
                                   season = Year,
                                   pause = 2,
                                   try_tolerance = 5,
                                   agents = ds.user_agents)
      
      hold_running <- pbp_list[[1]]
      hold_rosters <- pbp_list[[2]]
      hold_shifts <-  pbp_list[[3]]
      
      if (i == 1) { 
        new_pbp <-     pbp_list[[1]]
        new_rosters <- pbp_list[[2]]
        new_shifts <-  pbp_list[[3]]
        
        } 
      else if(i > 1) { 
        new_pbp <-     rbind(new_pbp, hold_running) 
        new_rosters <- rbind(new_rosters, hold_rosters) 
        new_shifts <-  rbind(new_shifts, hold_shifts) 
        
        }
      
      }, 
    
    # tryCatch Error function
    error = function(e) {cat("ERROR :", conditionMessage(e), "\n")}
    
    )
    
    }
  
  # Return List
  return_list <- list(new_pbp =     new_pbp, 
                      new_rosters = new_rosters, 
                      new_shifts =  new_shifts)
  
  return(return_list)
  
  }
pbp_list_new <- fun.scrape_pbp(year = "20182019")

# Pull out new data
pbp_new <- pbp_list_new$new_pbp

shifts_new <- pbp_list_new$new_shifts %>% 
  rename(player = player_name)

rosters_new <- pbp_list_new$new_rosters %>% 
  mutate(position = ifelse(player_position == "G", 3, 
                           ifelse(player_position == "D", 2, 1)))


#############################




## --------------------------- ##
##   Get Positions In Season   ##  
## --------------------------- ##

#################################

# Scrape NHL skater JSON information
NHL_player_info <- fun.NHL_info_scrape(season_ = "20182019")


# Update player_posittion data.frame
player_position <- player_position_historic %>% 
  rbind(., NHL_player_info %>% 
          filter(position != 3) %>% 
          select(player, position)
        ) %>% 
  group_by(player, position) %>% 
  summarise() %>% 
  ungroup() %>% 
  mutate(test = 1 * (lag(player, default = "A") == player)) %>%  # test for and remove duplicates
  filter(test != 1) %>%
  select(-c(test)) %>% 
  
  ##        ***  Manual Addition  ***            ##
  # rbind(., data.frame(player = "CALVIN.PETERSEN",
  #                    position = 3)
  #      ) %>%
  
  bind_rows(
    data.frame(
      player = c("ALEKSI.SAARELA", "CALE.MAKAR", "VLADISLAV.GAVRIKOV"),
      position = c(1, 2, 2), 
      stringsAsFactors = FALSE
      )
    ) %>%
  
  arrange(player) %>% 
  data.frame()


#################################



## --------------------------------------------- ##
##   Fix Names in PBP, Rosters, and Shift Data   ## *** PATRICK.MAROON Name Fix Added
## --------------------------------------------- ##

###################################################

# Update skater names in home_on_1:away_on_6 pbp slots
pbp_new <- pbp_new %>% 
  mutate_at(vars(event_player_1, event_player_2, event_player_3, home_on_1:home_on_6, away_on_1:away_on_6), 
            funs(ifelse(. == "ANDREI.KASTSITSYN", "ANDREI.KOSTITSYN", 
                        ifelse(. == "AJ.GREER", "A.J..GREER", 
                               ifelse(. == "ANDREW.GREENE", "ANDY.GREENE", 
                                      ifelse(. == "ANDREW.WOZNIEWSKI", "ANDY.WOZNIEWSKI", 
                                             ifelse(. == "ANTHONY.DEANGELO", "TONY.DEANGELO", 
                                                    ifelse(. == "BATES (JON).BATTAGLIA", "BATES.BATTAGLIA", 
                                                           ifelse(. == "BRADLEY.MILLS", "BRAD.MILLS", 
                                                                  ifelse(. == "COLIN (JOHN).WHITE", "COLIN.WHITE", 
                                                                         ifelse(. == "CRISTOVAL.NIEVES", "BOO.NIEVES", 
                                                                                ifelse(. == "DANNY.BRIERE", "DANIEL.BRIERE", 
                                                                                       ifelse(. %in% c("DAN.CLEARY", "DANNY.CLEARY"), "DANIEL.CLEARY", 
                  ifelse(. == "DANNY.O'REGAN", "DANIEL.O'REGAN", 
                         ifelse(. == "DENIS JR..GAUTHIER", "DENIS.GAUTHIER", 
                                ifelse(. == "FREDERICK.MEYER IV", "FREDDY.MEYER", 
                                       ifelse(. == "JACOB.DOWELL", "JAKE.DOWELL", 
                                              ifelse(. == "JAMES.VANDERMEER", "JIM.VANDERMEER", 
                                                     ifelse(. == "JAMES.WYMAN", "JT.WYMAN", 
                                                            ifelse(. == "JOHN.HILLEN III", "JACK.HILLEN", 
                                                                   ifelse(. == "JOHN.ODUYA", "JOHNNY.ODUYA", 
                                                                          ifelse(. == "JOHN.PEVERLEY", "RICH.PEVERLEY", 
                                                                                 ifelse(. == "JONATHAN.SIM", "JON.SIM", 
                                                                                        ifelse(. == "JONATHON.KALINSKI", "JON.KALINSKI", 
                                                                                               ifelse(. == "JOSEPH.CRABB", "JOEY.CRABB", 
                                                                                                      ifelse(. == "JOSHUA.BAILEY", "JOSH.BAILEY", 
                                                                                                             ifelse(. == "JOSHUA.MORRISSEY", "JOSH.MORRISSEY", 
                  ifelse(. == "JT.COMPHER", "J.T..COMPHER", 
                         ifelse(. == "KRYSTOFER.KOLANOS", "KRYS.KOLANOS", 
                                ifelse(. == "MARC.POULIOT", "MARC-ANTOINE.POULIOT", 
                                       ifelse(. == "MARTIN.ST. PIERRE", "MARTIN.ST PIERRE", 
                                              ifelse(. == "MARTY.HAVLAT", "MARTIN.HAVLAT", 
                                                     ifelse(. == "MATHEW.DUMBA", "MATT.DUMBA", 
                                                            ifelse(. == "MATTHEW.IRWIN", "MATT.IRWIN", 
                                                                   ifelse(. == "MATTHEW.NIETO", "MATT.NIETO", 
                                                                          ifelse(. == "MATTHEW.STAJAN", "MATT.STAJAN", 
                                                                                 ifelse(. == "MAXIM.MAYOROV", "MAKSIM.MAYOROV", 
                                                                                        ifelse(. == "MAXWELL.REINHART", "MAX.REINHART", 
                                                                                               ifelse(. == "MICHAEL.BLUNDEN", "MIKE.BLUNDEN", 
                                                                                                      ifelse(. == "MICHAËL.BOURNIVAL", "MICHAEL.BOURNIVAL", 
                                                                                                             ifelse(. == "MICHAEL.GRIER", "MIKE.GRIER",
                                                                                                                    ifelse(. == "MICHAEL.KNUBLE", "MIKE.KNUBLE", 
                   ifelse(. == "MICHAEL.KOMISAREK", "MIKE.KOMISAREK", 
                          ifelse(. == "MICHAEL.MATHESON", "MIKE.MATHESON", 
                                 ifelse(. == "MICHAEL.MODANO", "MIKE.MODANO", 
                                        ifelse(. == "MICHAEL.RUPP", "MIKE.RUPP", 
                                               ifelse(. == "MICHAEL.SILLINGER", "MIKE.SILLINGER", 
                                                      ifelse(. == "NATHAN.GUENIN", "NATE.GUENIN", 
                                                             ifelse(. == "NICHOLAS.BOYNTON", "NICK.BOYNTON", 
                                                                    ifelse(. == "NICHOLAS.DRAZENOVIC", "NICK.DRAZENOVIC", .)
                                                                    )))))))))))))))))))))))))))))))))))))))))))))))))

pbp_new <- pbp_new %>% 
  mutate_at(vars(event_player_1, event_player_2, event_player_3, home_on_1:home_on_6, away_on_1:away_on_6), 
            funs(ifelse(. == "NICKLAS.BERGFORS", "NICLAS.BERGFORS",
                        ifelse(. == "NIKLAS.KRONVALL", "NIKLAS.KRONWALL", 
                               ifelse(. == "NIKOLAI.ANTROPOV", "NIK.ANTROPOV", 
                                      ifelse(. == "NIKOLAI.ZHERDEV", "NIKOLAY.ZHERDEV", 
                                             ifelse(. == "OLIVIER.MAGNAN-GRENIER", "OLIVIER.MAGNAN", 
                                                    ifelse(. %in% c("P. J..AXELSSON", "PER JOHAN.AXELSSON"), "P.J..AXELSSON", 
                                                           ifelse(. == "PIERRE-ALEX.PARENTEAU", "P.A..PARENTEAU", 
                                                                  ifelse(. == "PHILIP.VARONE", "PHIL.VARONE", 
                                                                         ifelse(. == "RAYMOND.MACIAS", "RAY.MACIAS", 
                                                                                ifelse(. == "RJ.UMBERGER", "R.J..UMBERGER", 
                                                                                       ifelse(. == "ROBERT.BLAKE", "ROB.BLAKE", 
                                                                                              ifelse(. == "ROBERT.EARL", "ROBBIE.EARL", 
                                                                                                     ifelse(. == "ROBERT.HOLIK", "BOBBY.HOLIK", 
                                                                                                            ifelse(. == "ROBERT.SCUDERI", "ROB.SCUDERI", 
                                                                                                                   ifelse(. == "RODNEY.PELLEY", "ROD.PELLEY", 
                        ifelse(. == "SIARHEI.KASTSITSYN", "SERGEI.KOSTITSYN", 
                               ifelse(. == "STAFFAN.KRONVALL", "STAFFAN.KRONWALL", 
                                      ifelse(. == "STEVEN.REINPRECHT", "STEVE.REINPRECHT", 
                                             ifelse(. == "TJ.GALIARDI", "T.J..GALIARDI", 
                                                    ifelse(. == "TJ.HENSICK", "T.J..HENSICK", 
                                                           ifelse(. == "TOMMY.SESTITO", "TOM.SESTITO", 
                                                                  ifelse(. == "VACLAV.PROSPAL", "VINNY.PROSPAL", 
                                                                         ifelse(. == "VINCENT.HINOSTROZA", "VINNIE.HINOSTROZA", 
                                                                                ifelse(. == "WILLIAM.THOMAS", "BILL.THOMAS", 
                                                                                       ifelse(. == "ZACHARY.ASTON-REESE", "ZACH.ASTON-REESE", 
                                                                                              ifelse(. == "ZACHARY.SANFORD", "ZACH.SANFORD", 
                                                                                                     ifelse(. == "ZACHERY.STORTINI", "ZACK.STORTINI", 
                                                                                                            
                                                                                                            ## EXTRAS
                                                                                                            ifelse(. == "PAT.MAROON", "PATRICK.MAROON", 
                                                                                                            .)
                                                                                                     )))))))))))))))))))))))))))))


# Update skater names roster data
rosters_new <- rosters_new %>% 
  mutate_at(vars(player_name), 
            funs(ifelse(. == "ANDREI.KASTSITSYN", "ANDREI.KOSTITSYN", 
                        ifelse(. == "AJ.GREER", "A.J..GREER", 
                               ifelse(. == "ANDREW.GREENE", "ANDY.GREENE", 
                                      ifelse(. == "ANDREW.WOZNIEWSKI", "ANDY.WOZNIEWSKI", 
                                             ifelse(. == "ANTHONY.DEANGELO", "TONY.DEANGELO", 
                                                    ifelse(. == "BATES (JON).BATTAGLIA", "BATES.BATTAGLIA", 
                                                           ifelse(. == "BRADLEY.MILLS", "BRAD.MILLS", 
                                                                  ifelse(. == "COLIN (JOHN).WHITE", "COLIN.WHITE", 
                                                                         ifelse(. == "CRISTOVAL.NIEVES", "BOO.NIEVES", 
                                                                                ifelse(. == "DANNY.BRIERE", "DANIEL.BRIERE", 
                                                                                       ifelse(. %in% c("DAN.CLEARY", "DANNY.CLEARY"), "DANIEL.CLEARY", 
                                                                                              ifelse(. == "DANNY.O'REGAN", "DANIEL.O'REGAN", 
                         ifelse(. == "DENIS JR..GAUTHIER", "DENIS.GAUTHIER", 
                                ifelse(. == "FREDERICK.MEYER IV", "FREDDY.MEYER", 
                                       ifelse(. == "JACOB.DOWELL", "JAKE.DOWELL", 
                                              ifelse(. == "JAMES.VANDERMEER", "JIM.VANDERMEER", 
                                                     ifelse(. == "JAMES.WYMAN", "JT.WYMAN", 
                                                            ifelse(. == "JOHN.HILLEN III", "JACK.HILLEN", 
                                                                   ifelse(. == "JOHN.ODUYA", "JOHNNY.ODUYA", 
                                                                          ifelse(. == "JOHN.PEVERLEY", "RICH.PEVERLEY", 
                                                                                 ifelse(. == "JONATHAN.SIM", "JON.SIM", 
                                                                                        ifelse(. == "JONATHON.KALINSKI", "JON.KALINSKI", 
                                                                                               ifelse(. == "JOSEPH.CRABB", "JOEY.CRABB", 
                                                                                                      ifelse(. == "JOSHUA.BAILEY", "JOSH.BAILEY", 
                                                                                                             ifelse(. == "JOSHUA.MORRISSEY", "JOSH.MORRISSEY", 
                          ifelse(. == "JT.COMPHER", "J.T..COMPHER", 
                                 ifelse(. == "KRYSTOFER.KOLANOS", "KRYS.KOLANOS", 
                                        ifelse(. == "MARC.POULIOT", "MARC-ANTOINE.POULIOT", 
                                               ifelse(. == "MARTIN.ST. PIERRE", "MARTIN.ST PIERRE", 
                                                      ifelse(. == "MARTY.HAVLAT", "MARTIN.HAVLAT", 
                                                             ifelse(. == "MATHEW.DUMBA", "MATT.DUMBA", 
                                                                    ifelse(. == "MATTHEW.IRWIN", "MATT.IRWIN", 
                                                                           ifelse(. == "MATTHEW.NIETO", "MATT.NIETO", 
                                                                                  ifelse(. == "MATTHEW.STAJAN", "MATT.STAJAN", 
                                                                                         ifelse(. == "MAXIM.MAYOROV", "MAKSIM.MAYOROV", 
                                                                                                ifelse(. == "MAXWELL.REINHART", "MAX.REINHART", 
                                                                                                       ifelse(. == "MICHAEL.BLUNDEN", "MIKE.BLUNDEN", 
                                                                                                              ifelse(. == "MICHAËL.BOURNIVAL", "MICHAEL.BOURNIVAL", 
                                                                                                                     ifelse(. == "MICHAEL.GRIER", "MIKE.GRIER",
                                                                                                                            ifelse(. == "MICHAEL.KNUBLE", "MIKE.KNUBLE", 
                           ifelse(. == "MICHAEL.KOMISAREK", "MIKE.KOMISAREK", 
                                  ifelse(. == "MICHAEL.MATHESON", "MIKE.MATHESON", 
                                         ifelse(. == "MICHAEL.MODANO", "MIKE.MODANO", 
                                                ifelse(. == "MICHAEL.RUPP", "MIKE.RUPP", 
                                                       ifelse(. == "MICHAEL.SILLINGER", "MIKE.SILLINGER", 
                                                              ifelse(. == "NATHAN.GUENIN", "NATE.GUENIN", 
                                                                     ifelse(. == "NICHOLAS.BOYNTON", "NICK.BOYNTON", 
                                                                            ifelse(. == "NICHOLAS.DRAZENOVIC", "NICK.DRAZENOVIC", .)
                                                                     )))))))))))))))))))))))))))))))))))))))))))))))))

rosters_new <- rosters_new %>% 
  mutate_at(vars(player_name), 
            funs(ifelse(. == "NICKLAS.BERGFORS", "NICLAS.BERGFORS",
                        ifelse(. == "NIKLAS.KRONVALL", "NIKLAS.KRONWALL", 
                               ifelse(. == "NIKOLAI.ANTROPOV", "NIK.ANTROPOV", 
                                      ifelse(. == "NIKOLAI.ZHERDEV", "NIKOLAY.ZHERDEV", 
                                             ifelse(. == "OLIVIER.MAGNAN-GRENIER", "OLIVIER.MAGNAN", 
                                                    ifelse(. %in% c("P. J..AXELSSON", "PER JOHAN.AXELSSON"), "P.J..AXELSSON", 
                                                           ifelse(. == "PIERRE-ALEX.PARENTEAU", "P.A..PARENTEAU", 
                                                                  ifelse(. == "PHILIP.VARONE", "PHIL.VARONE", 
                                                                         ifelse(. == "RAYMOND.MACIAS", "RAY.MACIAS", 
                                                                                ifelse(. == "RJ.UMBERGER", "R.J..UMBERGER", 
                                                                                       ifelse(. == "ROBERT.BLAKE", "ROB.BLAKE", 
                                                                                              ifelse(. == "ROBERT.EARL", "ROBBIE.EARL", 
                                                                                                     ifelse(. == "ROBERT.HOLIK", "BOBBY.HOLIK", 
                                                                                                            ifelse(. == "ROBERT.SCUDERI", "ROB.SCUDERI", 
                                                                                                                   ifelse(. == "RODNEY.PELLEY", "ROD.PELLEY", 
                        ifelse(. == "SIARHEI.KASTSITSYN", "SERGEI.KOSTITSYN", 
                               ifelse(. == "STAFFAN.KRONVALL", "STAFFAN.KRONWALL", 
                                      ifelse(. == "STEVEN.REINPRECHT", "STEVE.REINPRECHT", 
                                             ifelse(. == "TJ.GALIARDI", "T.J..GALIARDI", 
                                                    ifelse(. == "TJ.HENSICK", "T.J..HENSICK", 
                                                           ifelse(. == "TOMMY.SESTITO", "TOM.SESTITO", 
                                                                  ifelse(. == "VACLAV.PROSPAL", "VINNY.PROSPAL", 
                                                                         ifelse(. == "VINCENT.HINOSTROZA", "VINNIE.HINOSTROZA", 
                                                                                ifelse(. == "WILLIAM.THOMAS", "BILL.THOMAS", 
                                                                                       ifelse(. == "ZACHARY.ASTON-REESE", "ZACH.ASTON-REESE", 
                                                                                              ifelse(. == "ZACHARY.SANFORD", "ZACH.SANFORD", 
                                                                                                     ifelse(. == "ZACHERY.STORTINI", "ZACK.STORTINI", 
                                                                                                            
                                                                                                            ## EXTRAS
                                                                                                            ifelse(. == "PAT.MAROON", "PATRICK.MAROON", 
                                                                                                            .)
                                                                                              )))))))))))))))))))))))))))))


# Update skater names roster data
shifts_new <- shifts_new %>% 
  mutate_at(vars(player), 
            funs(ifelse(. == "ANDREI.KASTSITSYN", "ANDREI.KOSTITSYN", 
                        ifelse(. == "AJ.GREER", "A.J..GREER", 
                               ifelse(. == "ANDREW.GREENE", "ANDY.GREENE", 
                                      ifelse(. == "ANDREW.WOZNIEWSKI", "ANDY.WOZNIEWSKI", 
                                             ifelse(. == "ANTHONY.DEANGELO", "TONY.DEANGELO", 
                                                    ifelse(. == "BATES (JON).BATTAGLIA", "BATES.BATTAGLIA", 
                                                           ifelse(. == "BRADLEY.MILLS", "BRAD.MILLS", 
                                                                  ifelse(. == "COLIN (JOHN).WHITE", "COLIN.WHITE", 
                                                                         ifelse(. == "CRISTOVAL.NIEVES", "BOO.NIEVES", 
                                                                                ifelse(. == "DANNY.BRIERE", "DANIEL.BRIERE", 
                                                                                       ifelse(. %in% c("DAN.CLEARY", "DANNY.CLEARY"), "DANIEL.CLEARY", 
                                                                                              ifelse(. == "DANNY.O'REGAN", "DANIEL.O'REGAN", 
                         ifelse(. == "DENIS JR..GAUTHIER", "DENIS.GAUTHIER", 
                                ifelse(. == "FREDERICK.MEYER IV", "FREDDY.MEYER", 
                                       ifelse(. == "JACOB.DOWELL", "JAKE.DOWELL", 
                                              ifelse(. == "JAMES.VANDERMEER", "JIM.VANDERMEER", 
                                                     ifelse(. == "JAMES.WYMAN", "JT.WYMAN", 
                                                            ifelse(. == "JOHN.HILLEN III", "JACK.HILLEN", 
                                                                   ifelse(. == "JOHN.ODUYA", "JOHNNY.ODUYA", 
                                                                          ifelse(. == "JOHN.PEVERLEY", "RICH.PEVERLEY", 
                                                                                 ifelse(. == "JONATHAN.SIM", "JON.SIM", 
                                                                                        ifelse(. == "JONATHON.KALINSKI", "JON.KALINSKI", 
                                                                                               ifelse(. == "JOSEPH.CRABB", "JOEY.CRABB", 
                                                                                                      ifelse(. == "JOSHUA.BAILEY", "JOSH.BAILEY", 
                                                                                                             ifelse(. == "JOSHUA.MORRISSEY", "JOSH.MORRISSEY", 
                          ifelse(. == "JT.COMPHER", "J.T..COMPHER", 
                                 ifelse(. == "KRYSTOFER.KOLANOS", "KRYS.KOLANOS", 
                                        ifelse(. == "MARC.POULIOT", "MARC-ANTOINE.POULIOT", 
                                               ifelse(. == "MARTIN.ST. PIERRE", "MARTIN.ST PIERRE", 
                                                      ifelse(. == "MARTY.HAVLAT", "MARTIN.HAVLAT", 
                                                             ifelse(. == "MATHEW.DUMBA", "MATT.DUMBA", 
                                                                    ifelse(. == "MATTHEW.IRWIN", "MATT.IRWIN", 
                                                                           ifelse(. == "MATTHEW.NIETO", "MATT.NIETO", 
                                                                                  ifelse(. == "MATTHEW.STAJAN", "MATT.STAJAN", 
                                                                                         ifelse(. == "MAXIM.MAYOROV", "MAKSIM.MAYOROV", 
                                                                                                ifelse(. == "MAXWELL.REINHART", "MAX.REINHART", 
                                                                                                       ifelse(. == "MICHAEL.BLUNDEN", "MIKE.BLUNDEN", 
                                                                                                              ifelse(. == "MICHAËL.BOURNIVAL", "MICHAEL.BOURNIVAL", 
                                                                                                                     ifelse(. == "MICHAEL.GRIER", "MIKE.GRIER",
                                                                                                                            ifelse(. == "MICHAEL.KNUBLE", "MIKE.KNUBLE", 
                           ifelse(. == "MICHAEL.KOMISAREK", "MIKE.KOMISAREK", 
                                  ifelse(. == "MICHAEL.MATHESON", "MIKE.MATHESON", 
                                         ifelse(. == "MICHAEL.MODANO", "MIKE.MODANO", 
                                                ifelse(. == "MICHAEL.RUPP", "MIKE.RUPP", 
                                                       ifelse(. == "MICHAEL.SILLINGER", "MIKE.SILLINGER", 
                                                              ifelse(. == "NATHAN.GUENIN", "NATE.GUENIN", 
                                                                     ifelse(. == "NICHOLAS.BOYNTON", "NICK.BOYNTON", 
                                                                            ifelse(. == "NICHOLAS.DRAZENOVIC", "NICK.DRAZENOVIC", .)
                                                                     )))))))))))))))))))))))))))))))))))))))))))))))))

shifts_new <- shifts_new %>% 
  mutate_at(vars(player), 
            funs(ifelse(. == "NICKLAS.BERGFORS", "NICLAS.BERGFORS",
                        ifelse(. == "NIKLAS.KRONVALL", "NIKLAS.KRONWALL", 
                               ifelse(. == "NIKOLAI.ANTROPOV", "NIK.ANTROPOV", 
                                      ifelse(. == "NIKOLAI.ZHERDEV", "NIKOLAY.ZHERDEV", 
                                             ifelse(. == "OLIVIER.MAGNAN-GRENIER", "OLIVIER.MAGNAN", 
                                                    ifelse(. %in% c("P. J..AXELSSON", "PER JOHAN.AXELSSON"), "P.J..AXELSSON", 
                                                           ifelse(. == "PIERRE-ALEX.PARENTEAU", "P.A..PARENTEAU", 
                                                                  ifelse(. == "PHILIP.VARONE", "PHIL.VARONE", 
                                                                         ifelse(. == "RAYMOND.MACIAS", "RAY.MACIAS", 
                                                                                ifelse(. == "RJ.UMBERGER", "R.J..UMBERGER", 
                                                                                       ifelse(. == "ROBERT.BLAKE", "ROB.BLAKE", 
                                                                                              ifelse(. == "ROBERT.EARL", "ROBBIE.EARL", 
                                                                                                     ifelse(. == "ROBERT.HOLIK", "BOBBY.HOLIK", 
                                                                                                            ifelse(. == "ROBERT.SCUDERI", "ROB.SCUDERI", 
                                                                                                                   ifelse(. == "RODNEY.PELLEY", "ROD.PELLEY", 
                        ifelse(. == "SIARHEI.KASTSITSYN", "SERGEI.KOSTITSYN", 
                               ifelse(. == "STAFFAN.KRONVALL", "STAFFAN.KRONWALL", 
                                      ifelse(. == "STEVEN.REINPRECHT", "STEVE.REINPRECHT", 
                                             ifelse(. == "TJ.GALIARDI", "T.J..GALIARDI", 
                                                    ifelse(. == "TJ.HENSICK", "T.J..HENSICK", 
                                                           ifelse(. == "TOMMY.SESTITO", "TOM.SESTITO", 
                                                                  ifelse(. == "VACLAV.PROSPAL", "VINNY.PROSPAL", 
                                                                         ifelse(. == "VINCENT.HINOSTROZA", "VINNIE.HINOSTROZA", 
                                                                                ifelse(. == "WILLIAM.THOMAS", "BILL.THOMAS", 
                                                                                       ifelse(. == "ZACHARY.ASTON-REESE", "ZACH.ASTON-REESE", 
                                                                                              ifelse(. == "ZACHARY.SANFORD", "ZACH.SANFORD", 
                                                                                                     ifelse(. == "ZACHERY.STORTINI", "ZACK.STORTINI", 
                                                                                                            
                                                                                                            ## EXTRAS
                                                                                                            ifelse(. == "PAT.MAROON", "PATRICK.MAROON", 
                                                                                                            .)
                                                                                              )))))))))))))))))))))))))))))


###################################################



# Test for NA positions in new pbp data
fun.position_test <- function(pbp_data) { 
  
  pbp_data <- pbp_new
  
  goalies <- sort(unique(c(pbp_data$home_goalie, pbp_data$away_goalie)))
  
  position_test <- data.frame(player = sort(unique(c(
    sort(na.omit(unique(pbp_data$home_on_1))), 
    sort(na.omit(unique(pbp_data$home_on_2))), 
    sort(na.omit(unique(pbp_data$home_on_3))), 
    sort(na.omit(unique(pbp_data$home_on_4))), 
    sort(na.omit(unique(pbp_data$home_on_5))), 
    sort(na.omit(unique(pbp_data$home_on_6))), 
    
    sort(na.omit(unique(pbp_data$away_on_1))), 
    sort(na.omit(unique(pbp_data$away_on_2))), 
    sort(na.omit(unique(pbp_data$away_on_3))), 
    sort(na.omit(unique(pbp_data$away_on_4))), 
    sort(na.omit(unique(pbp_data$away_on_5))), 
    sort(na.omit(unique(pbp_data$away_on_6))))))
    ) %>% 
    mutate(player = as.character(player)) %>% 
    filter(!player %in% goalies) %>% 
    left_join(., player_position, by = "player") %>% 
    filter(is.na(position))
  
  # Print result
  if(nrow(position_test) > 0) { 
    print(paste0("There were position mismatches!"), quote = F)
    print(paste0("NA Players: ", position_test$player), quote = F)
    
    }
  else if(nrow(position_test) == 0) { 
    print(paste0("No position mismatches!"), quote = F)
    
    }
  
  return(position_test)
  
  }
position_test <- fun.position_test(pbp_data = pbp_new)




## --------------------------------- ##
##   Prep / Add xG Data to New PBP   ##
## --------------------------------- ##

#######################################

# Functions to expand, index, and add xG (4 models) to scraped pbp df (version current 2/26/18)
pbp_xG_add_list <- fun.pbp_full_add(data = pbp_new, 
                                    model_EV = xG_model_XGB_7_EV, 
                                    model_UE = xG_model_XGB_7_UE, 
                                    model_SH = xG_model_XGB_10_SH, 
                                    model_EN = xG_model_XGB_10_EN)

# Return data from function
pbp_df <- pbp_xG_add_list$pbp_full


# Check densities
hist(na.omit(filter(pbp_df, event_type %in% st.fenwick_events, grepl("E", game_strength_state))$pred_XGB_7), breaks = 50)
hist(na.omit(filter(pbp_df, event_type %in% st.fenwick_events, game_strength_state %in% st.pp_strength)$pred_XGB_7), breaks = 50)
hist(na.omit(filter(pbp_df, event_type %in% st.fenwick_events, game_strength_state %in% st.even_strength)$pred_XGB_7), breaks = 50)


if (nrow(pbp_new) == nrow(pbp_df) & ncol(pbp_new) == ncol(pbp_df) - 14) { 
  rm(pbp_xG_add_list)
  print("Success! All Rows Intact! All Columns Added!")
  
  } else { 
    print("UH-OH!! Data Not Added as Intended :(")  
  
    }

# Test
paste0("Season: ", unique(pbp_df$season), "  //  Games: ", length(unique(pbp_df$game_id)), 
       "  //  Goals: ", sum((pbp_df$event_type == "GOAL")), "  //  xG: ", round(sum(na.omit(pbp_df$pred_XGB_7)), 2), 
       "  //  NA fenwicks: ", 1 - sum(pbp_df$event_type %in% st.fenwick_events & !is.na(pbp_df$pred_XGB_7)) / 
         sum((pbp_df$event_type %in% st.fenwick_events & pbp_df$game_period < 5)))


#######################################






## -------------------- RUN GAME BY GAME FUNCTIONS --------------------- ##


## -------------------------- ##
##   Skaters - Game By Game   ##
## -------------------------- ##

################################

games_all_sit_new <- fun.all_sit_standard(data_ = pbp_df)

games_EV_new <- fun.combine_counts(data = pbp_df)
games_PP_new <- fun.combine_counts_PP(data = pbp_df)
games_SH_new <- fun.combine_counts_SH(data = pbp_df)

games_5v5_new <- fun.combine_counts_EV_strength(data = pbp_df, strength = "5v5", scr_adj_list = score_adj_5v5)
games_4v4_new <- fun.combine_counts_EV_strength(data = pbp_df, strength = "4v4", scr_adj_list = score_adj_4v4)
games_3v3_new <- fun.combine_counts_EV_strength(data = pbp_df, strength = "3v3", scr_adj_list = score_adj_3v3)

games_5v4_new <- fun.combine_counts_PP_strength(data = pbp_df, strength = "5v4", scr_adj_list = score_adj_5v4)
games_5v3_new <- fun.combine_counts_PP_strength(data = pbp_df, strength = "5v3", scr_adj_list = score_adj_5v3)
games_4v3_new <- fun.combine_counts_PP_strength(data = pbp_df, strength = "4v3", scr_adj_list = score_adj_4v3)

games_4v5_new <- fun.combine_counts_SH_strength(data = pbp_df, strength = "4v5", scr_adj_list = score_adj_5v4)
games_3v5_new <- fun.combine_counts_SH_strength(data = pbp_df, strength = "3v5", scr_adj_list = score_adj_5v3)
games_3v4_new <- fun.combine_counts_SH_strength(data = pbp_df, strength = "3v4", scr_adj_list = score_adj_4v3)


################################


## ------------------------------- ##
##   Rel_TM - TOI Together Games   ##
## ------------------------------- ##

#####################################

teammate_TOI_EV_new <- fun.teammate(pbp_data = pbp_df, strength = "even")
teammate_TOI_PP_new <- fun.teammate(pbp_data = pbp_df, strength = "powerplay")
teammate_TOI_SH_new <- fun.teammate(pbp_data = pbp_df, strength = "shorthanded")

teammate_TOI_5v5_new <- fun.teammate(pbp_data = pbp_df, strength = "5v5")
teammate_TOI_5v4_new <- fun.teammate(pbp_data = pbp_df, strength = "5v4")
teammate_TOI_4v5_new <- fun.teammate(pbp_data = pbp_df, strength = "4v5")


#####################################


## -------------------------- ##
##   Goalies - Game by Game   ##
## -------------------------- ##

################################

goalie_games_all_sit_new <- fun.goalie_games(data = pbp_df)


################################


## ---------------------------- ##
##   Penalty Goals Calculaton   ##
## ---------------------------- ##

##################################

# Run Functions
pen_source <- fun.pen_setup(data = pbp_df)

# Manual Fix for EN to 4v4 penalties
# pen_source <- fun.pen_setup(data = pbp_df %>%
#                               mutate(game_strength_state = ifelse(game_id == "2018021179" & event_index == 292 & event_type == "PENL", "5v5", game_strength_state),
#                                      game_strength_state = ifelse(game_id == "2018021179" & event_index == 293 & event_type == "PENL", "5v5", game_strength_state)
#                                      )
#                             )

pen_enhanced <- fun.pen_assign(data = pen_source)
pen_calc_main <- fun.pen_value_main(pen_data = pen_enhanced, pbp_data = pbp_df)
pen_calc_xtras <- fun.pen_value_xtra(data = pen_enhanced)
pen_team_take <- fun.pen_team_take(data = pen_calc_main)
adj_pen_games <- fun.pen_value_sum(main_data = pen_calc_main, xtra_data = pen_calc_xtras)
adj_pen_games_new <- fun.pen_value_sum_add(pen_data = adj_pen_games, skater_data = games_all_sit_new, goalie_data = goalie_games_all_sit_new, position_data = player_position)


##################################


## ------------------------------ ##
##   Team Stats Games Functions   ##
## ------------------------------ ##

####################################

team_games_all_sit_new <- fun.team_games_all_sit(data = pbp_df)

team_games_EV_new <- fun.team_games_EV(data = pbp_df, strength = "EV", scr_adj_list = score_adj_EV)
team_games_PP_new <- fun.team_games_PP(data = pbp_df, strength = "PP", scr_adj_list = score_adj_PP)
team_games_SH_new <- fun.team_games_SH(data = pbp_df, strength = "SH", scr_adj_list = score_adj_PP)

team_games_5v5_new <- fun.team_games_EV(data = pbp_df, strength = "5v5", scr_adj_list = score_adj_5v5)
team_games_4v4_new <- fun.team_games_EV(data = pbp_df, strength = "4v4", scr_adj_list = score_adj_4v4)
team_games_3v3_new <- fun.team_games_EV(data = pbp_df, strength = "3v3", scr_adj_list = score_adj_3v3)

team_games_5v4_new <- fun.team_games_PP(data = pbp_df, strength = "5v4", scr_adj_list = score_adj_5v4)
team_games_5v3_new <- fun.team_games_PP(data = pbp_df, strength = "5v3", scr_adj_list = score_adj_5v3)
team_games_4v3_new <- fun.team_games_PP(data = pbp_df, strength = "4v3", scr_adj_list = score_adj_4v3)

team_games_4v5_new <- fun.team_games_SH(data = pbp_df, strength = "4v5", scr_adj_list = score_adj_5v4)
team_games_3v5_new <- fun.team_games_SH(data = pbp_df, strength = "3v5", scr_adj_list = score_adj_5v3)
team_games_3v4_new <- fun.team_games_SH(data = pbp_df, strength = "3v4", scr_adj_list = score_adj_4v3)


####################################


## --------------------------- ##
##   Team Game Charts Labels   ## *** For New PBP Data
## --------------------------- ##

#################################

team_games_new_pbp <- fun.team_game_labels(pbp_data = pbp_df)


#################################



# Verify All Games Are Included in All Data Frames from Above
fun.check_new_games <- function() { 
  
  check_games <- list(
    schedule  = sort(unique(schedule_current$game_id)), 
      
    pbp =      sort(unique(pbp_df$game_id)),
    shifts =   sort(unique(shifts_new$game_id)),
    rosters =  sort(unique(rosters_new$game_id)),
    
    games_all =  sort(unique(games_all_sit_new$game_id)), 
    games_EV =   sort(unique(games_EV_new$game_id)), 
    games_PP =   sort(unique(games_PP_new$game_id)), 
    games_SH =   sort(unique(games_SH_new$game_id)), 
    games_5v5 =  sort(unique(games_5v5_new$game_id)), 
    games_4v4 =  sort(unique(games_4v4_new$game_id)), 
    games_3v3 =  sort(unique(games_3v3_new$game_id)), 
    games_5v4 =  sort(unique(games_5v4_new$game_id)), 
    games_5v3 =  sort(unique(games_5v3_new$game_id)), 
    games_4v3 =  sort(unique(games_4v3_new$game_id)), 
    games_4v5 =  sort(unique(games_4v5_new$game_id)), 
    games_3v5 =  sort(unique(games_3v5_new$game_id)), 
    games_3v4 =  sort(unique(games_3v4_new$game_id)), 
    
    TOI_tog_EV =  sort(unique(teammate_TOI_EV_new$game_id)), 
    TOI_tog_PP =  sort(unique(teammate_TOI_PP_new$game_id)), 
    TOI_tog_SH =  sort(unique(teammate_TOI_SH_new$game_id)), 
    TOI_tog_5v5 = sort(unique(teammate_TOI_5v5_new$game_id)), 
    TOI_tog_5v4 = sort(unique(teammate_TOI_5v4_new$game_id)), 
    TOI_tog_4v5 = sort(unique(teammate_TOI_4v5_new$game_id)), 
    
    teams_all = sort(unique(team_games_all_sit_new$game_id)), 
    teams_EV =  sort(unique(team_games_EV_new$game_id)), 
    teams_PP =  sort(unique(team_games_PP_new$game_id)), 
    teams_SH =  sort(unique(team_games_SH_new$game_id)), 
    teams_5v5 =  sort(unique(team_games_5v5_new$game_id)), 
    teams_4v4 =  sort(unique(team_games_4v4_new$game_id)), 
    teams_3v3 =  sort(unique(team_games_3v3_new$game_id)), 
    teams_5v4 =  sort(unique(team_games_5v4_new$game_id)), 
    teams_5v3 =  sort(unique(team_games_5v3_new$game_id)), 
    teams_4v3 =  sort(unique(team_games_4v3_new$game_id)), 
    teams_4v5 =  sort(unique(team_games_4v5_new$game_id)), 
    teams_3v5 =  sort(unique(team_games_3v5_new$game_id)), 
    teams_3v4 =  sort(unique(team_games_3v4_new$game_id)), 
    
    pen_goals =       sort(unique(adj_pen_games_new$game_id)), 
    goalies_all_sit = sort(unique(goalie_games_all_sit_new$game_id))
    )
  
  NA_check <- c(0, 0, 0, 0, 
                sum(is.na(games_all_sit_new)), sum(is.na(games_EV_new)), sum(is.na(games_PP_new)), sum(is.na(games_SH_new)), 
                sum(is.na(games_5v5_new)),
                sum(is.na(games_4v4_new)),
                sum(is.na(games_3v3_new)),
                sum(is.na(games_5v4_new)),
                sum(is.na(games_5v3_new)),
                sum(is.na(games_4v3_new)),
                sum(is.na(games_4v5_new)),
                sum(is.na(games_3v5_new)),
                sum(is.na(games_3v4_new)),
                
                sum(is.na(teammate_TOI_EV_new)), sum(is.na(teammate_TOI_PP_new)), sum(is.na(teammate_TOI_SH_new)), 
                sum(is.na(teammate_TOI_5v5_new)), 
                sum(is.na(teammate_TOI_5v4_new)), 
                sum(is.na(teammate_TOI_4v5_new)), 
                sum(is.na(team_games_all_sit_new)), sum(is.na(team_games_EV_new)), sum(is.na(team_games_PP_new)), sum(is.na(team_games_SH_new)), 
                sum(is.na(team_games_5v5_new)),
                sum(is.na(team_games_4v4_new)),
                sum(is.na(team_games_3v3_new)),
                sum(is.na(team_games_5v4_new)),
                sum(is.na(team_games_5v3_new)),
                sum(is.na(team_games_4v3_new)),
                sum(is.na(team_games_4v5_new)),
                sum(is.na(team_games_3v5_new)),
                sum(is.na(team_games_3v4_new)),
                
                sum(is.na(adj_pen_games_new)), 
                sum(is.na(goalie_games_all_sit_new))
                )
    
  
  # Check Game Totals
  total_games <- length(check_games$pbp)
  
  check <- data.frame(game_count = do.call(rbind, lapply(check_games, function(x) length(x))), 
                      NAs = NA_check
                      ) %>% 
    rownames_to_column(., var = "table") %>% 
    mutate(test = 1 * (game_count == total_games))
  
  print(check)
  
  # Print message
  if (mean(check$test) == 1 & sum(NA_check) == 0) { 
    print("Success! - All Games Match! No NAs! Great Job! :)", quote = F)
    
    } 
  else if (mean(check$test) != 1 & sum(NA_check) == 0) { 
    print("Games Don't Match... but No NAs!", quote = F)
    
    }
  else if (mean(check$test) == 1 & sum(NA_check) != 0) { 
    print("All Games Match... NAs present")
    
    }
  else if (mean(check$test) != 1 & sum(NA_check) != 0) { 
    print("Oh No! Games Don't Match, NAs present :(")
    
    }
  
  # Return
  return(check)
  
  }
check_new_games <- fun.check_new_games()




# pbp_joined <- pbp_df
# 
# games_all_sit_joined <- games_all_sit_new
# games_EV_joined <-      games_EV_new
# games_PP_joined <-      games_PP_new
# games_SH_joined <-      games_SH_new
# games_5v5_joined <-     games_5v5_new
# games_4v4_joined <-     games_4v4_new
# games_3v3_joined <-     games_3v3_new
# games_5v4_joined <-     games_5v4_new
# games_5v3_joined <-     games_5v3_new
# games_4v3_joined <-     games_4v3_new
# games_4v5_joined <-     games_4v5_new
# games_3v5_joined <-     games_3v5_new
# games_3v4_joined <-     games_3v4_new
# 
# team_games_all_sit_joined <- team_games_all_sit_new
# team_games_EV_joined <-      team_games_EV_new
# team_games_PP_joined <-      team_games_PP_new
# team_games_SH_joined <-      team_games_SH_new
# team_games_5v5_joined <-     team_games_5v5_new
# team_games_4v4_joined <-     team_games_4v4_new
# team_games_3v3_joined <-     team_games_3v3_new
# team_games_5v4_joined <-     team_games_5v4_new
# team_games_5v3_joined <-     team_games_5v3_new
# team_games_4v3_joined <-     team_games_4v3_new
# team_games_4v5_joined <-     team_games_4v5_new
# team_games_3v5_joined <-     team_games_3v5_new
# team_games_3v4_joined <-     team_games_3v4_new
# 
# TOI_together_EV_joined <-  teammate_TOI_EV_new
# TOI_together_PP_joined <-  teammate_TOI_PP_new
# TOI_together_SH_joined <-  teammate_TOI_SH_new
# TOI_together_5v5_joined <- teammate_TOI_5v5_new
# TOI_together_5v4_joined <- teammate_TOI_5v4_new
# TOI_together_4v5_joined <- teammate_TOI_4v5_new
# 
# goalie_games_all_sit_joined <- goalie_games_all_sit_new
# adj_pen_games_joined <-        adj_pen_games_new







## ---------------- SAVE NEW DATA AND LOAD TOTAL DATA ---------------- ##


## ------------------------- ##
##   Save / Check New Data   ##
## ------------------------- ##

###############################

db <- DBI::dbConnect(SQLite(), dbname = "data/NHL_db_1819.sqlite") # NEW DATABASE NAME (second time)

# Remove game_id index before writing new data
dbSendQuery(db, "DROP INDEX game_id_index")
suppressWarnings(dbGetQuery(db, "PRAGMA INDEX_LIST(pbp_full)"))

# Write new data
dbWriteTable(db, "pbp_full", pbp_df, overwrite = F, append = T)
dbWriteTable(db, "shifts_full", shifts_new, overwrite = F, append = T)
dbWriteTable(db, "rosters_full", rosters_new, overwrite = F, append = T)
dbWriteTable(db, "game_charts_labels", team_games_new_pbp, overwrite = F, append = T)

dbWriteTable(db, "games_data_all_sit", games_all_sit_new, overwrite = F, append = T)
dbWriteTable(db, "games_data_EV", games_EV_new, overwrite = F, append = T)
dbWriteTable(db, "games_data_PP", games_PP_new, overwrite = F, append = T)
dbWriteTable(db, "games_data_SH", games_SH_new, overwrite = F, append = T)
dbWriteTable(db, "games_data_5v5", games_5v5_new, overwrite = F, append = T)
dbWriteTable(db, "games_data_4v4", games_4v4_new, overwrite = F, append = T)
dbWriteTable(db, "games_data_3v3", games_3v3_new, overwrite = F, append = T)
dbWriteTable(db, "games_data_5v4", games_5v4_new, overwrite = F, append = T)
dbWriteTable(db, "games_data_5v3", games_5v3_new, overwrite = F, append = T)
dbWriteTable(db, "games_data_4v3", games_4v3_new, overwrite = F, append = T)
dbWriteTable(db, "games_data_4v5", games_4v5_new, overwrite = F, append = T)
dbWriteTable(db, "games_data_3v5", games_3v5_new, overwrite = F, append = T)
dbWriteTable(db, "games_data_3v4", games_3v4_new, overwrite = F, append = T)

dbWriteTable(db, "team_data_all_sit", team_games_all_sit_new, overwrite = F, append = T)
dbWriteTable(db, "team_data_EV", team_games_EV_new, overwrite = F, append = T)
dbWriteTable(db, "team_data_PP", team_games_PP_new, overwrite = F, append = T)
dbWriteTable(db, "team_data_SH", team_games_SH_new, overwrite = F, append = T)
dbWriteTable(db, "team_data_5v5", team_games_5v5_new, overwrite = F, append = T)
dbWriteTable(db, "team_data_4v4", team_games_4v4_new, overwrite = F, append = T)
dbWriteTable(db, "team_data_3v3", team_games_3v3_new, overwrite = F, append = T)
dbWriteTable(db, "team_data_5v4", team_games_5v4_new, overwrite = F, append = T)
dbWriteTable(db, "team_data_5v3", team_games_5v3_new, overwrite = F, append = T)
dbWriteTable(db, "team_data_4v3", team_games_4v3_new, overwrite = F, append = T)
dbWriteTable(db, "team_data_4v5", team_games_4v5_new, overwrite = F, append = T)
dbWriteTable(db, "team_data_3v5", team_games_3v5_new, overwrite = F, append = T)
dbWriteTable(db, "team_data_3v4", team_games_3v4_new, overwrite = F, append = T)

dbWriteTable(db, "TOI_together_data_EV", teammate_TOI_EV_new, overwrite = F, append = T)
dbWriteTable(db, "TOI_together_data_PP", teammate_TOI_PP_new, overwrite = F, append = T)
dbWriteTable(db, "TOI_together_data_SH", teammate_TOI_SH_new, overwrite = F, append = T)
dbWriteTable(db, "TOI_together_data_5v5", teammate_TOI_5v5_new, overwrite = F, append = T)
dbWriteTable(db, "TOI_together_data_5v4", teammate_TOI_5v4_new, overwrite = F, append = T)
dbWriteTable(db, "TOI_together_data_4v5", teammate_TOI_4v5_new, overwrite = F, append = T)

dbWriteTable(db, "goalie_games_all_sit", goalie_games_all_sit_new, overwrite = F, append = T)
dbWriteTable(db, "adj_pen_games_all_sit", adj_pen_games_new, overwrite = F, append = T)

# Re-add game_id index to pbp_full table
dbSendQuery(db, "CREATE INDEX game_id_index ON pbp_full (game_id)")
suppressWarnings(dbGetQuery(db, "PRAGMA INDEX_LIST(pbp_full)"))

dbDisconnect(db)




## Remove a Table - *** if needed ***
#db <- DBI::dbConnect(SQLite(), dbname = "data/NHL_db_1819.sqlite") # NEW DATABASE NAME (second time)

#dbRemoveTable(db, "games_data_all_sit")
#dbRemoveTable(db, "games_data_EV")
#dbRemoveTable(db, "games_data_PP")
#dbRemoveTable(db, "games_data_SH")

#dbRemoveTable(db, "games_data_4v5")
#dbRemoveTable(db, "games_data_3v5")
#dbRemoveTable(db, "games_data_3v4")

#dbRemoveTable(db, "TOI_together_data_EV")
#dbRemoveTable(db, "TOI_together_data_PP")
#dbRemoveTable(db, "TOI_together_data_SH")

#dbRemoveTable(db, "team_data_all_sit")
#dbRemoveTable(db, "team_data_EV")
#dbRemoveTable(db, "team_data_PP")
#dbRemoveTable(db, "team_data_SH")

#dbRemoveTable(db, "team_data_5v5")
#dbRemoveTable(db, "team_data_4v4")
#dbRemoveTable(db, "team_data_3v3")

#dbRemoveTable(db, "team_data_5v4")
#dbRemoveTable(db, "team_data_5v3")
#dbRemoveTable(db, "team_data_4v3")

#dbRemoveTable(db, "team_data_4v5")
#dbRemoveTable(db, "team_data_3v5")
#dbRemoveTable(db, "team_data_3v4")

#dbRemoveTable(db, "goalie_games_all_sit")
#dbRemoveTable(db, "adj_pen_games_all_sit")
#dbRemoveTable(db, "game_charts_labels")

#dbDisconnect(db)


###############################



## ------------------------- ##
##   Load Full Joined Data   ##
## ------------------------- ##

###############################

db <- DBI::dbConnect(SQLite(), dbname = "data/NHL_db_1819.sqlite") # NEW DATABASE NAME

pbp_joined <-          db %>% tbl("pbp_full") %>% data.frame()
#shifts_joined <-      db %>% tbl("shifts_full") %>% data.frame()         # not needed here
#rosters_joined <-     db %>% tbl("rosters_full") %>% data.frame()        # not needed here
#game_labels_joined <- db %>% tbl("game_charts_labels") %>% data.frame()  # not needed here

games_all_sit_joined <- db %>% tbl("games_data_all_sit") %>% data.frame()
games_EV_joined <-      db %>% tbl("games_data_EV") %>% data.frame()
games_PP_joined <-      db %>% tbl("games_data_PP") %>% data.frame()
games_SH_joined <-      db %>% tbl("games_data_SH") %>% data.frame()
games_5v5_joined <-     db %>% tbl("games_data_5v5") %>% data.frame()
games_4v4_joined <-     db %>% tbl("games_data_4v4") %>% data.frame()
games_3v3_joined <-     db %>% tbl("games_data_3v3") %>% data.frame()
games_5v4_joined <-     db %>% tbl("games_data_5v4") %>% data.frame()
games_5v3_joined <-     db %>% tbl("games_data_5v3") %>% data.frame()
games_4v3_joined <-     db %>% tbl("games_data_4v3") %>% data.frame()
games_4v5_joined <-     db %>% tbl("games_data_4v5") %>% data.frame()
games_3v5_joined <-     db %>% tbl("games_data_3v5") %>% data.frame()
games_3v4_joined <-     db %>% tbl("games_data_3v4") %>% data.frame()

team_games_all_sit_joined <- db %>% tbl("team_data_all_sit") %>% data.frame()
team_games_EV_joined <-      db %>% tbl("team_data_EV") %>% data.frame()
team_games_PP_joined <-      db %>% tbl("team_data_PP") %>% data.frame()
team_games_SH_joined <-      db %>% tbl("team_data_SH") %>% data.frame()
team_games_5v5_joined <-     db %>% tbl("team_data_5v5") %>% data.frame()
team_games_4v4_joined <-     db %>% tbl("team_data_4v4") %>% data.frame()
team_games_3v3_joined <-     db %>% tbl("team_data_3v3") %>% data.frame()
team_games_5v4_joined <-     db %>% tbl("team_data_5v4") %>% data.frame()
team_games_5v3_joined <-     db %>% tbl("team_data_5v3") %>% data.frame()
team_games_4v3_joined <-     db %>% tbl("team_data_4v3") %>% data.frame()
team_games_4v5_joined <-     db %>% tbl("team_data_4v5") %>% data.frame()
team_games_3v5_joined <-     db %>% tbl("team_data_3v5") %>% data.frame()
team_games_3v4_joined <-     db %>% tbl("team_data_3v4") %>% data.frame()

TOI_together_EV_joined <-  db %>% tbl("TOI_together_data_EV") %>% data.frame()
TOI_together_PP_joined <-  db %>% tbl("TOI_together_data_PP") %>% data.frame()
TOI_together_SH_joined <-  db %>% tbl("TOI_together_data_SH") %>% data.frame()
TOI_together_5v5_joined <- db %>% tbl("TOI_together_data_5v5") %>% data.frame()
TOI_together_5v4_joined <- db %>% tbl("TOI_together_data_5v4") %>% data.frame()
TOI_together_4v5_joined <- db %>% tbl("TOI_together_data_4v5") %>% data.frame()

goalie_games_all_sit_joined <- db %>% tbl("goalie_games_all_sit") %>% data.frame()
adj_pen_games_joined <-        db %>% tbl("adj_pen_games_all_sit") %>% data.frame()

dbDisconnect(db)


###############################




## ------------------- COMPUTE SUMMED TABLES ------------------- ##


## ------------------ ##
##   Sum Count Data   ##
## ------------------ ##

########################

# Run Functions - Totals
counts_all_sit_season <- fun.playercounts_season_all_sit(data = games_all_sit_joined, position_data = player_position)

counts_EV_season <-      fun.playercounts_season_EV(data = games_EV_joined, strength = "EV", per_60 = "F")
counts_PP_season <-      fun.playercounts_season_PP(data = games_PP_joined, strength = "PP", per_60 = "F")
counts_SH_season <-      fun.playercounts_season_SH(data = games_SH_joined, strength = "SH", per_60 = "F")

counts_5v5_season <-     fun.playercounts_season_EV(data = games_5v5_joined, strength = "5v5", per_60 = "F")
counts_4v4_season <-     fun.playercounts_season_EV(data = games_4v4_joined, strength = "4v4", per_60 = "F")
counts_3v3_season <-     fun.playercounts_season_EV(data = games_3v3_joined, strength = "3v3", per_60 = "F")

counts_5v4_season <-     fun.playercounts_season_PP(data = games_5v4_joined, strength = "5v4", per_60 = "F")
counts_5v3_season <-     fun.playercounts_season_PP(data = games_5v3_joined, strength = "5v3", per_60 = "F")
counts_4v3_season <-     fun.playercounts_season_PP(data = games_4v3_joined, strength = "4v3", per_60 = "F")

counts_4v5_season <-     fun.playercounts_season_SH(data = games_4v5_joined, strength = "4v5", per_60 = "F")
counts_3v5_season <-     fun.playercounts_season_SH(data = games_3v5_joined, strength = "3v5", per_60 = "F")
counts_3v4_season <-     fun.playercounts_season_SH(data = games_3v4_joined, strength = "3v4", per_60 = "F")


# Run Functions - per 60 (for SPM calculations)
counts_EV_season_60 <- fun.playercounts_season_EV(data = games_EV_joined, strength = "EV", per_60 = "T")
counts_PP_season_60 <- fun.playercounts_season_PP(data = games_PP_joined, strength = "PP", per_60 = "T")
counts_SH_season_60 <- fun.playercounts_season_SH(data = games_SH_joined, strength = "SH", per_60 = "T")


########################


## ----------------------------- ##
##   Sum Rel_TM / WOWY Metrics   ##
## ----------------------------- ##

##################################

# Run Function
rel_TM_player_EV_list <- fun.relative_teammate(TM_data = TOI_together_EV_joined, 
                                               games_data = games_EV_joined, 
                                               position_data = player_position, 
                                               strength = "even")

rel_TM_player_PP_list <- fun.relative_teammate(TM_data = TOI_together_PP_joined, 
                                               games_data = games_PP_joined, 
                                               position_data = player_position, 
                                               strength = "powerplay")

rel_TM_player_SH_list <- fun.relative_teammate(TM_data = TOI_together_SH_joined, 
                                               games_data = games_SH_joined, 
                                               position_data = player_position, 
                                               strength = "shorthanded")


rel_TM_player_5v5_list <- fun.relative_teammate(TM_data = TOI_together_5v5_joined, 
                                               games_data = games_5v5_joined, 
                                               position_data = player_position, 
                                               strength = "5v5")

rel_TM_player_5v4_list <- fun.relative_teammate(TM_data = TOI_together_5v4_joined, 
                                               games_data = games_5v4_joined, 
                                               position_data = player_position, 
                                               strength = "5v4")

rel_TM_player_4v5_list <- fun.relative_teammate(TM_data = TOI_together_4v5_joined, 
                                               games_data = games_4v5_joined, 
                                               position_data = player_position, 
                                               strength = "4v5")


# Pull out rel_TM data
rel_TM_player_EV_season <- rel_TM_player_EV_list$rel_TM_data
rel_TM_player_PP_season <- rel_TM_player_PP_list$rel_TM_data
rel_TM_player_SH_season <- rel_TM_player_SH_list$rel_TM_data

rel_TM_player_5v5_season <- rel_TM_player_5v5_list$rel_TM_data
rel_TM_player_5v4_season <- rel_TM_player_5v4_list$rel_TM_data
rel_TM_player_4v5_season <- rel_TM_player_4v5_list$rel_TM_data


# Pull out WOWY data and reorder / rename - EV
WOWY_EV_season <- rel_TM_player_EV_list$WOWY_data %>% 
  select(player, teammate, 
         season, 
         Team, position_p, position_t, 
         GP_p, TOI_p, GP_t, TOI_t, TOI_tog, player_TOI_perc_w, GF60_p:xGA60_p, GF60_t:xGA60_t) %>% 
  mutate(player_TOI_perc_w = player_TOI_perc_w * 100) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  data.frame()

# Pull out WOWY data and reorder / rename - PP
WOWY_PP_season <- rel_TM_player_PP_list$WOWY_data %>% 
  select(player, teammate, 
         season, 
         Team, position_p, position_t, 
         TOI_p, TOI_t, TOI_tog, player_TOI_perc_w, GF60_p:xGF60_p, GF60_t:xGF60_t) %>% 
  mutate(player_TOI_perc_w = player_TOI_perc_w * 100) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  data.frame()

# Pull out WOWY data and reorder / rename - SH
WOWY_SH_season <- rel_TM_player_SH_list$WOWY_data %>% 
  select(player, teammate, 
         season, 
         Team, position_p, position_t, 
         TOI_p, TOI_t, TOI_tog, player_TOI_perc_w, GA60_p:xGA60_p, GA60_t:xGA60_t) %>% 
  mutate(player_TOI_perc_w = player_TOI_perc_w * 100) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  data.frame()

# Pull out WOWY data and reorder / rename - 5v5
WOWY_5v5_season <- rel_TM_player_5v5_list$WOWY_data %>% 
  select(player, teammate, 
         season, 
         Team, position_p, position_t, 
         GP_p, TOI_p, GP_t, TOI_t, TOI_tog, player_TOI_perc_w, GF60_p:xGA60_p, GF60_t:xGA60_t) %>% 
  mutate(player_TOI_perc_w = player_TOI_perc_w * 100) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  data.frame()

# Pull out WOWY data and reorder / rename - 5v4
WOWY_5v4_season <- rel_TM_player_5v4_list$WOWY_data %>% 
  select(player, teammate, 
         season, 
         Team, position_p, position_t, 
         TOI_p, TOI_t, TOI_tog, player_TOI_perc_w, GF60_p:xGF60_p, GF60_t:xGF60_t) %>% 
  mutate(player_TOI_perc_w = player_TOI_perc_w * 100) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  data.frame()

# Pull out WOWY data and reorder / rename - 4v5
WOWY_4v5_season <- rel_TM_player_4v5_list$WOWY_data %>% 
  select(player, teammate, 
         season, 
         Team, position_p, position_t, 
         TOI_p, TOI_t, TOI_tog, player_TOI_perc_w, GA60_p:xGA60_p, GA60_t:xGA60_t) %>% 
  mutate(player_TOI_perc_w = player_TOI_perc_w * 100) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  data.frame()


##################################


## --------------------- ##
##   Sum Penalty Goals   ##
## --------------------- ##

###########################

# Per player, season, Team
adj_pen_season <- adj_pen_games_joined %>% 
  group_by(player, position, season, Team) %>% 
  summarise_at(vars(take_count, draw_count, adj_take, adj_draw), funs(sum)) %>% 
  left_join(., rbind(games_all_sit_joined %>% 
                       group_by(player, season, Team) %>% 
                       summarise(TOI = sum(TOI)) %>% 
                       data.frame(),
                     goalie_games_all_sit_joined %>% 
                       group_by(player, season, Team) %>% 
                       summarise(TOI = sum(TOI)) %>% 
                       data.frame()
                     ), 
            by = c("player", "season", "Team")
            ) %>% 
  group_by(position, season) %>% 
  mutate(take_AA = ((adj_take / TOI) - (sum(na.omit(adj_take)) / sum(na.omit(TOI)))) * TOI, 
         draw_AA = ((adj_draw / TOI) - (sum(na.omit(adj_draw)) / sum(na.omit(TOI)))) * TOI, 
         pens =    take_AA + draw_AA
         ) %>% 
  mutate_at(vars(TOI, take_AA:pens), funs(round(., 1))) %>% 
  select(player:Team, TOI, take_count, draw_count, adj_take, adj_draw, take_AA, draw_AA, pens) %>% 
  ungroup() %>% 
  mutate(position = ifelse(position == 1, "F", 
                           ifelse(position == 2, "D", 
                                  ifelse(position == 3, "G", "U")))
         ) %>% 
  rename(TOI_all = TOI) %>% 
  data.frame()


###########################


## -------------------- ##
##   Sum Goalie Stats   ##
## -------------------- ##

##########################

# Run Function
goalie_season <- fun.goalie_sum_all_sit(data = goalie_games_all_sit_joined)


##########################


## ------------------ ##
##   Sum Team Stats   ##
## ------------------ ##

########################

# Run Functions
team_games_sum_all_sit_season <- fun.team_sum_all_sit(data = team_games_all_sit_joined)

team_sum_EV_season <- fun.team_sum_EV(data = team_games_EV_joined, strength = "EV")
team_sum_PP_season <- fun.team_sum_PP(data = team_games_PP_joined, strength = "PP")
team_sum_SH_season <- fun.team_sum_SH(data = team_games_SH_joined, strength = "SH")

team_sum_5v5_season <- fun.team_sum_EV(data = team_games_5v5_joined, strength = "5v5")
team_sum_4v4_season <- fun.team_sum_EV(data = team_games_4v4_joined, strength = "4v4")
team_sum_3v3_season <- fun.team_sum_EV(data = team_games_3v3_joined, strength = "3v3")

team_sum_5v4_season <- fun.team_sum_PP(data = team_games_5v4_joined, strength = "5v4")
team_sum_5v3_season <- fun.team_sum_PP(data = team_games_5v3_joined, strength = "5v3")
team_sum_4v3_season <- fun.team_sum_PP(data = team_games_4v3_joined, strength = "4v3")

team_sum_4v5_season <- fun.team_sum_SH(data = team_games_4v5_joined, strength = "4v5")
team_sum_3v5_season <- fun.team_sum_SH(data = team_games_3v5_joined, strength = "3v5")
team_sum_3v4_season <- fun.team_sum_SH(data = team_games_3v4_joined, strength = "3v4")


########################


## ---------------------- ##
##   TEAM RAPM RUN - EV   ## 
## ---------------------- ##

############################

# Run Functions
team_strength_list_EV <- fun.team_RAPM(data = pbp_joined)
team_strength_RAPM_EV <- team_strength_list_EV$team_data


############################


## ---------------------- ##
##   TEAM RAPM RUN - PP   ##
## ---------------------- ##

############################

# Run Functions
team_strength_list_PP <- fun.team_RAPM_PP_SH(data = pbp_joined)
team_strength_RAPM_PP <- team_strength_list_PP$team_PP
team_strength_RAPM_SH <- team_strength_list_PP$team_SH


############################


## -------------------- ##
##   SPM/GAR/WAR Prep   ##
## -------------------- ##

##########################

## ----------------- ##
##   Goals to Wins   ##
## ----------------- ##

# Goals in all situations
NHL_league_goals <- pbp_joined %>% 
  group_by(game_id, season, home_team, away_team) %>% 
  filter(event_type == "GOAL") %>% 
  summarise(GF_home = sum(event_type == "GOAL" & event_team == home_team & game_period < 5), 
            GF_away = sum(event_type == "GOAL" & event_team == away_team & game_period < 5)
            ) %>% 
  data.frame()

# Calculate Goals to Wins Conversion - in season
goals_to_wins_season <- NHL_league_goals %>% 
  mutate(n = 1) %>% 
  group_by(season) %>% 
  summarise(games = sum(n),  
            GF = (sum(GF_home) + sum(GF_away)) / games / 2, 
            GPW = 4 * GF / 2.091048 # exponent determined in GAR_predictions_2.R script
            ) %>% 
  rename(season_ID = season) %>% 
  data.frame()

rm(NHL_league_goals)


## -------------------------- ##
##   Join Counts and Rel_TM   ##
## -------------------------- ##

all_metrics_season_EV <- counts_EV_season_60 %>% 
  select(-c(t_GF:t_xGA_state)) %>% 
  left_join(., select(rel_TM_player_EV_season, player, season, Team, rel_TM_GF60:rel_TM_xGA60_state), by = c("player", "season", "Team"))


all_metrics_season_PP <- counts_PP_season_60 %>% 
  select(-c(t_GF:t_xGA_state)) %>% 
  left_join(., select(rel_TM_player_PP_season, player, season, Team, rel_TM_GF60:rel_TM_xGF60_state), by = c("player", "season", "Team"))


all_metrics_season_SH <- counts_SH_season_60 %>% 
  select(-c(t_GF:t_xGA_state)) %>% 
  left_join(., select(rel_TM_player_SH_season, player, season, Team, rel_TM_GA60:rel_TM_xGA60_state), by = c("player", "season", "Team"))


## ----------------------- ##
##   Prepare All Sit TOI   ##
## ----------------------- ##

on_ice_all_sit_season <- games_all_sit_joined %>% 
  mutate(GP = 1) %>% 
  group_by(player, season, Team) %>% 
  summarise(TOI_all = sum(TOI), 
            GP = sum(GP)
            ) %>% 
  data.frame()



## ---------------------- ##
##   Prepare Team RAPMs   ##
## ---------------------- ##

team_strength_PP_full_AA <- team_strength_RAPM_PP %>% 
  group_by(season) %>% 
  mutate(PPO_GF_AA_team = ((GF_expand / TOI_PP) - (sum(GF_expand) / sum(TOI_PP))) * 60) %>% 
  select(Team, season, TOI_PP, skaters_PP, PPO_GF, PPO_GF_AA_team) %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>% 
  rename(t_PPO_GF = PPO_GF, 
         t_TOI_PP = TOI_PP
         ) %>% 
  data.frame()

team_strength_SH_full_AA <- team_strength_RAPM_SH %>% 
  group_by(season) %>% 
  mutate(SHD_xG_AA_team = ((xGA_expand / TOI_SH) - (sum(xGA_expand) / sum(TOI_SH))) * 60) %>% 
  select(Team, season, TOI_SH, skaters_SH, SHD_xG, SHD_xG_AA_team) %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>% 
  rename(t_SHD_xG = SHD_xG, 
         t_TOI_SH = TOI_SH
         ) %>% 
  data.frame()



## -------------------------- ##
##   Set Weights & Features   ##
## -------------------------- ##

## -- Weights -- ##

mod_EVO_F_weights <- c("lm" =       0.252551,   "bagEarth" =  0.3448017,  "svmLinear" = 0.4026653)
mod_EVO_D_weights <- c("lm" =       0.37921,    "cubist" =    0.3210417,  "svmLinear" = 0.299772)
mod_EVD_F_weights <- c("bagEarth" = 0.413722,   "svmLinear" = 0.297881,   "glmnet" =    0.288418)
mod_EVD_D_weights <- c("lm" =       0.3688093,  "cubist" =    0.2741847,  "glmnet" =    0.3570327)
mod_PPO_F_weights <- c("cubist" =   0.3791753,  "bagEarth" =  0.3650347,  "svmLinear" = 0.2558133)
mod_PPO_D_weights <- c("lm" =       0.3972313,  "svmLinear" = 0.352244,   "glmnet" =    0.2505447)
mod_SHD_F_weights <- c("cubist" =   0.3101013,  "bagEarth" =  0.3426613,  "svmLinear" = 0.3472623)
mod_SHD_D_weights <- c("cubist" =   0.282119,   "bagEarth" =  0.443778,   "glmnet" =    0.274121)


## -- Features -- ##

# EVO_F
features_EVO_F_small <- c("TOI_perc", "G", "GIVE_d", "TAKE_o", "TAKE_n", "TAKE_d", "iHF_d", "iHA_o", 
                          "OZS_perc", "NZS_perc", "r_FO_perc", "rel_TM_GF60", "rel_TM_SF60")

features_EVO_F_large <- c("TOI_perc", "G", "iSF", "GIVE_o", "GIVE_n", "GIVE_d", "TAKE_o", "TAKE_n", "TAKE_d", 
                          "iHF_n", "iHF_d", "iHA_o", "OZS_perc", "NZS_perc", "r_FO_perc", "rel_TM_GF60", "rel_TM_xGF60", "rel_TM_SF60")

# EVO_D
features_EVO_D_small <- c("A1", "iCF", "GIVE_d", "TAKE_o", "iHA_n", "NZS_perc", "DZS_perc", "rel_TM_GF60", "rel_TM_SF60")

features_EVO_D_large <- c("TOI_perc","A1", "iCF", "GIVE_o", "GIVE_d", "TAKE_o", "TAKE_d", "iHF_o", "iHF_d", "iHA_o", "iHA_d", 
                          "NZS_perc", "DZS_perc", "rel_TM_GF60", "rel_TM_SF60") 

# EVD_F
features_EVD_F_large <- c("TOI_perc", "iBLK", "GIVE_o", "GIVE_d", "TAKE_o", "TAKE_n", "iHF_o", "iHF_d", 
                          "iHA_o", "iHA_d", "NZS_perc", "DZS_perc", "rel_TM_xGA60_state")

# EVD_D
features_EVD_D_large <- c("TOI_perc", "iBLK", "GIVE_o", "GIVE_d", "TAKE_n", "iHF_n", "iHF_d", "iHA_n", "iHA_d", 
                          "OZS_perc", "DZS_perc", "rel_TM_xGA60_state")

features_EVD_D_small <- c("TOI_perc", "iHA_n", "OZS_perc", "DZS_perc", "rel_TM_xGA60_state") 

# PPO_F
features_PPO_F_large <- c("TOI_perc", "A1", "A2", "ixG", "GIVE_all", "TAKE_all", "iHF_all", "iHA_all",   
                          "OZS_perc", "DZS_perc", "r_FO_perc", "rel_TM_GF60", "rel_TM_CF60")

# PPO_D
features_PPO_D_large <- c("TOI_perc", "G", "A1", "A2", "ixG", "GIVE_all", "TAKE_all", "iHF_all", "iHA_all", 
                          "OZS_perc", "DZS_perc", "rel_TM_GF60", "rel_TM_xGF60")

features_PPO_D_small <- c("TOI_perc", "G", "A1", "A2", "OZS_perc", "DZS_perc", "rel_TM_GF60")

# SHD_F
features_SHD_F_large <- c("TOI_perc", "GIVE_all", "TAKE_all", "iHA_all", "OZS_perc", "DZS_perc", "r_FO_perc", "rel_TM_CA60", "rel_TM_xGA60")

# SHD_D 
features_SHD_D_large <- c("TOI_perc", "GIVE_all", "TAKE_all", "iHF_all", "iHA_all", "OZS_perc", "DZS_perc", "rel_TM_CA60",  "rel_TM_xGA60")


# TOI Cutoffs for Predictions
TOI_cut_EV <- 60
TOI_cut_PP <- 25
TOI_cut_SH <- 25



## ------------------------ ##
##   Prepare SPM/GAR Data   ##
## ------------------------ ##

# Specify data
metrics_EV <- all_metrics_season_EV
metrics_PP <- all_metrics_season_PP
metrics_SH <- all_metrics_season_SH


### Even-Strength Offense
EVO_box_rates <- metrics_EV %>% 
  mutate(FO_perc = ifelse(FOW > 0 & FOL > 0, 100 * FOW / (FOW + FOL), 0), 
         r_FO_perc = (FOL + 50) / (FOW + 50)
         ) %>% 
  select(player:Team, 
         TOI,                      ### ADD IN TOI for others as well
         GP, TOI_GP, TOI_perc,
         G, A1, A2, Points, 
         G_adj, A1_adj, A2_adj, Points_adj, 
         iSF, iFF, iCF, ixG, iCF_adj, ixG_adj, 
         GIVE_o:iHA_adj, 
         OZS_perc, NZS_perc, DZS_perc, 
         FO_perc, r_FO_perc,
         rel_TM_GF60, rel_TM_SF60, rel_TM_FF60, rel_TM_CF60, rel_TM_xGF60)


### Even-Strength Defense
EVD_box_rates <- metrics_EV %>% 
  mutate(FO_perc = ifelse(FOW > 0 & FOL > 0, 100 * FOW / (FOW + FOL), 0), 
         r_FO_perc = (FOL + 50) / (FOW + 50)
         ) %>% 
  select(player:Team, 
         TOI,
         GP, TOI_GP, TOI_perc, 
         iBLK, iBLK_adj, 
         GIVE_o:iHA_adj, 
         OZS_perc, NZS_perc, DZS_perc, 
         FO_perc, r_FO_perc, 
         rel_TM_GA60, rel_TM_SA60, rel_TM_FA60, rel_TM_CA60, rel_TM_xGA60, 
         rel_TM_GA60_state, rel_TM_SA60_state, rel_TM_FA60_state, rel_TM_CA60_state, rel_TM_xGA60_state
         )


### Powerplay 
PPO_box_rates <- metrics_PP %>% 
  mutate(FO_perc = ifelse(FOW > 0 & FOL > 0, 100 * FOW / (FOW + FOL), 0), 
         r_FO_perc = (FOL + 50) / (FOW + 50), 
         TOI_perc = 100 * (TOI / (t_TOI + 5.58 * 8)), # determined here: mean(games_PP_full_new_names$t_TOI) / checked with team grouping
         GIVE_all = GIVE_o + GIVE_n + GIVE_d, 
         TAKE_all = TAKE_o + TAKE_n + TAKE_d,
         iHF_all = iHF_o + iHF_n + iHF_d, 
         iHA_all = iHA_o + iHA_n + iHA_d
         ) %>% 
  select(player:Team, 
         TOI,
         GP, TOI_perc, TOI_GP, 
         G, A1, A2, G_adj, A1_adj, A2_adj, Points,
         iSF, iFF, iCF, ixG, iCF_adj, ixG_adj, 
         GIVE_o:iHA_adj, GIVE_all:iHA_all,
         OZS_perc, NZS_perc, DZS_perc, FO_perc, r_FO_perc,
         rel_TM_GF60, rel_TM_SF60, rel_TM_FF60, rel_TM_CF60, rel_TM_xGF60, 
         rel_TM_GF60_state, rel_TM_SF60_state, rel_TM_FF60_state, rel_TM_CF60_state, rel_TM_xGF60_state
         ) 


### Shorthanded
SHD_box_rates <- metrics_SH %>% 
  mutate(FO_perc = ifelse(FOW > 0 & FOL > 0, 100 * FOW / (FOW + FOL), 0), 
         r_FO_perc = (FOL + 50) / (FOW + 50),
         TOI_perc = 100 * (TOI / (t_TOI + 5.58 * 8)), # determined here: mean(games_PP_full_new_names$t_TOI) / checked with team grouping
         GIVE_all = GIVE_o + GIVE_n + GIVE_d, 
         TAKE_all = TAKE_o + TAKE_n + TAKE_d,
         iHF_all = iHF_o + iHF_n + iHF_d, 
         iHA_all = iHA_o + iHA_n + iHA_d 
         ) %>% 
  select(player:Team, 
         TOI,
         GP, TOI_perc, TOI_GP, 
         iBLK, iBLK_adj, 
         GIVE_o:iHA_adj, 
         GIVE_all:iHA_all,
         OZS_perc, NZS_perc, DZS_perc, 
         FO_perc, r_FO_perc, 
         rel_TM_GA60, rel_TM_SA60, rel_TM_FA60, rel_TM_CA60, rel_TM_xGA60, 
         rel_TM_GA60_state, rel_TM_SA60_state, rel_TM_FA60_state, rel_TM_CA60_state, rel_TM_xGA60_state
         )



# EVO: Split F/D
pred_EVO_F_join <- EVO_box_rates %>% filter(position == 1)
pred_EVO_D_join <- EVO_box_rates %>% filter(position == 2)

# EVD: Split F/D
pred_EVD_F_join <- EVD_box_rates %>% filter(position == 1)
pred_EVD_D_join <- EVD_box_rates %>% filter(position == 2)

# PPO: Split F/D
pred_PPO_F_join <- PPO_box_rates %>% filter(position == 1)
pred_PPO_D_join <- PPO_box_rates %>% filter(position == 2)

# SHD: Split F/D
pred_SHD_F_join <- SHD_box_rates %>% filter(position == 1)
pred_SHD_D_join <- SHD_box_rates %>% filter(position == 2)




# Remove Excess
rm(EVO_box_rates, EVD_box_rates, metrics_EV, PPO_box_rates, metrics_PP, SHD_box_rates, metrics_SH)
gc()


##########################


## ------------------------------------- ##
##   SPM/GAR/WAR Predictions / Combine   ##
## ------------------------------------- ##

###########################################

# Even-Strength Function
ALL_EV <- fun.ALL_EV_GAA()

# Powerplay Offense Function
ALL_PP <- fun.ALL_PP_GAA()

# Shorthanded Defense Function
ALL_SH <- fun.ALL_SH_GAA()


## ------------------------------------ ##
##   Combine All / Create Final Table   ##
## ------------------------------------ ##

# Objects - new replacement level as of 12/12/18 *** deprecated
#EV_rep_F <-   round(0.1911043, 4)
#EV_rep_D <-   round(0.1191875, 4)

#PP_rep_F <-   round(0.3864802, 4)
#PP_rep_D <-   round(0.3594602, 4)

#SH_rep_F <-   round(-0.0411400, 5)
#SH_rep_D <-   round(0.07133463, 5)

#Pens_rep_F <- round(0.03211396, 5)
#Pens_rep_D <- round(0.02519755, 5)

# Objects - new replacement level as of 1/16/19
EV_rep_F <-   round(0.1878387, 4)
EV_rep_D <-   round(0.1163090, 4)

PP_rep_F <-   round(0.3865902, 4)
PP_rep_D <-   round(0.3594252, 4)

SH_rep_F <-   round(-0.04115749, 5)
SH_rep_D <-   round(0.07122438, 5)

Pens_rep_F <- round(0.03211396, 5)
Pens_rep_D <- round(0.02519755, 5)


# Final GAA + GAR Join
ALL_GAR <- on_ice_all_sit_season %>% 
  left_join(., ALL_EV %>% select(player, season, Team, TOI_EV, TOI_perc_EV:adj_EVD_total, EVO_AA_adj, EVD_AA_adj), by = c("player", "season", "Team")) %>% 
  left_join(., ALL_PP %>% select(player, season, Team, TOI_PP, TOI_perc_PP:adj_PPO_total, PPO_AA_adj), by = c("player", "season", "Team")) %>% 
  left_join(., ALL_SH %>% select(player, season, Team, TOI_SH, TOI_perc_SH:adj_SHD_total, SHD_AA_adj), by = c("player", "season", "Team")) %>% 
  left_join(., adj_pen_season %>% select(player, season, Team, take_AA, draw_AA, pens_AA = pens), by = c("player", "season", "Team")) %>% 
  left_join(., player_position, by = "player") %>% 
  filter(!is.na(position)) %>% 
  
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
  mutate(GAA = EVO_AA_adj + EVD_AA_adj + PPO_AA_adj + SHD_AA_adj + pens_AA) %>% 
  
  select(player, position, season, Team, GP, TOI_all, TOI_EV, TOI_PP, TOI_SH, TOI_perc_EV, TOI_perc_PP, TOI_perc_SH, 
         SPM_EVO_60, SPM_EVD_60, SPM_PPO_60, SPM_SHD_60, SPM_EVO_AA_60, SPM_EVD_AA_60, SPM_PPO_AA_60, SPM_SHD_AA_60,
         team_TOI_EV, team_TOI_PP, team_TOI_SH, team_skaters_EV, team_skaters_PP, team_skaters_SH, team_EV_GF, team_EV_xGA, team_PPO_GF_AA, team_SHD_xGA_AA, 
         adj_EVO_60, adj_EVD_60, adj_PPO_60, adj_SHD_60, 
         SPM_EVO_AA, SPM_EVD_AA, SPM_PPO_AA, SPM_SHD_AA, 
         adj_EVO_total, adj_EVD_total, adj_PPO_total, adj_SHD_total, 
         EVO_AA = EVO_AA_adj, 
         EVD_AA = EVD_AA_adj, 
         PPO_AA = PPO_AA_adj, 
         SHD_AA = SHD_AA_adj, 
         take_AA, draw_AA, pens_AA, 
         GAA
         ) %>% 
  
  # Goals Above Replacement
  mutate(EV_GAA = EVO_AA + EVD_AA, 
         EV_GAR =   ifelse(position == 1, EV_GAA +  (TOI_EV * (EV_rep_F / 60)), EV_GAA + (TOI_EV * (EV_rep_D / 60))), 
         PP_GAR =   ifelse(position == 1, PPO_AA +  (TOI_PP * (PP_rep_F / 60)), PPO_AA + (TOI_PP * (PP_rep_D / 60))), 
         SH_GAR =   ifelse(position == 1, SHD_AA +  (TOI_SH * (SH_rep_F / 60)), SHD_AA + (TOI_SH * (SH_rep_D / 60))), 
         Pens_GAR = ifelse(position == 1, pens_AA + (TOI_all * (Pens_rep_F / 60)), pens_AA + (TOI_all * (Pens_rep_D / 60))), 
         GAR = EV_GAR + PP_GAR + SH_GAR + Pens_GAR, 
         WAR = GAR / goals_to_wins_season$GPW[match(season, goals_to_wins_season$season_ID)] # determined in GAR_predicitions_2.R script
         ) %>% 
  mutate_at(vars(TOI_perc_EV:TOI_perc_SH), funs(100 * round(., 3))) %>% 
  mutate_at(vars(SPM_EVO_60:SPM_SHD_AA_60, adj_EVO_60:adj_SHD_60), funs(round(., 3))) %>% 
  mutate_at(vars(team_skaters_EV:team_skaters_SH, team_EV_GF:team_SHD_xGA_AA, SPM_EVO_AA:SPM_SHD_AA, adj_EVO_total:adj_SHD_total), funs(round(., 2))) %>% 
  mutate_at(vars(TOI_all:TOI_SH, team_TOI_EV:team_TOI_SH, EVO_AA:SHD_AA, GAA:WAR), funs(round(., 1))) %>% 
  data.frame()

# Split
#GAA_split <- ALL_GAR %>% select(player:GAA)
#GAR_split <- ALL_GAR %>% select(player:TOI_SH, EV_GAR:WAR)



# Remove Excess
rm(ALL_EV, ALL_PP, ALL_SH)
gc()


###########################################


## ---------------------------- ##
##   Goalie & Shooter GAR/WAR   ##
## ---------------------------- ##

##################################

# Run function
shooting_RAPM_All_Sit_list <- fun.shooting_RAPM(pbp_data = pbp_joined, strength_ = "All")

goalie_GAR_in_season <- shooting_RAPM_All_Sit_list$goalie_df
shooter_GAR_in_season <- shooting_RAPM_All_Sit_list$shooter_df


## ----------- ##
##   Goalies   ##
## ----------- ##

# Current replacement level (3rd goalie and below per team, per team per season, 10-fold cv)
#goalie_rep <- -0.007312411
goalie_rep <- -0.007315805

# Convert to GAR / WAR
goalie_GAR_in_season_final <- goalie_GAR_in_season %>% 
  left_join(., goals_to_wins_season %>% rename(season = season_ID) %>% select(season, GPW), by = "season") %>% 
  mutate(GAR = GAA + FA * -goalie_rep, 
         WAR = GAR / GPW
         ) %>% 
  mutate_at(vars(GAA, GAR, WAR), funs(round(., 1))) %>% 
  select(player, position, season, Team, FA, coefficient, prob, GAA, GAR, WAR) %>% 
  data.frame()


## ------------ ##
##   Shooters   ##
## ------------ ##

# Shooter Teams (unused for final)
teams_shooter <- ALL_GAR %>% 
  group_by(player, Team, season) %>% 
  summarise() %>% 
  group_by(player, season) %>% 
  mutate(Team = paste0(Team, collapse = "/")) %>% 
  summarise(Team = first(Team)) %>% 
  data.frame()

# Shooter Replacement Level Per Position
#shooter_rep_F <- -0.007263538
#shooter_rep_D <- -0.01033
shooter_rep_F <- -0.006706953
shooter_rep_D <- -0.011937867

# Shooter GAR Final data.frame
shooter_GAR_in_season_final <- shooter_GAR_in_season %>% 
  left_join(., teams_shooter, by = c("player", "season")) %>% 
  mutate(GAR = GAA + iFF * -ifelse(position == 2, shooter_rep_D, shooter_rep_F)) %>% 
  mutate_at(vars(GAA, GAR), funs(round(., 1))) %>% 
  select(player, position, season, Team, iFF, coefficient, prob, GAA, GAR) %>% 
  data.frame()


# remove excess data
rm(goalie_GAR_in_season, goalie_rep, shooter_GAR_in_season, shooter_rep_F, shooter_rep_D)


##################################


## -------------------- ##
##   Skater RAPM - EV   ##
## -------------------- ##

##########################

# Run RAPM Function
RAPM_EV_list <- fun.RAPM_EV_all(pbp_data = pbp_joined, games_data = games_EV_joined)
RAPM_EV_in_season_rates <- RAPM_EV_list$RAPM_EV_rates
RAPM_EV_in_season_impact <- RAPM_EV_list$RAPM_EV_impact


##########################


## ----------------------- ##
##   Skater RAPM - PP/SH   ##
## ----------------------- ##

#############################

# Run Function
RAPM_PP_list <- fun.RAPM_PP_SH_all(pbp_data = pbp_joined, games_data_PP = games_PP_joined, games_data_SH = games_SH_joined)
RAPM_PP_in_season_rates <- RAPM_PP_list$RAPM_PP_rates
RAPM_PP_in_season_impact <- RAPM_PP_list$RAPM_PP_impact


#############################


## ------------------------------ ##
##   Game Charts - Avg Per Game   ##
## ------------------------------ ##

####################################

# Average Corsi / xG Per Game - All Sit 
average_shots <- rbind(
  pbp_joined %>% 
  filter(game_period < 5, 
         event_type %in% st.corsi_events
         ) %>% 
  group_by(game_id, season, Team = home_team) %>% 
  summarise(CF_all =  sum((event_type %in% st.corsi_events & event_team == home_team)), 
            xGF_all = sum(na.omit((event_team == home_team) * pred_XGB_7))
            ) %>% 
  data.frame(),
  
  pbp_joined %>% 
    filter(game_period < 5, 
           event_type %in% st.corsi_events
           ) %>% 
    group_by(game_id, season, Team = away_team) %>% 
    summarise(CF_all =  sum((event_type %in% st.corsi_events & event_team == away_team)), 
              xGF_all = sum(na.omit((event_team == away_team) * pred_XGB_7))
              ) %>% 
    data.frame()
  ) %>% 
  group_by(season) %>% 
  summarise(mean_CF_all =  mean(CF_all), 
            mean_xGF_all = mean(xGF_all), 
            sd_CF_all =    sd(CF_all), 
            sd_xGF_all =   sd(xGF_all)
            ) %>% 
  data.frame()


####################################





## ----------------------- SAVE SUMMED TABLES ----------------------- ##


## --------------------------- ##
##   Save Sum Data / Cleanup   ##
## --------------------------- ##

#################################

# Combine all summed data into list for saving / transfer

in_season_sums_list <- list(# Skater Standard Stats
                            counts_all_sit_season = counts_all_sit_season, 
                            counts_EV_season =  counts_EV_season, 
                            counts_PP_season =  counts_PP_season, 
                            counts_SH_season =  counts_SH_season, 
                            counts_5v5_season = counts_5v5_season, 
                            counts_4v4_season = counts_4v4_season, 
                            counts_3v3_season = counts_3v3_season, 
                            counts_5v4_season = counts_5v4_season, 
                            counts_5v3_season = counts_5v3_season, 
                            counts_4v3_season = counts_4v3_season, 
                            counts_4v5_season = counts_4v5_season, 
                            counts_3v5_season = counts_3v5_season, 
                            counts_3v4_season = counts_3v4_season, 
                            
                            # Rel_TM Data
                            rel_TM_player_EV_season =  rel_TM_player_EV_season, 
                            rel_TM_player_PP_season =  rel_TM_player_PP_season, 
                            rel_TM_player_SH_season =  rel_TM_player_SH_season, 
                            rel_TM_player_5v5_season = rel_TM_player_5v5_season, 
                            rel_TM_player_5v4_season = rel_TM_player_5v4_season, 
                            rel_TM_player_4v5_season = rel_TM_player_4v5_season, 
                            
                            # WOWY Data
                            WOWY_EV_season =  WOWY_EV_season, 
                            WOWY_PP_season =  WOWY_PP_season, 
                            WOWY_SH_season =  WOWY_SH_season, 
                            WOWY_5v5_season = WOWY_5v5_season, 
                            WOWY_5v4_season = WOWY_5v4_season, 
                            WOWY_4v5_season = WOWY_4v5_season, 
                            
                            # Team Standard Stats
                            team_sum_all_sit_season = team_games_sum_all_sit_season, 
                            team_sum_EV_season =  team_sum_EV_season, 
                            team_sum_PP_season =  team_sum_PP_season, 
                            team_sum_SH_season =  team_sum_SH_season, 
                            team_sum_5v5_season = team_sum_5v5_season, 
                            team_sum_4v4_season = team_sum_4v4_season, 
                            team_sum_3v3_season = team_sum_3v3_season, 
                            team_sum_5v4_season = team_sum_5v4_season, 
                            team_sum_5v3_season = team_sum_5v3_season, 
                            team_sum_4v3_season = team_sum_4v3_season, 
                            team_sum_4v5_season = team_sum_4v5_season, 
                            team_sum_3v5_season = team_sum_3v5_season, 
                            team_sum_3v4_season = team_sum_3v4_season, 
                            
                            # Team RAPMs
                            team_strength_RAPM_EV = team_strength_RAPM_EV, 
                            team_strength_RAPM_PP = team_strength_RAPM_PP, 
                            team_strength_RAPM_SH = team_strength_RAPM_SH, 
                            
                            # Penalty Goals Data
                            adj_pen_season = adj_pen_season, 
                            
                            # Goalie Standard Stats
                            goalie_season = goalie_season, 
                            
                            # Goals to Wins In Season
                            goals_to_wins_season = goals_to_wins_season, 
                            
                            # GAR/WAR In Season - Skaters
                            ALL_GAR = ALL_GAR, 
                            
                            # GAR/WAR In Season - Goalies
                            goalie_GAR_in_season_final = goalie_GAR_in_season_final, 
                            
                            # Shooter GAR In Season
                            shooter_GAR_in_season_final = shooter_GAR_in_season_final, 
                            
                            # Skater RAPMS - EV
                            RAPM_EV_in_season_rates = RAPM_EV_in_season_rates, 
                            RAPM_EV_in_season_impact = RAPM_EV_in_season_impact, 
                            
                            # Skater RAPMS - PP/SH
                            RAPM_PP_in_season_rates = RAPM_PP_in_season_rates, 
                            RAPM_PP_in_season_impact = RAPM_PP_in_season_impact, 
                            
                            # Average Shots In-Season
                            average_shots_in_season = average_shots, 
                            
                            # Player Position
                            player_position = player_position, 
                            
                            # Last Updated Time
                            last_update = as.character(Sys.time()), 
                            
                            # Check new games data.frame
                            last_scrape_check = check_new_games
                            )



# SAVE Running List Data
saveRDS(in_season_sums_list, "data/in_season_sums_list.rds")
saveRDS(RAPM_EV_list, "data/RAPM_EV_list_in_season.rds")
saveRDS(RAPM_PP_list, "data/RAPM_PP_list_in_season.rds")
saveRDS(shooting_RAPM_All_Sit_list, "data/shooting_RAPM_All_Sit_list.rds")


saveRDS(in_season_sums_list, "data/in_season_sums_list_1819_p.rds")

#################################












