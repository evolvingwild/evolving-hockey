#####################################################################################
#####           SHINY All Functions           ||             09/22/18           #####
#####################################################################################

# Basic Functions
standardize <- function(metric) {
  
  x <- (metric - mean(na.omit(metric))) / sd(na.omit(metric))
  
}
peek <- function(object, number) { 
  
  head(colnames(object), number)
  
}



## ------------------------------------------------------ ##



## ------------ ##
##   xG Model   ##
## ------------ ##

##################

fun.pbp_expand <- function(data) {
  
  data$event_description <- as.character(data$event_description)
  data$coords_x <- as.numeric(as.character(data$coords_x))
  data$coords_y <- as.numeric(as.character(data$coords_y))
  
  print("expand", quote = F) 
  
  hold <- data %>% 
    # Manny's enhanced pbp functions
    mutate(event_circle = 
             1 * (coords_x <= -25 & coords_y > 0) + 
             2 * (coords_x <= -25 & coords_y < 0) + 
             3 * (coords_x < 0 & coords_x > 25 & coords_y > 0) + 
             4 * (coords_x < 0 & coords_x > 25 & coords_y < 0) +
             5 * (abs(coords_x) < 5 & abs(coords_y) < 5) + 
             6 * (coords_x > 0 & coords_x < 25 & coords_y > 0) +
             7 * (coords_x > 0 & coords_x < 25 & coords_y < 0) + 
             8 * (coords_x >= 25 & coords_y > 0) +
             9 * (coords_x >= 25 & coords_y < 0), 
           
           event_rinkside = ifelse(coords_x <= -25, "L",
                                   ifelse(coords_x > -25 & coords_x < 25, "N", 
                                          ifelse(coords_x >= 25, "R", NA))),
           
           event_zone = ifelse((grepl("off. zone", tolower(event_description)) == TRUE), "Off", 
                               ifelse((grepl("neu. zone", tolower(event_description)) == TRUE), "Neu", 
                                      ifelse((grepl("def. zone", tolower(event_description)) == TRUE), "Def", NA))),  
           
           event_zone = ifelse(event_zone == "Def" & event_type == "BLOCK", "Off", event_zone), 
           
           home_zone = ifelse(event_team == away_team & event_zone == "Off", "Def", 
                              ifelse(event_team == away_team & event_zone == "Def", "Off", event_zone)), 
           
           # Initial distance / angle calculation
           pbp_distance = suppressWarnings(as.numeric(sub(".*Zone, *(.*?) * ft.*", "\\1", event_description))), 
           pbp_distance = ifelse(event_type %in% st.fenwick_events & is.na(pbp_distance), 0, pbp_distance), 
           
           event_distance = sqrt((89 - abs(coords_x))^2 + coords_y^2), 
           
           event_angle = abs(atan(coords_y / (89 - abs(coords_x))) * (180 / pi)), 
           
           # Update distance calc for long shots (and various mistakes)
           event_distance = ifelse(event_type %in% st.fenwick_events & pbp_distance > 89 & coords_x < 0 & 
                                     event_detail != "Tip-In" & event_detail != "Wrap-around" & event_detail != "Deflected" & !(pbp_distance > 89 & event_zone == "Off"), 
                                   sqrt((abs(coords_x) + 89)^2 + coords_y^2), 
                                   
                                   ifelse(event_type %in% st.fenwick_events & pbp_distance > 89 & coords_x > 0 & 
                                            event_detail != "Tip-In" & event_detail != "Wrap-around" & event_detail != "Deflected" & !(pbp_distance > 89 & event_zone == "Off"), 
                                          sqrt((coords_x + 89)^2 + coords_y^2), 
                                          event_distance)),  
           
           event_angle = ifelse(event_type %in% st.fenwick_events & pbp_distance > 89 & coords_x < 0 & 
                                  event_detail != "Tip-In" & event_detail != "Wrap-around" & event_detail != "Deflected" & !(pbp_distance > 89 & event_zone == "Off"), 
                                abs( atan(coords_y / (abs(coords_x) + 89)) * (180 / pi)), 
                                
                                ifelse(event_type %in% st.fenwick_events & pbp_distance > 89 & coords_x > 0 & 
                                         event_detail != "Tip-In" & event_detail != "Wrap-around" & event_detail != "Deflected" & !(pbp_distance > 89 & event_zone == "Off"), 
                                       abs(atan(coords_y / (coords_x + 89)) * (180 / pi)), 
                                       event_angle)), 
           
           event_zone = ifelse(event_type %in% st.fenwick_events & event_zone == "Def" & pbp_distance <= 64, "Off", event_zone), 
           
           # Update penalty shot strength states
           game_strength_state = ifelse((grepl("penalty shot", tolower(event_description)) == TRUE) & event_team == home_team, "Ev1", 
                                        ifelse((grepl("penalty shot", tolower(event_description)) == TRUE) & event_team == away_team, "1vE", game_strength_state)), 
           
           home_skaters = ifelse((grepl("penalty shot", tolower(event_description)) == TRUE) & event_team == home_team, 1, 
                                 ifelse((grepl("penalty shot", tolower(event_description)) == TRUE) & event_team == away_team, 0, home_skaters)), 
           
           away_skaters = ifelse((grepl("penalty shot", tolower(event_description)) == TRUE) & event_team == home_team, 0, 
                                 ifelse((grepl("penalty shot", tolower(event_description)) == TRUE) & event_team == away_team, 1, away_skaters))
           )
  
  print("face_ID", quote = F)  
  
  # Add home_zonestart for corsi events
  face_index <- hold %>% 
    filter(event_type %in% c(st.corsi_events, "FAC"),
           game_period < 5
           ) %>%
    arrange(game_id, event_index) %>%
    mutate(face_index = cumsum(event_type == "FAC")) %>%
    group_by(game_id, face_index) %>%
    arrange(event_index) %>% 
    mutate(test = first(home_zone),  
           home_zonestart = ifelse(first(home_zone) == "Def", 1, 
                                   ifelse(first(home_zone) == "Neu", 2, 
                                          ifelse(first(home_zone) == "Off", 3, NA)))
           ) %>%
    ungroup() %>% 
    select(game_id, event_index, home_zonestart) %>% 
    data.frame()
  
  # Join
  print("join", quote = F) 
  print("---", quote = F)
  
  hold <- left_join(hold, face_index, by = c("game_id", "event_index"))
  
  }
fun.pbp_index <- function(data) { 
  
  # Add shift/face/pen indexes & shift IDs
  
  print("index", quote = F)
  
  pbp_hold <- data %>% 
    arrange(game_id, event_index) %>% 
    mutate(face_index =  cumsum(event_type == "FAC"), 
           shift_index = cumsum(event_type == "ON"), 
           pen_index =   cumsum(event_type == "PENL"))
  
  
  print("shift_ID", quote = F)
  
  hold <- pbp_hold %>% 
    filter(event_type %in% c("FAC", "GOAL", "BLOCK", "SHOT", "MISS", "HIT", "TAKE", "GIVE"), 
           game_period < 5
           ) %>% 
    group_by(game_id, game_period, season,
             home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
             away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, 
             home_goalie, away_goalie, 
             face_index, shift_index, pen_index
             ) %>% 
    mutate(shift_ID = round(first(event_index) * as.numeric(game_id))) %>% 
    summarise(shift_ID = first(shift_ID), 
              shift_length = last(game_seconds) - first(game_seconds)) %>% 
    ungroup() %>% 
    select(game_id, game_period, season, shift_ID, face_index, 
           shift_index, pen_index, shift_length, home_on_1:away_goalie
           ) %>% 
    data.frame()
  
  
  print("join", quote = F)
  print("---", quote = F)
  
  join <- left_join(pbp_hold, hold, by = c("game_id", "game_period", "season",
                                           "home_on_1", "home_on_2", "home_on_3", 
                                           "home_on_4", "home_on_5", "home_on_6", 
                                           "away_on_1", "away_on_2", "away_on_3", 
                                           "away_on_4","away_on_5", "away_on_6", 
                                           "home_goalie", "away_goalie", 
                                           "face_index", "shift_index", "pen_index"))
  }
fun.pbp_prep <- function(data, prep_type) {
  
  # Prep pbp for xG models (EV or UE) 
  
  if (prep_type == "EV") { 
    
    # Prep for EV xG model
    pbp_prep_EV <- data %>% 
      filter(event_type %in% c("FAC", "GOAL", "BLOCK", "SHOT", "MISS", "HIT", "TAKE", "GIVE"), 
             game_period < 5, 
             !(grepl("penalty shot", tolower(event_description))), 
             !is.na(coords_x),
             !is.na(coords_y), 
             !(coords_x == 0 & coords_y == 0 & event_type %in% st.corsi_events & 
                 (pbp_distance != 90 & event_type %in% st.fenwick_events))
             ) %>% 
      group_by(season, game_id, game_period) %>%
      arrange(event_index) %>% 
      mutate(seconds_since_last =  game_seconds - lag(game_seconds),
             event_type_last =     lag(event_type),
             event_team_last =     lag(event_team),
             event_strength_last = lag(game_strength_state), 
             coords_x_last =       lag(coords_x),
             coords_y_last =       lag(coords_y)
             ) %>%
      ungroup() %>%
      arrange(season, game_id, event_index) %>% 
      filter(event_type %in% st.fenwick_events, 
             game_strength_state %in% st.even_strength, 
             !is.na(coords_x_last), 
             !is.na(coords_y_last)
             ) %>% 
      # Create priors
      mutate(same_team_last =     1 * (event_team == event_team_last),
             is_home =            1 * (event_team == home_team),
             score_state =        ifelse(is_home == 1, home_score - away_score, away_score - home_score), 
             event_detail =       ifelse(is.na(event_detail), "Wrist", event_detail), 
             distance_from_last = sqrt((coords_x - coords_x_last)^2 + (coords_y - coords_y_last)^2), 
             
             # Flurry indicator
             flurry = 1 * (shift_ID == lag(shift_ID, default = 0) & event_team == lag(event_team)), 
             # Probably a better way to do this...
             flurry_1 = ifelse(flurry == 1 & lag(flurry) == 1, 2, flurry), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 2, 3, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 3, 4, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 4, 5, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 5, 6, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 6, 7, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 7, 8, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 8, 9, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 9, 10, flurry_1)
             ) %>%
      select(-c(flurry)) %>% 
      rename(shot_distance = event_distance, 
             shot_angle = event_angle,
             flurry = flurry_1
             ) %>% 
      select(game_id, event_index, season, game_date, game_period, game_seconds, 
             game_strength_state, game_score_state, score_state, is_home, 
             event_player_1, home_goalie, away_goalie, 
             home_score, away_score, home_team, away_team, home_skaters, away_skaters, 
             event_description, event_team, event_type, event_detail, 
             coords_x, coords_y, pbp_distance, shot_distance, shot_angle,
             event_team_last, same_team_last, event_strength_last, event_type_last, 
             seconds_since_last, distance_from_last, coords_x_last, coords_y_last,
             shift_ID, shift_length, flurry
             ) %>% 
      data.frame()
    
    }
  else if (prep_type == "UE") { 
    
    # Prep for UE xG model
    pbp_prep_UE <- data %>% 
      filter(event_type %in% c("FAC", "GOAL", "BLOCK", "SHOT", "MISS", "HIT", "TAKE", "GIVE"), 
             game_period < 5, 
             !(grepl("penalty shot", tolower(event_description))), 
             !is.na(coords_x),
             !is.na(coords_y), 
             !(coords_x == 0 & coords_y == 0 & event_type %in% st.corsi_events & 
                 (pbp_distance != 90 & event_type %in% st.fenwick_events))
             ) %>% 
      group_by(season, game_id, game_period) %>%
      arrange(event_index) %>% 
      mutate(seconds_since_last =  game_seconds - lag(game_seconds),
             event_type_last =     lag(event_type),
             event_team_last =     lag(event_team),
             event_strength_last = lag(game_strength_state), 
             coords_x_last =       lag(coords_x),
             coords_y_last =       lag(coords_y),
             true_strength_state = paste0(home_skaters, "v", away_skaters)
             ) %>% 
      group_by(season, game_id, pen_index) %>% 
      mutate(pen_seconds_since = (game_strength_state %in% st.pp_strength) * (game_seconds - first(game_seconds)), 
             pen_seconds_since = ifelse(pen_seconds_since > 0 & pen_seconds_since >= 300, 120, pen_seconds_since)
             ) %>% 
      ungroup() %>% 
      arrange(season, game_id, event_index) %>% 
      filter(event_type %in% st.fenwick_events, 
             
             (event_team == home_team & (game_strength_state %in% c("5v4", "5v3", "4v3") | true_strength_state %in% c("6v5", "6v4"))) | 
               (event_team == away_team & (game_strength_state %in% c("4v5", "3v5", "3v4") | true_strength_state %in% c("5v6", "4v6"))), 
             
             !is.na(coords_x_last), 
             !is.na(coords_y_last) 
             ) %>% 
      # Create priors
      mutate(same_team_last =     1 * (event_team == event_team_last),
             is_home =            1 * (event_team == home_team),
             score_state =        ifelse(is_home == 1, home_score - away_score, away_score - home_score), 
             event_detail =       ifelse(is.na(event_detail), "Wrist", event_detail), 
             distance_from_last = sqrt((coords_x - coords_x_last)^2 + (coords_y - coords_y_last)^2), 
             prior_event_EV =     1 * (event_strength_last %in% st.even_strength), 
             
             # Flurry indicator
             flurry = 1 * (shift_ID == lag(shift_ID, default = 0) & event_team == lag(event_team)), 
             # Probably a better way to do this...
             flurry_1 = ifelse(flurry == 1 & lag(flurry) ==   1,  2, flurry), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 2,  3, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 3,  4, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 4,  5, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 5,  6, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 6,  7, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 7,  8, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 8,  9, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 9,  10, flurry_1), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 10, 11, flurry_1), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 11, 12, flurry_1), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 12, 13, flurry_1), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 13, 14, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 14, 15, flurry_1)
             ) %>%
      select(-c(flurry)) %>% 
      rename(shot_distance = event_distance, 
             shot_angle = event_angle,
             flurry = flurry_1
             ) %>% 
      select(game_id, event_index, season, game_date, game_period, game_seconds, 
             game_strength_state, true_strength_state, game_score_state, score_state, is_home, 
             event_player_1, home_goalie, away_goalie, 
             home_score, away_score, home_team, away_team, home_skaters, away_skaters, 
             event_description, event_team, event_type, event_detail, 
             coords_x, coords_y, pbp_distance, shot_distance, shot_angle,
             event_team_last, same_team_last, event_strength_last, prior_event_EV, event_type_last, 
             seconds_since_last, pen_seconds_since, distance_from_last, coords_x_last, coords_y_last,
             shift_ID, shift_length, flurry
             ) %>% 
      data.frame()
    
    }
  else if (prep_type == "SH") { 
    
    # Prep for SH xG model
    pbp_prep_SH <- data %>% 
      filter(event_type %in% c("FAC", "GOAL", "BLOCK", "SHOT", "MISS", "HIT", "TAKE", "GIVE"), 
             game_period < 5, 
             !(grepl("penalty shot", tolower(event_description))), 
             !is.na(coords_x),
             !is.na(coords_y), 
             !(coords_x == 0 & coords_y == 0 & event_type %in% st.corsi_events & 
                 (pbp_distance != 90 & event_type %in% st.fenwick_events))
             ) %>% 
      group_by(season, game_id, game_period) %>%
      arrange(event_index) %>% 
      mutate(seconds_since_last =  game_seconds - lag(game_seconds),
             event_type_last =     lag(event_type),
             event_team_last =     lag(event_team),
             event_strength_last = lag(game_strength_state), 
             coords_x_last =       lag(coords_x),
             coords_y_last =       lag(coords_y),
             shift_ID =            as.numeric(shift_ID)
             ) %>% 
      group_by(season, game_id, pen_index) %>% 
      mutate(pen_seconds_since = (game_strength_state %in% st.pp_strength) * (game_seconds - first(game_seconds)), 
             pen_seconds_since = ifelse(pen_seconds_since > 0 & pen_seconds_since >= 300, 120, pen_seconds_since)
             ) %>% 
      ungroup() %>% 
      arrange(season, game_id, event_index) %>% 
      filter(event_type %in% st.fenwick_events, 
             event_team == away_team & game_strength_state %in% c("5v4", "5v3", "4v3") | 
               event_team == home_team & game_strength_state %in% c("4v5", "3v5", "3v4"), 
             !is.na(coords_x_last), 
             !is.na(coords_y_last)
             ) %>% 
      mutate(same_team_last =     1 * (event_team == event_team_last),
             is_home =            1 * (event_team == home_team),
             score_state =        ifelse(is_home == 1, home_score - away_score, away_score - home_score), 
             event_detail =       ifelse(is.na(event_detail), "Wrist", event_detail), 
             distance_from_last = sqrt((coords_x - coords_x_last)^2 + (coords_y - coords_y_last)^2), 
             prior_event_EV =     1 * (event_strength_last %in% st.even_strength), 
             
             # Flurry indicator
             flurry = 1 * (shift_ID == lag(shift_ID, default = 0) & event_team == lag(event_team)),
             # Probably a better way to do this...
             flurry_1 = ifelse(flurry == 1 & lag(flurry) ==   1,  2, flurry), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 2,  3, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 3,  4, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 4,  5, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 5,  6, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 6,  7, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 7,  8, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 8,  9, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 9,  10, flurry_1), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 10, 11, flurry_1), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 11, 12, flurry_1), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 12, 13, flurry_1), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 13, 14, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 14, 15, flurry_1)
             ) %>%
      select(-c(flurry)) %>% 
      rename(shot_distance = event_distance, 
             shot_angle = event_angle,
             flurry = flurry_1
             ) %>% 
      select(game_id, event_index, season, game_date, game_period, game_seconds, 
             game_strength_state, game_score_state, score_state, is_home, 
             event_player_1, home_goalie, away_goalie, 
             home_score, away_score, home_team, away_team, home_skaters, away_skaters, 
             event_description, event_team, event_type, event_detail, 
             coords_x, coords_y, pbp_distance, shot_distance, shot_angle,
             event_team_last, same_team_last, event_strength_last, prior_event_EV, event_type_last, 
             seconds_since_last, pen_seconds_since, distance_from_last, coords_x_last, coords_y_last,
             shift_ID, shift_length, flurry
             ) %>% 
      data.frame()
    
    }
  else if (prep_type == "EN") { 
    
    # Prep for EN xG model
    pbp_prep_EN <- data %>% 
      filter(event_type %in% c("FAC", "GOAL", "BLOCK", "SHOT", "MISS", "HIT", "TAKE", "GIVE"), 
             game_period < 5, 
             !(grepl("penalty shot", tolower(event_description))), 
             !is.na(coords_x),
             !is.na(coords_y), 
             !(coords_x == 0 & coords_y == 0 & event_type %in% st.corsi_events & 
                 (pbp_distance != 90 & event_type %in% st.fenwick_events))
             ) %>% 
      group_by(season, game_id, game_period) %>%
      arrange(event_index) %>% 
      mutate(seconds_since_last =  game_seconds - lag(game_seconds),
             event_type_last =     lag(event_type),
             event_team_last =     lag(event_team),
             event_strength_last = lag(game_strength_state), 
             coords_x_last =       lag(coords_x),
             coords_y_last =       lag(coords_y),
             shift_ID =            as.numeric(shift_ID)
             ) %>% 
      group_by(season, game_id, pen_index) %>% 
      ungroup() %>% 
      arrange(season, game_id, event_index) %>% 
      filter(event_type %in% st.fenwick_events, 
             (event_team == away_team & game_strength_state %in% c("Ev5", "Ev4", "Ev3") & (home_skaters == 6 | away_skaters == 6)) | 
               (event_team == home_team & game_strength_state %in% c("5vE", "4vE", "3vE") & (home_skaters == 6 | away_skaters == 6)), 
             !is.na(coords_x_last), 
             !is.na(coords_y_last)
             ) %>% 
      mutate(same_team_last =     1 * (event_team == event_team_last),
             is_home =            1 * (event_team == home_team),
             score_state =        ifelse(is_home == 1, home_score - away_score, away_score - home_score), 
             event_detail =       ifelse(is.na(event_detail), "Wrist", event_detail), 
             distance_from_last = sqrt((coords_x - coords_x_last)^2 + (coords_y - coords_y_last)^2), 
             
             # Flurry indicator
             flurry = 1 * (shift_ID == lag(shift_ID, default = 0) & event_team == lag(event_team)),
             # Probably a better way to do this...
             flurry_1 = ifelse(flurry == 1 & lag(flurry) ==   1,  2, flurry), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 2,  3, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 3,  4, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 4,  5, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 5,  6, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 6,  7, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 7,  8, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 8,  9, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 9,  10, flurry_1), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 10, 11, flurry_1), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 11, 12, flurry_1), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 12, 13, flurry_1), 
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 13, 14, flurry_1),
             flurry_1 = ifelse(flurry == 1 & lag(flurry_1) == 14, 15, flurry_1)
             ) %>%
      select(-c(flurry)) %>% 
      rename(shot_distance = event_distance, 
             shot_angle = event_angle,
             flurry = flurry_1
             ) %>% 
      select(game_id, event_index, season, game_date, game_period, game_seconds, 
             game_strength_state, game_score_state, score_state, is_home, 
             event_player_1, home_goalie, away_goalie, 
             home_score, away_score, home_team, away_team, home_skaters, away_skaters, 
             event_description, event_team, event_type, event_detail, 
             coords_x, coords_y, pbp_distance, shot_distance, shot_angle,
             event_team_last, same_team_last, event_strength_last, event_type_last, 
             seconds_since_last, distance_from_last, coords_x_last, coords_y_last,
             shift_ID, shift_length, flurry
             ) %>% 
      filter(!is.na(shot_distance), 
             !is.na(shot_angle)
             ) %>% 
      data.frame()
    
    }
  
  }
fun.model_prep <- function(data, prep_type) { 
  
  # Prep model data frames (EV or UE)
  
  if (prep_type == "EV") { 
    
    # Create EV dummy variables, returns a matrix.
    model_prep <- data %>% 
      mutate(is_goal = 1 * (event_type == "GOAL"), 
             
             state_5v5 = 1 * (game_strength_state == "5v5"), 
             state_4v4 = 1 * (game_strength_state == "4v4"), 
             state_3v3 = 1 * (game_strength_state == "3v3"), 
             
             score_down_4 = 1 * (score_state <= -4), 
             score_down_3 = 1 * (score_state == -3), 
             score_down_2 = 1 * (score_state == -2), 
             score_down_1 = 1 * (score_state == -1), 
             score_even   = 1 * (score_state ==  0), 
             score_up_1   = 1 * (score_state ==  1), 
             score_up_2   = 1 * (score_state ==  2), 
             score_up_3   = 1 * (score_state ==  3), 
             score_up_4   = 1 * (score_state >=  4), 
             
             wrist_shot =     1 * (event_detail == "Wrist"), 
             deflected_shot = 1 * (event_detail == "Deflected"), 
             tip_shot =       1 * (event_detail == "Tip-In"), 
             slap_shot =      1 * (event_detail == "Slap"), 
             backhand_shot =  1 * (event_detail == "Backhand"), 
             snap_shot =      1 * (event_detail == "Snap"), 
             wrap_shot =      1 * (event_detail == "Wrap-around"), 
             
             prior_shot_same =  1 * (event_type_last == "SHOT" & same_team_last == 1), 
             prior_miss_same =  1 * (event_type_last == "MISS" & same_team_last == 1), 
             prior_block_same = 1 * (event_type_last == "BLOCK" & same_team_last == 1), 
             prior_shot_opp =   1 * (event_type_last == "SHOT" & same_team_last == 0), 
             prior_miss_opp =   1 * (event_type_last == "MISS" & same_team_last == 0), 
             prior_block_opp =  1 * (event_type_last == "BLOCK" & same_team_last == 0), 
             
             prior_give_opp =  1 * (event_type_last == "GIVE" & same_team_last == 0), 
             prior_give_same = 1 * (event_type_last == "GIVE" & same_team_last == 1),
             prior_take_opp =  1 * (event_type_last == "TAKE" & same_team_last == 0), 
             prior_take_same = 1 * (event_type_last == "TAKE" & same_team_last == 1), 
             prior_hit_opp =   1 * (event_type_last == "HIT" & same_team_last == 0), 
             prior_hit_same =  1 * (event_type_last == "HIT" & same_team_last == 1), 
             prior_face =      1 * (event_type_last == "FAC")
             ) %>% 
      select(is_goal,
             shot_distance, shot_angle, is_home, 
             state_5v5:state_3v3, 
             score_down_4:score_up_4, 
             game_seconds, game_period, coords_x, coords_y, coords_x_last, coords_y_last, 
             wrist_shot:wrap_shot, distance_from_last, seconds_since_last, 
             prior_shot_same:prior_face) %>% 
      data.matrix()
    
    }
  else if (prep_type == "UE") { 
    
    # Create UE dummy variables, returns a matrix.
    model_prep <- data %>% 
      mutate(is_goal = 1 * (event_type == "GOAL"), 
             
             state_5v4 = 1 * ((true_strength_state == "5v4" & event_team == home_team) | (true_strength_state == "4v5" & event_team == away_team)), 
             state_5v3 = 1 * ((true_strength_state == "5v3" & event_team == home_team) | (true_strength_state == "3v5" & event_team == away_team)), 
             state_4v3 = 1 * ((true_strength_state == "4v3" & event_team == home_team) | (true_strength_state == "3v4" & event_team == away_team)), 
             state_6v5 = 1 * ((true_strength_state == "6v5" & event_team == home_team) | (true_strength_state == "5v6" & event_team == away_team)), 
             state_6v4 = 1 * ((true_strength_state == "6v4" & event_team == home_team) | (true_strength_state == "4v6" & event_team == away_team)), 
             
             score_down_4 = 1 * (score_state <= -4), 
             score_down_3 = 1 * (score_state == -3), 
             score_down_2 = 1 * (score_state == -2), 
             score_down_1 = 1 * (score_state == -1), 
             score_even   = 1 * (score_state ==  0), 
             score_up_1   = 1 * (score_state ==  1), 
             score_up_2   = 1 * (score_state ==  2), 
             score_up_3   = 1 * (score_state ==  3), 
             score_up_4   = 1 * (score_state >=  4), 
             
             wrist_shot =     1 * (event_detail == "Wrist"), 
             deflected_shot = 1 * (event_detail == "Deflected"), 
             tip_shot =       1 * (event_detail == "Tip-In"), 
             slap_shot =      1 * (event_detail == "Slap"), 
             backhand_shot =  1 * (event_detail == "Backhand"), 
             snap_shot =      1 * (event_detail == "Snap"), 
             wrap_shot =      1 * (event_detail == "Wrap-around"), 
             
             prior_shot_same =  1 * (event_type_last == "SHOT" & same_team_last == 1), 
             prior_miss_same =  1 * (event_type_last == "MISS" & same_team_last == 1), 
             prior_block_same = 1 * (event_type_last == "BLOCK" & same_team_last == 1), 
             prior_shot_opp =   1 * (event_type_last == "SHOT" & same_team_last == 0), 
             prior_miss_opp =   1 * (event_type_last == "MISS" & same_team_last == 0), 
             prior_block_opp =  1 * (event_type_last == "BLOCK" & same_team_last == 0), 
             prior_give_same =  1 * (event_type_last == "GIVE" & same_team_last == 1),
             prior_take_same =  1 * (event_type_last == "TAKE" & same_team_last == 1), 
             prior_hit_same =   1 * (event_type_last == "HIT" & same_team_last == 1), 
             prior_give_opp =   1 * (event_type_last == "GIVE" & same_team_last == 0), 
             prior_take_opp =   1 * (event_type_last == "TAKE" & same_team_last == 0), 
             prior_hit_opp =    1 * (event_type_last == "HIT" & same_team_last == 0), 
             prior_face =       1 * (event_type_last == "FAC")
             ) %>% 
      select(is_goal, 
             shot_distance, shot_angle, is_home, 
             state_5v4:state_6v4, 
             score_down_4:score_up_4, 
             game_seconds, game_period, coords_x, coords_y, coords_x_last, coords_y_last, 
             wrist_shot:wrap_shot, distance_from_last, seconds_since_last, prior_event_EV, 
             pen_seconds_since, prior_shot_same:prior_face
             ) %>% 
      data.matrix()
    
    }
  else if (prep_type == "SH") { 
    
    # Create SH dummy variables, returns a matrix.
    model_prep_SH <- data %>% 
      mutate(is_goal = 1 * (event_type == "GOAL"), 
             
             state_4v5 = 1 * ((game_strength_state == "5v4" & event_team == away_team) | (game_strength_state == "4v5" & event_team == home_team)), 
             state_3v5 = 1 * ((game_strength_state == "5v3" & event_team == away_team) | (game_strength_state == "3v5" & event_team == home_team)), 
             state_3v4 = 1 * ((game_strength_state == "4v3" & event_team == away_team) | (game_strength_state == "3v4" & event_team == home_team)), 
             
             score_down_4 = 1 * (score_state <= -4), 
             score_down_3 = 1 * (score_state == -3), 
             score_down_2 = 1 * (score_state == -2), 
             score_down_1 = 1 * (score_state == -1), 
             score_even   = 1 * (score_state ==  0), 
             score_up_1   = 1 * (score_state ==  1), 
             score_up_2   = 1 * (score_state ==  2), 
             score_up_3   = 1 * (score_state ==  3), 
             score_up_4   = 1 * (score_state >=  4), 
             
             wrist_shot =     1 * (event_detail == "Wrist"), 
             deflected_shot = 1 * (event_detail == "Deflected"), 
             tip_shot =       1 * (event_detail == "Tip-In"), 
             slap_shot =      1 * (event_detail == "Slap"), 
             backhand_shot =  1 * (event_detail == "Backhand"), 
             snap_shot =      1 * (event_detail == "Snap"), 
             wrap_shot =      1 * (event_detail == "Wrap-around"), 
             
             prior_shot_same =  1 * (event_type_last == "SHOT" & same_team_last == 1), 
             prior_miss_same =  1 * (event_type_last == "MISS" & same_team_last == 1), 
             prior_block_same = 1 * (event_type_last == "BLOCK" & same_team_last == 1), 
             prior_shot_opp =   1 * (event_type_last == "SHOT" & same_team_last == 0), 
             prior_miss_opp =   1 * (event_type_last == "MISS" & same_team_last == 0), 
             prior_block_opp =  1 * (event_type_last == "BLOCK" & same_team_last == 0), 
             prior_give_same =  1 * (event_type_last == "GIVE" & same_team_last == 1),
             prior_take_same =  1 * (event_type_last == "TAKE" & same_team_last == 1), 
             prior_hit_same =   1 * (event_type_last == "HIT" & same_team_last == 1), 
             prior_give_opp =   1 * (event_type_last == "GIVE" & same_team_last == 0), 
             prior_take_opp =   1 * (event_type_last == "TAKE" & same_team_last == 0), 
             prior_hit_opp =    1 * (event_type_last == "HIT" & same_team_last == 0), 
             prior_face =       1 * (event_type_last == "FAC")
             ) %>% 
      select(is_goal, 
             shot_distance, shot_angle, is_home, 
             state_4v5:state_3v4, 
             score_down_4:score_up_4, 
             game_seconds, game_period, coords_x, coords_y, coords_x_last, coords_y_last, 
             wrist_shot:wrap_shot, distance_from_last, seconds_since_last, prior_event_EV, 
             pen_seconds_since, prior_shot_same:prior_face) %>% 
      data.matrix()
    
    }
  else if (prep_type == "EN") { 
    
    # Create EN dummy variables, returns a matrix.
    model_prep_EN <- data %>% 
      mutate(is_goal = 1 * (event_type == "GOAL"), 
             
             state_Ev5 = 1 * ((game_strength_state == "Ev5" & event_team == away_team) | (game_strength_state == "5vE" & event_team == home_team)), 
             state_Ev4 = 1 * ((game_strength_state == "Ev4" & event_team == away_team) | (game_strength_state == "4vE" & event_team == home_team)), 
             state_Ev3 = 1 * ((game_strength_state == "Ev3" & event_team == away_team) | (game_strength_state == "3vE" & event_team == home_team)), 
             
             score_down_4 = 1 * (score_state <= -4), 
             score_down_3 = 1 * (score_state == -3), 
             score_down_2 = 1 * (score_state == -2), 
             score_down_1 = 1 * (score_state == -1), 
             score_even   = 1 * (score_state ==  0), 
             score_up_1   = 1 * (score_state ==  1), 
             score_up_2   = 1 * (score_state ==  2), 
             score_up_3   = 1 * (score_state ==  3), 
             score_up_4   = 1 * (score_state >=  4), 
             
             wrist_shot =     1 * (event_detail == "Wrist"), 
             deflected_shot = 1 * (event_detail == "Deflected"), 
             tip_shot =       1 * (event_detail == "Tip-In"), 
             slap_shot =      1 * (event_detail == "Slap"), 
             backhand_shot =  1 * (event_detail == "Backhand"), 
             snap_shot =      1 * (event_detail == "Snap"), 
             wrap_shot =      1 * (event_detail == "Wrap-around"), 
             
             prior_shot_same =  1 * (event_type_last == "SHOT" & same_team_last == 1), 
             prior_miss_same =  1 * (event_type_last == "MISS" & same_team_last == 1), 
             prior_block_same = 1 * (event_type_last == "BLOCK" & same_team_last == 1), 
             prior_shot_opp =   1 * (event_type_last == "SHOT" & same_team_last == 0), 
             prior_miss_opp =   1 * (event_type_last == "MISS" & same_team_last == 0), 
             prior_block_opp =  1 * (event_type_last == "BLOCK" & same_team_last == 0), 
             prior_give_same =  1 * (event_type_last == "GIVE" & same_team_last == 1),
             prior_take_same =  1 * (event_type_last == "TAKE" & same_team_last == 1), 
             prior_hit_same =   1 * (event_type_last == "HIT" & same_team_last == 1), 
             prior_give_opp =   1 * (event_type_last == "GIVE" & same_team_last == 0), 
             prior_take_opp =   1 * (event_type_last == "TAKE" & same_team_last == 0), 
             prior_hit_opp =    1 * (event_type_last == "HIT" & same_team_last == 0), 
             prior_face =       1 * (event_type_last == "FAC")
             ) %>% 
      select(is_goal, 
             shot_distance, shot_angle, is_home, 
             state_Ev5:state_Ev3, 
             score_down_4:score_up_4, 
             game_seconds, game_period, coords_x, coords_y, coords_x_last, coords_y_last, 
             wrist_shot:wrap_shot, distance_from_last, seconds_since_last, 
             prior_shot_same:prior_face) %>% 
      data.matrix()
    
    }
  
  }

# Full run
fun.pbp_full_add <- function(data, model_EV, model_UE, model_SH, model_EN) { 
  
  ### Function should be run using unprocessed scraped pbp data. Returns a list of 5 data frames: 
  ## ~$pbp_full -    full pbp data with extras and xG probabilities for both EV and UE situations
  ## ~$pbp_prep_EV - EV prep data frame for xG model evaluation
  ## ~$pbp_prep_UE - UE prep data frame for xG model evaluation
  ## ~$pbp_prep_SH - SH prep data frame for xG model evaluation
  ## ~$pbp_prep_EN - EN prep data frame for xG model evaluation
  
  # Initial prep of scraped pbp data.
  pbp_part <- fun.pbp_expand(data)
  pbp_part <- fun.pbp_index(pbp_part)
  
  
  # Convert for use with XGBoost and predict goal probability
  
  ## ----------------- ##
  ##   Even-Strength   ##
  ## ----------------- ##
  
  print("predict_EV", quote = F)
  pbp_prep_EV <- fun.pbp_prep(pbp_part, "EV")
  model_prep_EV <- fun.model_prep(pbp_prep_EV, "EV")
  model_prep_EV <- Matrix(model_prep_EV, sparse = TRUE)
  
  pred_matrix_EV <- model_prep_EV[, 2:ncol(model_prep_EV)]
  xgb_matrix_EV <- xgb.DMatrix(data = pred_matrix_EV)
  
  pbp_prep_EV$pred_XGB_7 <- predict(object = model_EV, xgb_matrix_EV)
  pred_goal_EV <- select(pbp_prep_EV, game_id, event_index, pred_XGB_7)
  
  
  ## ------------------- ##
  ##   Uneven-Strength   ##
  ## ------------------- ##
  
  print("predict_UE", quote = F)
  pbp_prep_UE <- fun.pbp_prep(pbp_part, "UE")
  model_prep_UE <- fun.model_prep(pbp_prep_UE, "UE")
  model_prep_UE <- Matrix(model_prep_UE, sparse = TRUE)
  
  pred_matrix_UE <- model_prep_UE[, 2:ncol(model_prep_UE)]
  
  # Deal with 1 row predict issue
  if (nrow(model_prep_UE) == 1) { 
    xgb_matrix_UE <- xgb.DMatrix(data = as.matrix(data.frame(as.list(pred_matrix_UE))))
    
    } else {
    xgb_matrix_UE <- xgb.DMatrix(data = pred_matrix_UE)
    
    }
  
  # Check to see there is data to predict
  if (nrow(pbp_prep_UE) > 0) { 
    pbp_prep_UE$pred_XGB_7 <- predict(object = model_UE, xgb_matrix_UE)
    pred_goal_UE <- select(pbp_prep_UE, game_id, event_index, pred_XGB_7)
    
    } else { 
    pred_goal_UE <- NULL
    
    }
  
  ## --------------- ##
  ##   Shorthanded   ##
  ## --------------- ##
  
  print("predict_SH", quote = F)
  pbp_prep_SH <- fun.pbp_prep(pbp_part, "SH")
  model_prep_SH <- fun.model_prep(pbp_prep_SH, "SH")
  model_prep_SH <- Matrix(model_prep_SH, sparse = TRUE)
  
  pred_matrix_SH <- model_prep_SH[, 2:ncol(model_prep_SH)]
  
  # Deal with 1 row predict issue
  if (nrow(model_prep_SH) == 1) { 
    xgb_matrix_SH <- xgb.DMatrix(data = as.matrix(data.frame(as.list(pred_matrix_SH))))
    
    } else {
    xgb_matrix_SH <- xgb.DMatrix(data = pred_matrix_SH)
    
    }
  
  # Check to see there is data to predict
  if (nrow(pbp_prep_SH) > 0) { 
    pbp_prep_SH$pred_XGB_7 <- predict(object = model_SH, xgb_matrix_SH)
    pred_goal_SH <- select(pbp_prep_SH, game_id, event_index, pred_XGB_7)
    
    } else { 
    pred_goal_SH <- NULL
    
    }
  
  
  ## ------------- ##
  ##   Empty Net   ##
  ## ------------- ##
  
  print("predict_EN", quote = F)
  pbp_prep_EN <- fun.pbp_prep(pbp_part, "EN")
  model_prep_EN <- fun.model_prep(pbp_prep_EN, "EN")
  model_prep_EN <- Matrix(model_prep_EN, sparse = TRUE)
  
  pred_matrix_EN <- model_prep_EN[, 2:ncol(model_prep_EN)]
  
  # Deal with 1 row predict issue
  if (nrow(model_prep_EN) == 1) { 
    xgb_matrix_EN <- xgb.DMatrix(data = as.matrix(data.frame(as.list(pred_matrix_EN))))  ## Deal with 1 row predict issue
    
    } else {
    xgb_matrix_EN <- xgb.DMatrix(data = pred_matrix_EN)
    
    }
  
  # Check to see there is data to predict
  if (nrow(pbp_prep_EN) > 0) { 
    pbp_prep_EN$pred_XGB_7 <- predict(object = model_EN, xgb_matrix_EN)
    pred_goal_EN <- select(pbp_prep_EN, game_id, event_index, pred_XGB_7)
    
    } else { 
    pred_goal_EN <- NULL
    
    }
  
  
  ## ------------------ ##
  ##   Join / Combine   ##
  ## ------------------ ##
  
  print("final_join", quote = F)
  
  hold <- rbind(pred_goal_EV, 
                pred_goal_UE, 
                pred_goal_SH, 
                pred_goal_EN)
  
  pbp_return <- pbp_part %>% 
    left_join(., hold, by = c("game_id", "event_index")) 
  
  pbp_return$shift_ID <-  as.character(pbp_return$shift_ID)
  pbp_prep_EV$shift_ID <- as.character(pbp_prep_EV$shift_ID)
  pbp_prep_UE$shift_ID <- as.character(pbp_prep_UE$shift_ID)
  pbp_prep_SH$shift_ID <- as.character(pbp_prep_SH$shift_ID)
  pbp_prep_EN$shift_ID <- as.character(pbp_prep_EN$shift_ID)
  
  list_data <- list(pbp_full = pbp_return, 
                    prep_EV =  pbp_prep_EV, 
                    prep_UE =  pbp_prep_UE, 
                    prep_SH =  pbp_prep_SH, 
                    prep_EN =  pbp_prep_EN)
  
  }


##################


## --------------------- ##
##   Schedule Function   ##
## --------------------- ##

###########################

fun.schedule <- function(start, end) { 
  
  # Create Team IDs
  fun.Team_IDs <- function() { 
    
    IDs <- data.frame(matrix(nrow = 33, ncol = 2))
    IDs$X1 <- c("N.J", "NYI", "NYR", "PHI", "PIT", "BOS", "BUF", "MTL", "OTT", "TOR", 
                "1000", "CAR", "FLA", "T.B", "WSH", "CHI", "DET", "NSH", "STL", "CGY", 
                "COL", "EDM", "VAN", "ANA", "DAL", "L.A", "1000", "S.J", "CBJ", "MIN", 
                "WPG", "ARI", "VGK")
    
    IDs$X2 <- seq(1:33)
    IDs$X2 <- ifelse(IDs$X2 == 31, 52, IDs$X2)
    IDs$X2 <- ifelse(IDs$X2 == 32, 53, IDs$X2)
    IDs$X2 <- ifelse(IDs$X2 == 33, 54, IDs$X2)
    names(IDs) <- c("Team", "ID")
    
    return(IDs)
    
    }
  Team_ID <- fun.Team_IDs()
  
  # Scrape Schedule
  sched <- ds.scrape_schedule(start,
                              end, 
                              try_tolerance = 5, 
                              agents = ds.user_agents)
  
  sched <- filter(sched, session != "PR")
  
  sched$home_team_id <- Team_ID$Team[match(sched$home_team_id, Team_ID$ID)]
  sched$away_team_id <- Team_ID$Team[match(sched$away_team_id, Team_ID$ID)]
  
  sched$test <- format(as.POSIXct(sched$game_datetime, 
                                  tz = "UTC", 
                                  format = "%Y-%m-%d %H:%M:%S"), 
                       tz = "Canada/Eastern")
  
  sched$test <- as.Date(sched$test)
  sched$test <- as.Date(ifelse(is.na(sched$test), 
                               as.Date(sched$game_datetime) - 1,
                               sched$test), 
                        origin = "1970-01-01")
  
  sched$game_date <- sched$test
  
  sched <- sched %>% 
    arrange(game_id) %>% 
    rename(home_team = home_team_id, 
           away_team = away_team_id)
  
  }


###########################


## ------------------- ##
##   NHL Player JSON   ##
## ------------------- ##

#########################

# Scrape NHL skater JSON information
fun.NHL_info_scrape <- function(season_) { 
  
  ## --------------- ##
  ##   Skater Data   ##
  ## --------------- ##
  
  # Scrape NHL JSON data
  NHL_data <- jsonlite::fromJSON(
    paste0(
      "http://www.nhl.com/stats/rest/skaters?isAggregate=false&reportType=basic&isGame=false&reportName=bios&cayenneExp=gameTypeId=2%20and%20seasonId%3E=", 
      season_, 
      "%20and%20seasonId%3C=", 
      season_
      )
    )
  
  # Skater data per season
  NHL_skater_data <- NHL_data$data %>% 
    mutate(player = toupper(playerName), 
           player = gsub(" ", ".", player), 
           birthday = as.Date(playerBirthDate), 
           season_age = as.numeric(floor((as.Date(paste0(substr(seasonId, 5, 8), "-2-15"), "%Y-%m-%d") - birthday) / 365.25)), # updated to reflect 365.25
           seasonId = as.character(seasonId), 
           playerTeamsPlayedFor = ifelse(grepl(", ", playerTeamsPlayedFor), gsub(", ", "/", playerTeamsPlayedFor), playerTeamsPlayedFor), 
           
           # Name Corrections
           player = ifelse(grepl("ALEXANDER", player), gsub("ALEXANDER", "ALEX", player), player), 
           player = ifelse(grepl("ALEXANDRE", player), gsub("ALEXANDRE", "ALEX", player), player), 
           player = ifelse(player == "BEN.ONDRUS", "BENJAMIN.ONDRUS", player),
           player = ifelse(player == "BRYCE.VAN.BRABANT", "BRYCE.VAN BRABANT", player),
           player = ifelse(player == "CALVIN.DE.HAAN", "CALVIN.DE HAAN", player), 
           player = ifelse(player == "CHASE.DE.LEO", "CHASE.DE LEO", player),
           player = ifelse(player == "CHRISTOPHER.DIDOMENICO", "CHRIS.DIDOMENICO", player), 
           player = ifelse(player == "CHRISTOPHER.TANEV", "CHRIS.TANEV", player), 
           player = ifelse(player == "DANIEL.CARCILLO", "DAN.CARCILLO", player),
           player = ifelse(player == "DANNY.O'REGAN", "DANIEL.O'REGAN", player), 
           player = ifelse(player == "DAVID.VAN.DER.GULIK", "DAVID.VAN DER GULIK", player),
           player = ifelse(player == "EVGENII.DADONOV", "EVGENY.DADONOV", player), 
           player = ifelse(player == "FREDDY.MODIN", "FREDRIK.MODIN", player),
           player = ifelse(player == "GREG.DE.VRIES", "GREG.DE VRIES", player),
           player = ifelse(player == "ILYA.ZUBOV", "ILJA.ZUBOV", player),
           player = ifelse(player == "JACOB.DE.LA.ROSE", "JACOB.DE LA ROSE", player), 
           player = ifelse(player == "JAMES.VAN.RIEMSDYK", "JAMES.VAN RIEMSDYK", player), 
           player = ifelse(player == "JEAN-FRANCOIS.JACQUES", "J-F.JACQUES", player),
           player = ifelse(player == "JAKOB.FORSBACKA.KARLSSON", "JAKOB.FORSBACKA KARLSSON", player),
           player = ifelse(player == "JIM.DOWD", "JAMES.DOWD", player),
           player = ifelse(player == "JEFF.HAMILTON", "JEFFREY.HAMILTON", player),
           player = ifelse(player == "JEFF.PENNER", "JEFFREY.PENNER", player),
           player = ifelse(player == "JOEL.ERIKSSON.EK", "JOEL.ERIKSSON EK", player), 
           player = ifelse(player == "MARK.VAN.GUILDER", "MARK.VAN GUILDER", player),
           player = ifelse(player == "MARTIN.ST..LOUIS", "MARTIN.ST. LOUIS", player),
           player = ifelse(player == "MARTIN.ST.PIERRE", "MARTIN.ST PIERRE", player),
           player = ifelse(player == "MICHAEL.CAMMALLERI", "MIKE.CAMMALLERI", player), 
           player = ifelse(player == "MICHAEL.DAL.COLLE", "MICHAEL.DAL COLLE", player), 
           player = ifelse(player == "MICHAEL.DEL.ZOTTO", "MICHAEL.DEL ZOTTO", player), 
           player = ifelse(player == "MIKE.VERNACE", "MICHAEL.VERNACE", player),
           player = ifelse(player == "MIKE.YORK", "MICHAEL.YORK", player),
           player = ifelse(player == "MIKE.VAN.RYN", "MIKE.VAN RYN", player),
           player = ifelse(player == "MITCHELL.MARNER", "MITCH.MARNER", player),
           player = ifelse(player == "PAT.MAROON", "PATRICK.MAROON", player),
           player = ifelse(player == "PA.PARENTEAU", "P.A..PARENTEAU", player),
           player = ifelse(player == "PHILLIP.DI.GIUSEPPE", "PHILLIP.DI GIUSEPPE", player), 
           player = ifelse(player == "STEFAN.DELLA.ROVERE", "STEFAN.DELLA ROVERE", player),
           player = ifelse(player == "STEPHANE.DA.COSTA", "STEPHANE.DA COSTA", player),
           player = ifelse(player == "TJ.GALIARDI", "T.J..GALIARDI", player),
           player = ifelse(player == "TOBY.ENSTROM", "TOBIAS.ENSTROM", player), 
           player = ifelse(player == "TREVOR.VAN.RIEMSDYK", "TREVOR.VAN RIEMSDYK", player), 
           player = ifelse(player == "ZACK.FITZGERALD", "ZACH.FITZGERALD", player),
           
           # Updates
           player = ifelse(player == "SEBASTIAN.AHO" & birthday == "1996-02-17", "5EBASTIAN.AHO", player), 
           player = ifelse(player == "ALEXANDRE.FORTIN", "ALEX.FORTIN", player)
           ) %>% 
    rename(season = seasonId) %>% 
    ungroup() %>% 
    rename_at(vars(playerPositionCode, playerShootsCatches, playerBirthCity, playerBirthCountry, playerTeamsPlayedFor, 
                   playerDraftOverallPickNo, playerDraftRoundNo, playerDraftYear, playerHeight, playerWeight), 
              funs(gsub("player", "", .))
              ) %>% 
    mutate(position = ifelse(PositionCode == "D", 2, 1),
           current_age = floor((Sys.Date() - birthday) / 365.25) # updated to reflect 365.25
           ) %>% 
    select(player, PositionCode, ShootsCatches, birthday, TeamsPlayedFor, 
           BirthCity, BirthCountry, DraftOverallPickNo, DraftRoundNo, 
           DraftYear, Height, Weight, position, current_age
           ) %>% 
    data.frame()
  
  
  ## --------------- ##
  ##   Goalie Data   ##
  ## --------------- ##
  
  # Scrape NHL JSON data
  NHL_goalie_data <- jsonlite::fromJSON(
    paste0(
      "http://www.nhl.com/stats/rest/goalies?isAggregate=false&reportType=basic&isGame=false&reportName=bios&cayenneExp=gameTypeId=2%20and%20seasonId%3E=", 
      season_, 
      "%20and%20seasonId%3C=", 
      season_
      )
    )
  
  # Goalie data per season
  NHL_goalie_data <- NHL_goalie_data$data %>% 
    mutate(player = toupper(playerName), 
           player = gsub(" ", ".", player), 
           birthday = as.Date(playerBirthDate), 
           season_age = as.numeric(floor((as.Date(paste0(substr(seasonId, 5, 8), "-2-15"), "%Y-%m-%d") - birthday) / 365.25)), # updated to reflect 365.25
           seasonId = as.character(seasonId), 
           playerTeamsPlayedFor = ifelse(grepl(", ", playerTeamsPlayedFor), gsub(", ", "/", playerTeamsPlayedFor), playerTeamsPlayedFor), 
           
           # Name Corrections
           player = ifelse(player == "CHRISTOPHER.GIBSON", "CHRIS.GIBSON", player), 
           player = ifelse(player == "CAL.PETERSEN", "CALVIN.PETERSEN", player)
           ) %>% 
    rename(season = seasonId) %>% 
    ungroup() %>% 
    rename_at(vars(playerPositionCode, playerShootsCatches, playerBirthCity, playerBirthCountry, playerTeamsPlayedFor, 
                   playerDraftOverallPickNo, playerDraftRoundNo, playerDraftYear, playerHeight, playerWeight), 
              funs(gsub("player", "", .))) %>% 
    mutate(position = 3,
           current_age = floor((Sys.Date() - birthday) / 365.25) # updated to reflect 365.25
           ) %>% 
    select(player, PositionCode, ShootsCatches, birthday, TeamsPlayedFor, 
           BirthCity, BirthCountry, DraftOverallPickNo, DraftRoundNo, 
           DraftYear, Height, Weight, position, current_age
           ) %>% 
    data.frame()
  
  
  # Join Data
  return_joined <- bind_rows(
      NHL_skater_data, 
      NHL_goalie_data
      ) %>% 
    arrange(player)
  
  }


#########################



## ------------------------------------------------------ ##



## ----------------- ##
##   All Sit Games   ##
## ----------------- ##

#######################

# Calulate Game-By_Game Stats - All Situations
fun.all_sit_standard <- function(data_) { 
  
  # Modify pbp data
  pbp_data <- data_ %>% 
    filter(game_period < 5, 
           event_length < 900
           ) %>% 
    select(-c(face_index:shift_length)) %>% 
    mutate(event_length = ifelse(is.na(event_length), 0, event_length)) %>% 
    rename(pred_goal = pred_XGB_7)
  
  # Time On Ice
  fun.onice_H_all <- function(data, venue) {
    
    on_ice <- data %>% 
      summarise(Team = first(home_team), 
                Opponent = first(away_team), 
                is_home = 1, 
                TOI = sum(event_length) / 60
                )
    
    return(on_ice)
    
    }
  fun.onice_A_all <- function(data, venue) {
    
    on_ice <- data %>% 
      summarise(Team = first(away_team), 
                Opponent = first(home_team), 
                is_home = 0, 
                TOI = sum(event_length) / 60
                )
    
    return(on_ice)
    
    }
  fun.onice_combine_all <- function(data) {
    
    # prepare data
    function_data <- data %>% 
      filter(game_strength_state %in% st.strength_states) %>% 
      mutate(event_length = ifelse(is.na(event_length), 0, event_length))
    
    # Run Functions
    h1 <- function_data %>% group_by(game_id, game_date, season, home_on_1, home_team) %>% fun.onice_H_all(., "home_on_1") %>% rename(player = home_on_1) %>% data.frame()
    h2 <- function_data %>% group_by(game_id, game_date, season, home_on_2, home_team) %>% fun.onice_H_all(., "home_on_2") %>% rename(player = home_on_2) %>% data.frame()
    h3 <- function_data %>% group_by(game_id, game_date, season, home_on_3, home_team) %>% fun.onice_H_all(., "home_on_3") %>% rename(player = home_on_3) %>% data.frame()
    h4 <- function_data %>% group_by(game_id, game_date, season, home_on_4, home_team) %>% fun.onice_H_all(., "home_on_4") %>% rename(player = home_on_4) %>% data.frame()
    h5 <- function_data %>% group_by(game_id, game_date, season, home_on_5, home_team) %>% fun.onice_H_all(., "home_on_5") %>% rename(player = home_on_5) %>% data.frame()
    h6 <- function_data %>% group_by(game_id, game_date, season, home_on_6, home_team) %>% fun.onice_H_all(., "home_on_6") %>% rename(player = home_on_6) %>% data.frame()
    
    a1 <- function_data %>% group_by(game_id, game_date, season, away_on_1, away_team) %>% fun.onice_A_all(., "away_on_1") %>% rename(player = away_on_1) %>% data.frame()
    a2 <- function_data %>% group_by(game_id, game_date, season, away_on_2, away_team) %>% fun.onice_A_all(., "away_on_2") %>% rename(player = away_on_2) %>% data.frame()
    a3 <- function_data %>% group_by(game_id, game_date, season, away_on_3, away_team) %>% fun.onice_A_all(., "away_on_3") %>% rename(player = away_on_3) %>% data.frame()
    a4 <- function_data %>% group_by(game_id, game_date, season, away_on_4, away_team) %>% fun.onice_A_all(., "away_on_4") %>% rename(player = away_on_4) %>% data.frame()
    a5 <- function_data %>% group_by(game_id, game_date, season, away_on_5, away_team) %>% fun.onice_A_all(., "away_on_5") %>% rename(player = away_on_5) %>% data.frame()
    a6 <- function_data %>% group_by(game_id, game_date, season, away_on_6, away_team) %>% fun.onice_A_all(., "away_on_6") %>% rename(player = away_on_6) %>% data.frame()
    
    # Combine
    join_df <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
    
    
    # Remove Goalies
    fun.goalie_find <- function(data_) {
      
      # Identifies goalies within a given pbp data.frame & returns a data.frame to join for removal
      
      goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data_$home_goalie, data_$away_goalie))))), 
                                  is_goalie = 1)
      
      goalie_return$player <- as.character(goalie_return$player)
      
      return(goalie_return)
      
      }
    goalie_remove <- fun.goalie_find(data_ = function_data)
    
    join_remove <- join_df[!(join_df$player %in% goalie_remove$player), ]
    
    # Format
    join_return <- join_remove %>% 
      group_by(player, game_id, game_date, season) %>% 
      summarise(Team = first(Team), 
                Opponent = first(Opponent), 
                is_home = first(is_home), 
                TOI  = sum(TOI)
                ) %>% 
      filter(!is.na(player), 
             !is.na(game_date) # filtering to catch pbp data issues for all situations
             ) %>% 
      select(player, game_id, game_date, season, Team, Opponent, is_home, TOI) %>% 
      data.frame()
    
    return(join_return)
    
    }
  
  # Zone Starts
  fun.ZS_H_all <- function(data, venue) {
    
    hold <- data %>% 
      filter(event_type == "FAC") %>%  
      summarise(Team = first(home_team),
                OZS = sum(event_type == "FAC" & home_zone == "Off"), 
                NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
                DZS = sum(event_type == "FAC" & home_zone == "Def")
                )
    
    return(hold)
    
    }
  fun.ZS_A_all <- function(data, venue) {
    
    hold <- data %>% 
      filter(event_type == "FAC") %>%  
      summarise(Team = first(away_team),
                OZS = sum(event_type == "FAC" & home_zone == "Def"), 
                NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
                DZS = sum(event_type == "FAC" & home_zone == "Off")
                )
    
    return(hold)
    
    }
  fun.ZS_compute_all <- function(data) {
    
    h1 <- data %>% group_by(game_id, season, home_on_1) %>% fun.ZS_H_all(., "home_on_1") %>% rename(player = home_on_1) %>% data.frame()
    h2 <- data %>% group_by(game_id, season, home_on_2) %>% fun.ZS_H_all(., "home_on_2") %>% rename(player = home_on_2) %>% data.frame()
    h3 <- data %>% group_by(game_id, season, home_on_3) %>% fun.ZS_H_all(., "home_on_3") %>% rename(player = home_on_3) %>% data.frame()
    h4 <- data %>% group_by(game_id, season, home_on_4) %>% fun.ZS_H_all(., "home_on_4") %>% rename(player = home_on_4) %>% data.frame()
    h5 <- data %>% group_by(game_id, season, home_on_5) %>% fun.ZS_H_all(., "home_on_5") %>% rename(player = home_on_5) %>% data.frame()
    h6 <- data %>% group_by(game_id, season, home_on_6) %>% fun.ZS_H_all(., "home_on_6") %>% rename(player = home_on_6) %>% data.frame()
    
    a1 <- data %>% group_by(game_id, season, away_on_1) %>% fun.ZS_A_all(., "away_on_1") %>% rename(player = away_on_1) %>% data.frame()
    a2 <- data %>% group_by(game_id, season, away_on_2) %>% fun.ZS_A_all(., "away_on_2") %>% rename(player = away_on_2) %>% data.frame()
    a3 <- data %>% group_by(game_id, season, away_on_3) %>% fun.ZS_A_all(., "away_on_3") %>% rename(player = away_on_3) %>% data.frame()
    a4 <- data %>% group_by(game_id, season, away_on_4) %>% fun.ZS_A_all(., "away_on_4") %>% rename(player = away_on_4) %>% data.frame()
    a5 <- data %>% group_by(game_id, season, away_on_5) %>% fun.ZS_A_all(., "away_on_5") %>% rename(player = away_on_5) %>% data.frame()
    a6 <- data %>% group_by(game_id, season, away_on_6) %>% fun.ZS_A_all(., "away_on_6") %>% rename(player = away_on_6) %>% data.frame()
    
    # Join
    merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
    
    summed <- merged %>% 
      group_by(player, game_id, season) %>%  
      summarise(Team = first(Team),
                OZS =  sum(OZS), 
                NZS =  sum(NZS), 
                DZS =  sum(DZS)
                ) %>% 
      arrange(player) %>% 
      data.frame()
    
    return(summed)
    
    }
  
  # Boxscore
  fun.counts_all <- function(data, venue) {
    
    if (venue == "home_team") {
      
      # Counts
      counts_1 <- data %>% 
        filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE"), 
               event_team == home_team
               ) %>% 
        group_by(event_player_1, game_id, season) %>% 
        summarise(Team = first(home_team),
                  G = sum(event_type == "GOAL"),
                  iSF = sum(event_type %in% st.shot_events), 
                  iFF = sum(event_type %in% st.fenwick_events), 
                  iCF = sum(event_type %in% st.corsi_events), 
                  ixG = sum(na.omit(pred_goal)), 
                  
                  GIVE = sum(event_type == "GIVE"), 
                  TAKE = sum(event_type == "TAKE")
                  ) %>% 
        rename(player = event_player_1) %>% 
        data.frame()
      
      counts_2 <- data %>% 
        filter(event_type %in% "GOAL", 
               event_team == home_team
               ) %>% 
        group_by(event_player_2, game_id, season) %>% 
        summarise(Team = first(home_team), 
                  A1 = sum(event_type == "GOAL")
                  ) %>% 
        rename(player = event_player_2) %>% 
        data.frame()
      
      counts_3 <- data %>% 
        filter(event_type == "GOAL", 
               event_team == home_team
               ) %>% 
        group_by(event_player_3, game_id, season) %>%
        summarise(Team = first(home_team), 
                  A2 = sum(event_type == "GOAL")
                  ) %>% 
        rename(player = event_player_3) %>% 
        data.frame()
      
      # Join
      merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
      
      joined <- merged %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
        mutate(Points = G + A1 + A2) %>% 
        select(player, game_id, season, Team, 
               G, A1, A2, Points, iSF, iFF, iCF, ixG, 
               GIVE, TAKE
               ) %>% 
        arrange(player, game_id) %>% 
        data.frame()
      
      return(joined)
      
      }
    else {
      
      # Compile
      counts_1 <- data %>% 
        filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE"), 
               event_team == away_team
               ) %>% 
        group_by(event_player_1, game_id, season) %>% 
        summarise(Team = first(away_team),
                  G = sum(event_type == "GOAL"),
                  iSF = sum(event_type %in% st.shot_events), 
                  iFF = sum(event_type %in% st.fenwick_events), 
                  iCF = sum(event_type %in% st.corsi_events), 
                  ixG = sum(na.omit(pred_goal)), 
                  
                  GIVE = sum(event_type == "GIVE"), 
                  TAKE = sum(event_type == "TAKE")
                  ) %>% 
        rename(player = event_player_1) %>% 
        data.frame()
      
      counts_2 <- data %>% 
        filter(event_type %in% c("GOAL"), 
               event_team == away_team
               ) %>% 
        group_by(event_player_2, game_id, season) %>% 
        summarise(Team = first(away_team), 
                  A1 = sum(event_type == "GOAL")
                  ) %>% 
        rename(player = event_player_2)
      
      counts_3 <- data %>% 
        filter(event_type == "GOAL", 
               event_team == away_team
               ) %>% 
        group_by(event_player_3, game_id, season) %>%
        summarise(Team = first(away_team), 
                  A2 = sum(event_type == "GOAL")
                  ) %>% 
        rename(player = event_player_3) %>% 
        data.frame()
      
      # Join
      merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
      
      joined <- merged %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
        mutate(Points = G + A1 + A2) %>% 
        select(player, game_id, season, Team, 
               G, A1, A2, Points, iSF, iFF, iCF, ixG, 
               GIVE, TAKE
               ) %>% 
        arrange(player, game_id) %>% 
        data.frame()
      
      return(joined)
      
      }
    
    }
  fun.faceoff_all <- function(data, venue) {
    
    if (venue == "home_team") {
      
      faceoffs <- data %>% 
        filter(event_type == "FAC") %>% 
        group_by(event_player_2, game_id, season) %>% 
        mutate(FOW = 1 * (home_team == event_team), 
               FOL = 1 * (away_team == event_team)
               ) %>% 
        summarise(Team = first(home_team),
                  FOW = sum(FOW), 
                  FOL = sum(FOL)
                  ) %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
        rename(player = event_player_2) %>% 
        ungroup() %>% 
        arrange(player, game_id) %>% 
        data.frame()
      
      return(faceoffs)
      
      }
    else {
      
      faceoffs <- data %>% 
        filter(event_type == "FAC") %>% 
        group_by(event_player_1, game_id, season) %>% 
        mutate(FOW = 1 * (away_team == event_team), 
               FOL = 1 * (home_team == event_team)
               ) %>% 
        summarise(Team = first(away_team),
                  FOW = sum(FOW), 
                  FOL = sum(FOL)
                  ) %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
        rename(player = event_player_1) %>% 
        ungroup() %>% 
        arrange(player, game_id) %>% 
        data.frame()
      
      return(faceoffs)
      
      }
    
    }
  fun.penalty_all <- function(data, venue) {
    
    if (venue == "home_team") {
      
      pen_1 <- data %>% 
        filter(event_team == home_team, 
               event_type %in% c("PENL", "HIT", "BLOCK")
               ) %>% 
        group_by(event_player_1, game_id, season) %>% 
        summarise(Team = first(home_team),
                  iPENT2 = sum(na.omit(1 * (event_type == "PENL") +
                                         1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                         1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))), 
                  iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                  iHF = sum(na.omit(event_type == "HIT"))
                  ) %>% 
        rename(player = event_player_1) %>% 
        data.frame()
      
      
      pen_2 <- data %>% 
        filter(event_team == away_team, 
               event_type %in% c("PENL", "HIT", "BLOCK")
               ) %>% 
        group_by(event_player_2, game_id, season) %>% 
        summarise(Team = first(home_team), 
                  iPEND2 = sum(na.omit(1 * (event_type == "PENL") +
                                         1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                         1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                  iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                  iHA = sum(na.omit(event_type == "HIT")), 
                  iBLK = sum(event_type == "BLOCK")
                  ) %>% 
        rename(player = event_player_2) %>% 
        data.frame()
      
      
      merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
      
      joined <- merged %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
        mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
        select(player, game_id, season, Team, 
               iBLK, iHF, iHA, 
               iPENT2, iPEND2, iPENT5, iPEND5
               ) %>% 
        arrange(player, game_id) %>% 
        data.frame()
      
      return(joined)
      
      }
    else {
      
      pen_1 <- data %>% 
        filter(event_team == away_team, 
               event_type %in% c("PENL", "HIT", "BLOCK")
               ) %>% 
        group_by(event_player_1, game_id, season) %>% 
        summarise(Team = first(away_team),
                  iPENT2 = sum(na.omit(1 * (event_type == "PENL") +
                                         1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                         1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))), 
                  iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                  iHF = sum(na.omit(event_type == "HIT"))
                  ) %>% 
        rename(player = event_player_1) %>% 
        data.frame()
      
      pen_2 <- data %>% 
        filter(event_team == home_team, 
               event_type %in% c("PENL", "HIT", "BLOCK")
               ) %>% 
        group_by(event_player_2, game_id, season) %>% 
        summarise(Team = first(away_team),
                  iPEND2 = sum(na.omit(1 * (event_type == "PENL") +
                                         1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                         1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                  iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                  iHA = sum(na.omit(event_type == "HIT")), 
                  iBLK = sum(event_type == "BLOCK")
                  ) %>% 
        rename(player = event_player_2) %>% 
        data.frame()
      
      
      merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
      
      joined <- merged %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
        select(player, game_id, season, Team, 
               iBLK, iHF, iHA, 
               iPENT2, iPEND2, iPENT5, iPEND5
               ) %>% 
        arrange(player, game_id) %>% 
        data.frame()
      
      return(joined)
      
      }
    
    }
  
  # Run Functions
  on_ice_all <- fun.onice_combine_all(data = pbp_data)
  
  zone_starts_all <- fun.ZS_compute_all(data = pbp_data)
  
  counts_all <- fun.counts_all(data = pbp_data, "home_team") %>% 
    rbind(., fun.counts_all(data = pbp_data, "away_team")) %>% 
    arrange(player, game_id)
  
  faceoff_all <- fun.faceoff_all(data = pbp_data, "home_team") %>% 
    rbind(., fun.faceoff_all(data = pbp_data, "away_team")) %>% 
    arrange(player, game_id)
  
  penalty_all <- fun.penalty_all(data = pbp_data, "home_team") %>% 
    rbind(., fun.penalty_all(data = pbp_data, "away_team")) %>% 
    arrange(player, game_id)
  
  # Team TOI for TOI_perc calculation
  team_TOI_all <- rbind(
    pbp_data %>% 
      group_by(game_id, season, home_team) %>% 
      summarise(TOI = sum(event_length) / 60) %>% 
      rename(Team = home_team) %>% 
      data.frame(), 
    
    pbp_data %>% 
      group_by(game_id, season, away_team) %>% 
      summarise(TOI = sum(event_length) / 60) %>% 
      rename(Team = away_team) %>% 
      data.frame()
    ) %>% 
    group_by(game_id, season, Team) %>% 
    summarise(t_TOI = sum(TOI)) %>% 
    arrange(game_id, Team) %>% 
    data.frame()
  
  
  ## --------------- ##
  ##   Combine All   ##
  ## --------------- ##
  
  test_join <- on_ice_all %>% 
    full_join(., zone_starts_all, by = c("player", "game_id", "season", "Team")) %>% 
    full_join(., counts_all, by = c("player", "game_id", "season", "Team")) %>% 
    full_join(., faceoff_all, by = c("player", "game_id", "season", "Team")) %>% 
    full_join(., penalty_all, by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., team_TOI_all, by = c("game_id", "season", "Team")) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
    mutate_if(is.logical, funs(replace(., is.na(.), 0)))
  
  # Remove goalies
  fun.goalie_remove <- function(data_) {
    
    # Identifies goalies within a given pbp data.frame & returns a data.frame to join for removal
    goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data_$home_goalie, data_$away_goalie))))), 
                                is_goalie = 1)
    
    goalie_return$player <- as.character(goalie_return$player)
    
    return(goalie_return)
    
    }
  goalieremove <- fun.goalie_remove(data_ = pbp_data)
  
  all <- test_join %>% 
    left_join(., goalieremove, "player") %>% 
    filter(is.na(is_goalie)) %>% 
    select(-c(is_goalie)) %>% 
    arrange(player, game_id) %>% 
    filter(!is.na(player), 
           !is.na(game_date) # filtering to catch pbp data issues for all situations
           ) %>% 
    select(player, game_id, game_date, season, Team, Opponent, is_home,  
           TOI, G:ixG, iBLK, iHF, iHA, GIVE, TAKE, iPENT2:iPEND5, FOW, FOL, OZS, NZS, DZS, 
           t_TOI
           ) %>% 
    data.frame()
  
  }


#######################


## ----------------------- ##
##   Even-Strength Games   ##
## ----------------------- ##

#############################

# Skaters: On Ice Corsi For / Against, TOI, Team - per game
fun.oniceCorsiH <- function(data, player_slot) {
  
  hold <- data %>% 
    summarise(TOI = sum(event_length) / 60, 
              
              Team = first(home_team), 
              Opponent = first(away_team), 
              is_home = 1, 
              
              # Normal Score Adjustment
              GF =  sum((event_type == "GOAL" & event_team == home_team) * score_adj_EV$home_goal_adj[home_lead_state]), 
              GA =  sum((event_type == "GOAL" & event_team == away_team) * score_adj_EV$away_goal_adj[home_lead_state]), 
              SF =  sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_EV$home_shots_adj[home_lead]), 
              SA =  sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_EV$away_shots_adj[home_lead]), 
              
              FF =  sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_EV$home_fenwick_adj[home_lead]), 
              FA =  sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_EV$away_fenwick_adj[home_lead]),
              CF =  sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_EV$home_corsi_adj[home_lead]), 
              CA =  sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_EV$away_corsi_adj[home_lead]),
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_EV$home_xG_adj[home_lead_state])),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_EV$away_xG_adj[home_lead_state])), 
              
              # Score Adjustment + State Adjustment
              GF_state =  sum((event_type == "GOAL" & event_team == home_team) * score_adj_EV$home_goal_adj[home_lead_state] * state_adj_EV$Goals[str_state]), 
              GA_state =  sum((event_type == "GOAL" & event_team == away_team) * score_adj_EV$away_goal_adj[home_lead_state] * state_adj_EV$Goals[str_state]), 
              SF_state =  sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_EV$home_shots_adj[home_lead] * state_adj_EV$Shots[str_state]), 
              SA_state =  sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_EV$away_shots_adj[home_lead] * state_adj_EV$Shots[str_state]), 
              
              FF_state =  sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_EV$home_fenwick_adj[home_lead] * state_adj_EV$Fenwick[str_state]), 
              FA_state =  sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_EV$away_fenwick_adj[home_lead] * state_adj_EV$Fenwick[str_state]), 
              CF_state =  sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_EV$home_corsi_adj[home_lead] * state_adj_EV$Corsi[str_state]), 
              CA_state =  sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_EV$away_corsi_adj[home_lead] * state_adj_EV$Corsi[str_state]),
              
              xGF_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_EV$home_xG_adj[home_lead_state] * state_adj_EV$xG[str_state])),
              xGA_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_EV$away_xG_adj[home_lead_state] * state_adj_EV$xG[str_state]))
              )
  
  return(hold)

  }
fun.oniceCorsiA <- function(data, player_slot) {
  
  hold <- data %>% 
    summarise(TOI = sum(event_length) / 60, 
              
              Team = first(away_team), 
              Opponent = first(home_team), 
              is_home = 0, 
              
              # Normal Score Adjustment
              GF =  sum((event_type == "GOAL" & event_team == away_team) * score_adj_EV$away_goal_adj[home_lead_state]), 
              GA =  sum((event_type == "GOAL" & event_team == home_team) * score_adj_EV$home_goal_adj[home_lead_state]), 
              SF =  sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_EV$away_shots_adj[home_lead]), 
              SA =  sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_EV$home_shots_adj[home_lead]), 
              
              FF =  sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_EV$away_fenwick_adj[home_lead]), 
              FA =  sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_EV$home_fenwick_adj[home_lead]),
              CF =  sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_EV$away_corsi_adj[home_lead]), 
              CA =  sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_EV$home_corsi_adj[home_lead]),
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_EV$away_xG_adj[home_lead_state])),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_EV$home_xG_adj[home_lead_state])), 
              
              # Score Adjustment + State Adjustment
              GF_state =  sum((event_type == "GOAL" & event_team == away_team) * score_adj_EV$away_goal_adj[home_lead_state] * state_adj_EV$Goals[str_state]), 
              GA_state =  sum((event_type == "GOAL" & event_team == home_team) * score_adj_EV$home_goal_adj[home_lead_state] * state_adj_EV$Goals[str_state]), 
              SF_state =  sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_EV$away_shots_adj[home_lead] * state_adj_EV$Shots[str_state]), 
              SA_state =  sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_EV$home_shots_adj[home_lead] * state_adj_EV$Shots[str_state]), 
              
              FF_state =  sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_EV$away_fenwick_adj[home_lead] * state_adj_EV$Fenwick[str_state]), 
              FA_state =  sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_EV$home_fenwick_adj[home_lead] * state_adj_EV$Fenwick[str_state]), 
              CF_state =  sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_EV$away_corsi_adj[home_lead] * state_adj_EV$Corsi[str_state]), 
              CA_state =  sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_EV$home_corsi_adj[home_lead] * state_adj_EV$Corsi[str_state]),
              
              xGF_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_EV$away_xG_adj[home_lead_state] * state_adj_EV$xG[str_state])),
              xGA_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_EV$home_xG_adj[home_lead_state] * state_adj_EV$xG[str_state]))
              )
  
  return(hold)

  }
fun.componiceCorsi <- function(data) {
  
  print("on_ice_home", quote = F)
  h1 <- data %>% group_by(game_id, game_date, season, home_on_1, home_team) %>% fun.oniceCorsiH(., "home_on_1") %>% rename(player = home_on_1) %>% data.frame()
  h2 <- data %>% group_by(game_id, game_date, season, home_on_2, home_team) %>% fun.oniceCorsiH(., "home_on_2") %>% rename(player = home_on_2) %>% data.frame()
  h3 <- data %>% group_by(game_id, game_date, season, home_on_3, home_team) %>% fun.oniceCorsiH(., "home_on_3") %>% rename(player = home_on_3) %>% data.frame()
  h4 <- data %>% group_by(game_id, game_date, season, home_on_4, home_team) %>% fun.oniceCorsiH(., "home_on_4") %>% rename(player = home_on_4) %>% data.frame()
  h5 <- data %>% group_by(game_id, game_date, season, home_on_5, home_team) %>% fun.oniceCorsiH(., "home_on_5") %>% rename(player = home_on_5) %>% data.frame()
  h6 <- data %>% group_by(game_id, game_date, season, home_on_6, home_team) %>% fun.oniceCorsiH(., "home_on_6") %>% rename(player = home_on_6) %>% data.frame()
  
  print("on_ice_away", quote = F)
  a1 <- data %>% group_by(game_id, game_date, season, away_on_1, away_team) %>% fun.oniceCorsiA(., "away_on_1") %>% rename(player = away_on_1) %>% data.frame()
  a2 <- data %>% group_by(game_id, game_date, season, away_on_2, away_team) %>% fun.oniceCorsiA(., "away_on_2") %>% rename(player = away_on_2) %>% data.frame()
  a3 <- data %>% group_by(game_id, game_date, season, away_on_3, away_team) %>% fun.oniceCorsiA(., "away_on_3") %>% rename(player = away_on_3) %>% data.frame()
  a4 <- data %>% group_by(game_id, game_date, season, away_on_4, away_team) %>% fun.oniceCorsiA(., "away_on_4") %>% rename(player = away_on_4) %>% data.frame()
  a5 <- data %>% group_by(game_id, game_date, season, away_on_5, away_team) %>% fun.oniceCorsiA(., "away_on_5") %>% rename(player = away_on_5) %>% data.frame()
  a6 <- data %>% group_by(game_id, game_date, season, away_on_6, away_team) %>% fun.oniceCorsiA(., "away_on_6") %>% rename(player = away_on_6) %>% data.frame()
  
  
  # Join all data.frames
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
  
  merge_return <- merged %>% 
    group_by(player, game_id, game_date, season) %>%  
    summarise(Team =     first(Team), 
              Opponent = first(Opponent), 
              is_home =  first(is_home), 
              
              TOI =      sum(TOI), 
              GF =       sum(GF), 
              GA =       sum(GA), 
              SF =       sum(SF), 
              SA =       sum(SA), 
              FF =       sum(FF), 
              FA =       sum(FA), 
              CF =       sum(CF), 
              CA =       sum(CA), 
              xGF =      sum(xGF), 
              xGA =      sum(xGA), 
              
              GF_state =      sum(GF_state), 
              GA_state =      sum(GA_state), 
              SF_state =      sum(SF_state), 
              SA_state =      sum(SA_state), 
              FF_state =      sum(FF_state), 
              FA_state =      sum(FA_state), 
              CF_state =      sum(CF_state), 
              CA_state =      sum(CA_state), 
              xGF_state =     sum(xGF_state), 
              xGA_state =     sum(xGA_state)
              ) %>% 
    filter(!is.na(player)) %>% 
    data.frame()
  
  return(merge_return)

  }

# Teams: On Ice Corsi For / Against, TOI - per game
fun.oniceTeamCorsiH <- function(data) {
  
  hold <- data %>% 
    summarise(TOI = sum(event_length) / 60, 
              Team = first(home_team), 
              
              # Normal Score Adjustment
              GF =  sum((event_type == "GOAL" & event_team == home_team) * score_adj_EV$home_goal_adj[home_lead_state]), 
              GA =  sum((event_type == "GOAL" & event_team == away_team) * score_adj_EV$away_goal_adj[home_lead_state]), 
              SF =  sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_EV$home_shots_adj[home_lead]), 
              SA =  sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_EV$away_shots_adj[home_lead]), 
              
              FF =  sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_EV$home_fenwick_adj[home_lead]), 
              FA =  sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_EV$away_fenwick_adj[home_lead]),
              CF =  sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_EV$home_corsi_adj[home_lead]), 
              CA =  sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_EV$away_corsi_adj[home_lead]),
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_EV$home_xG_adj[home_lead_state])),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_EV$away_xG_adj[home_lead_state])), 
              
              # Score Adjustment + State Adjustment
              GF_state =  sum((event_type == "GOAL" & event_team == home_team) * score_adj_EV$home_goal_adj[home_lead_state] * state_adj_EV$Goals[str_state]), 
              GA_state =  sum((event_type == "GOAL" & event_team == away_team) * score_adj_EV$away_goal_adj[home_lead_state] * state_adj_EV$Goals[str_state]), 
              SF_state =  sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_EV$home_shots_adj[home_lead] * state_adj_EV$Shots[str_state]), 
              SA_state =  sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_EV$away_shots_adj[home_lead] * state_adj_EV$Shots[str_state]), 
              
              FF_state =  sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_EV$home_fenwick_adj[home_lead] * state_adj_EV$Fenwick[str_state]), 
              FA_state =  sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_EV$away_fenwick_adj[home_lead] * state_adj_EV$Fenwick[str_state]), 
              CF_state =  sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_EV$home_corsi_adj[home_lead] * state_adj_EV$Corsi[str_state]), 
              CA_state =  sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_EV$away_corsi_adj[home_lead] * state_adj_EV$Corsi[str_state]),
              
              xGF_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_EV$home_xG_adj[home_lead_state] * state_adj_EV$xG[str_state])),
              xGA_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_EV$away_xG_adj[home_lead_state] * state_adj_EV$xG[str_state]))
              )
  
  return(hold)
  
  }
fun.oniceTeamCorsiA <- function(data) {
  
  hold <- data %>% 
    summarise(TOI = sum(event_length) / 60, 
              Team = first(away_team), 
              
              # Normal Score Adjustment
              GF =  sum((event_type == "GOAL" & event_team == away_team) * score_adj_EV$away_goal_adj[home_lead_state]), 
              GA =  sum((event_type == "GOAL" & event_team == home_team) * score_adj_EV$home_goal_adj[home_lead_state]), 
              SF =  sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_EV$away_shots_adj[home_lead]), 
              SA =  sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_EV$home_shots_adj[home_lead]), 
              
              FF =  sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_EV$away_fenwick_adj[home_lead]), 
              FA =  sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_EV$home_fenwick_adj[home_lead]),
              CF =  sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_EV$away_corsi_adj[home_lead]), 
              CA =  sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_EV$home_corsi_adj[home_lead]),
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_EV$away_xG_adj[home_lead_state])),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_EV$home_xG_adj[home_lead_state])), 
              
              # Score Adjustment + State Adjustment
              GF_state =  sum((event_type == "GOAL" & event_team == away_team) * score_adj_EV$away_goal_adj[home_lead_state] * state_adj_EV$Goals[str_state]), 
              GA_state =  sum((event_type == "GOAL" & event_team == home_team) * score_adj_EV$home_goal_adj[home_lead_state] * state_adj_EV$Goals[str_state]), 
              SF_state =  sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_EV$away_shots_adj[home_lead] * state_adj_EV$Shots[str_state]), 
              SA_state =  sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_EV$home_shots_adj[home_lead] * state_adj_EV$Shots[str_state]), 
              
              FF_state =  sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_EV$away_fenwick_adj[home_lead] * state_adj_EV$Fenwick[str_state]), 
              FA_state =  sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_EV$home_fenwick_adj[home_lead] * state_adj_EV$Fenwick[str_state]), 
              CF_state =  sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_EV$away_corsi_adj[home_lead] * state_adj_EV$Corsi[str_state]), 
              CA_state =  sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_EV$home_corsi_adj[home_lead] * state_adj_EV$Corsi[str_state]),
              
              xGF_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_EV$away_xG_adj[home_lead_state] * state_adj_EV$xG[str_state])),
              xGA_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_EV$home_xG_adj[home_lead_state] * state_adj_EV$xG[str_state]))
              )
  
  return(hold)
  
  }
fun.componiceCorsiTeam <- function(data) {
  
  print("team_on_ice_home", quote = F)
  h_df <- data %>% 
    group_by(game_id, season, home_team) %>% 
    fun.oniceTeamCorsiH(.) %>% 
    data.frame()
  
  print("team_on_ice_away", quote = F)
  a_df <- data %>% 
    group_by(game_id, season, away_team) %>% 
    fun.oniceTeamCorsiA(.) %>% 
    data.frame()
  
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h_df, a_df))
  
  merge_return <- merged %>% 
    group_by(Team, game_id, season) %>% 
    summarise_at(vars(TOI, 
                      GF, GA, SF, SA, FF, FA, CF, CA, xGF, xGA, 
                      GF_state, GA_state, SF_state, SA_state, FF_state, FA_state, CF_state, CA_state, xGF_state, xGA_state), 
                 funs(sum)
                 ) %>% 
    data.frame()
  
  return(merge_return)

  }

# Merge skaters and team on-ice metrics
fun.shotmetrics <- function(data) {
  
  # Run functions
  skater <- fun.componiceCorsi(data)
  team <- fun.componiceCorsiTeam(data)
  
  joined <- left_join(skater, team, by = c("game_id", "Team", "season"))
  
  names(joined) <- c("player", "game_id", "game_date", "season", "Team", "Opponent", "is_home", 
                     
                     "TOI", 
                     "onGF", "onGA", "onSF", "onSA", "onFF", "onFA", "onCF", "onCA", "onxGF", "onxGA", 
                     "onGF_state", "onGA_state", "onSF_state", "onSA_state", "onFF_state", "onFA_state", "onCF_state", "onCA_state", "onxGF_state", "onxGA_state", 
                     
                     "t_TOI", 
                     "t_GF", "t_GA", "t_SF", "t_SA", "t_FF", "t_FA", "t_CF", "t_CA", "t_xGF", "t_xGA",
                     "t_GF_state", "t_GA_state", "t_SF_state", "t_SA_state", "t_FF_state", "t_FA_state", "t_CF_state", "t_CA_state", "t_xGF_state", "t_xGA_state"
                     )
  
  # Remove goalies
  fun.goalie_remove <- function(data_) {
    
    # Identifies goalies within a given pbp data.frame & returns a data.frame to join for removal
    goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data_$home_goalie, data_$away_goalie))))), 
                                is_goalie = 1)
    
    goalie_return$player <- as.character(goalie_return$player)
    
    return(goalie_return)
  
    }
  
  goalieremove <- fun.goalie_remove(data_ = data)
  
  all <- joined %>% 
    left_join(., goalieremove, "player") %>% 
    filter(is.na(is_goalie)) %>% 
    select(-c(is_goalie)) %>% 
    arrange(player, game_id) %>% 
    data.frame()
  
  return(all)

  }

# Zone Starts
fun.ZS_H <- function(data, venue) {
  
  hold <- data %>% 
    filter(event_type == "FAC") %>%  
    summarise(Team = first(home_team),
              OZS = sum(event_type == "FAC" & home_zone == "Off"), 
              NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
              DZS = sum(event_type == "FAC" & home_zone == "Def")
              )
  
  return(hold)

  }
fun.ZS_A <- function(data, venue) {
  
  hold <- data %>% 
    filter(event_type == "FAC") %>%  
    summarise(Team = first(away_team),
              OZS = sum(event_type == "FAC" & home_zone == "Def"), 
              NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
              DZS = sum(event_type == "FAC" & home_zone == "Off")
              )
  
  return(hold)

  }
fun.ZS_compute <- function(data) {
  
  print("zone_starts_home", quote = F)
  h1 <- data %>% group_by(game_id, season, home_on_1) %>% fun.ZS_H(., "home_on_1") %>% rename(player = home_on_1) %>% data.frame()
  h2 <- data %>% group_by(game_id, season, home_on_2) %>% fun.ZS_H(., "home_on_2") %>% rename(player = home_on_2) %>% data.frame()
  h3 <- data %>% group_by(game_id, season, home_on_3) %>% fun.ZS_H(., "home_on_3") %>% rename(player = home_on_3) %>% data.frame()
  h4 <- data %>% group_by(game_id, season, home_on_4) %>% fun.ZS_H(., "home_on_4") %>% rename(player = home_on_4) %>% data.frame()
  h5 <- data %>% group_by(game_id, season, home_on_5) %>% fun.ZS_H(., "home_on_5") %>% rename(player = home_on_5) %>% data.frame()
  h6 <- data %>% group_by(game_id, season, home_on_6) %>% fun.ZS_H(., "home_on_6") %>% rename(player = home_on_6) %>% data.frame()
  
  print("zone_starts_away", quote = F)
  a1 <- data %>% group_by(game_id, season, away_on_1) %>% fun.ZS_A(., "away_on_1") %>% rename(player = away_on_1) %>% data.frame()
  a2 <- data %>% group_by(game_id, season, away_on_2) %>% fun.ZS_A(., "away_on_2") %>% rename(player = away_on_2) %>% data.frame()
  a3 <- data %>% group_by(game_id, season, away_on_3) %>% fun.ZS_A(., "away_on_3") %>% rename(player = away_on_3) %>% data.frame()
  a4 <- data %>% group_by(game_id, season, away_on_4) %>% fun.ZS_A(., "away_on_4") %>% rename(player = away_on_4) %>% data.frame()
  a5 <- data %>% group_by(game_id, season, away_on_5) %>% fun.ZS_A(., "away_on_5") %>% rename(player = away_on_5) %>% data.frame()
  a6 <- data %>% group_by(game_id, season, away_on_6) %>% fun.ZS_A(., "away_on_6") %>% rename(player = away_on_6) %>% data.frame()
  
  # Join
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
  
  summed <- merged %>% 
    group_by(player, game_id, season) %>%  
    summarise(Team = first(Team),
              OZS =  sum(OZS), 
              NZS =  sum(NZS), 
              DZS =  sum(DZS)
              ) %>% 
    arrange(player) %>% 
    data.frame()
  
  return(summed)

  }

# Boxscore
fun.counts <- function(data, venue) {
  
  if (venue == "home_team") {
    
    # Counts
    counts_1 <- data %>% 
      filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE"), 
             event_team == home_team
             ) %>% 
      group_by(event_player_1, game_id, season) %>% 
      summarise(Team = first(home_team),
                G = sum(event_type == "GOAL"),
                G_adj = sum((event_type == "GOAL") * score_adj_EV$home_goal_adj[home_lead_state]),
                iSF = sum(event_type %in% st.shot_events),
                iCF = sum(event_type %in% st.corsi_events), 
                iCF_adj = sum((event_type %in% st.corsi_events) * score_adj_EV$home_corsi_adj[home_lead]), 
                iFF = sum(event_type %in% st.fenwick_events),
                ixG = sum(na.omit(pred_goal)), 
                ixG_adj = sum(na.omit(pred_goal * score_adj_EV$home_xG_adj[home_lead_state])), 
                
                GIVE_o = sum(event_type == "GIVE" & event_zone == "Off"), 
                GIVE_n = sum(event_type == "GIVE" & event_zone == "Neu"), 
                GIVE_d = sum(event_type == "GIVE" & event_zone == "Def"), 
                
                TAKE_o = sum(event_type == "TAKE" & event_zone == "Off"), 
                TAKE_n = sum(event_type == "TAKE" & event_zone == "Neu"), 
                TAKE_d = sum(event_type == "TAKE" & event_zone == "Def"), 
                
                GIVE_adj = sum((event_type == "GIVE") * score_adj_EV$home_GIVE_adj[home_lead_state]),
                TAKE_adj = sum((event_type == "TAKE") * score_adj_EV$home_TAKE_adj[home_lead])
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    counts_2 <- data %>% 
      filter(event_type %in% c("GOAL"), 
             event_team == home_team
             ) %>% 
      group_by(event_player_2, game_id, season) %>% 
      summarise(Team = first(home_team), 
                A1 = sum(event_type == "GOAL"), 
                A1_adj = sum((event_type == "GOAL") * score_adj_EV$home_A1_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    counts_3 <- data %>% 
      filter(event_type == "GOAL", 
             event_team == home_team
             ) %>% 
      group_by(event_player_3, game_id, season) %>%
      summarise(Team = first(home_team), 
                A2 = sum(event_type == "GOAL"), 
                A2_adj = sum((event_type == "GOAL") * score_adj_EV$home_A2_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_3) %>% 
      data.frame()
    
    # Join
    merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      mutate(Points = G + A1 + A2, 
             Points_adj = G_adj + A1_adj + A2_adj
             ) %>% 
      select(player, game_id, season, Team, 
             G, A1, A2, Points, G_adj, A1_adj, A2_adj, Points_adj,
             iSF, iCF, iFF, ixG, iCF_adj, ixG_adj, 
             GIVE_o, GIVE_n, GIVE_d, TAKE_o, TAKE_n, TAKE_d,
             GIVE_adj, TAKE_adj
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
  
    }
  else {
    
    # Compile
    counts_1 <- data %>% 
      filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE"), 
             event_team == away_team
             ) %>% 
      group_by(event_player_1, game_id, season) %>% 
      summarise(Team = first(away_team),
                G = sum(event_type == "GOAL"),
                G_adj = sum((event_type == "GOAL") * score_adj_EV$away_goal_adj[home_lead_state]),
                iSF = sum(event_type %in% st.shot_events),
                iCF = sum(event_type %in% st.corsi_events),
                iCF_adj = sum((event_type %in% st.corsi_events) * score_adj_EV$away_corsi_adj[home_lead]), 
                iFF = sum(event_type %in% st.fenwick_events), 
                ixG = sum(na.omit(pred_goal)), 
                ixG_adj = sum(na.omit(pred_goal * score_adj_EV$away_xG_adj[home_lead_state])), 
                
                GIVE_o = sum(event_type == "GIVE" & event_zone == "Off"), 
                GIVE_n = sum(event_type == "GIVE" & event_zone == "Neu"), 
                GIVE_d = sum(event_type == "GIVE" & event_zone == "Def"), 
                
                TAKE_o = sum(event_type == "TAKE" & event_zone == "Off"), 
                TAKE_n = sum(event_type == "TAKE" & event_zone == "Neu"), 
                TAKE_d = sum(event_type == "TAKE" & event_zone == "Def"), 
                
                GIVE_adj = sum((event_type == "GIVE") * score_adj_EV$away_GIVE_adj[home_lead_state]), 
                TAKE_adj = sum((event_type == "TAKE") * score_adj_EV$away_TAKE_adj[home_lead])
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    counts_2 <- data %>% 
      filter(event_type %in% c("GOAL"), 
             event_team == away_team
             ) %>% 
      group_by(event_player_2, game_id, season) %>% 
      summarise(Team = first(away_team), 
                A1 = sum(event_type == "GOAL"), 
                A1_adj = sum((event_type == "GOAL") * score_adj_EV$away_A1_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    counts_3 <- data %>% 
      filter(event_type == "GOAL", 
             event_team == away_team
             ) %>% 
      group_by(event_player_3, game_id, season) %>%
      summarise(Team = first(away_team), 
                A2 = sum(event_type == "GOAL"), 
                A2_adj = sum((event_type == "GOAL") * score_adj_EV$away_A2_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_3) %>% 
      data.frame()
    
    # Join
    merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      mutate(Points = G + A1 + A2, 
             Points_adj = G_adj + A1_adj + A2_adj
             ) %>% 
      select(player, game_id, season, Team, 
             G, A1, A2, Points, G_adj, A1_adj, A2_adj, Points_adj,
             iSF, iCF, iFF, ixG, iCF_adj, ixG_adj, 
             GIVE_o, GIVE_n, GIVE_d, TAKE_o, TAKE_n, TAKE_d,
             GIVE_adj, TAKE_adj
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
  
    }

  }
fun.faceoff <- function(data, venue) {
  
  if (venue == "home_team") {
    
    faceoffs <- data %>% 
      filter(event_type == "FAC") %>% 
      group_by(event_player_2, game_id, season) %>% 
      mutate(FOW = 1 * (home_team == event_team), 
             FOL = 1 * (away_team == event_team)
             ) %>% 
      summarise(Team = first(home_team),
                FOW = sum(FOW), 
                FOL = sum(FOL)
                ) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      rename(player = event_player_2) %>% 
      ungroup() %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(faceoffs)
  
    }
  else {
    
    faceoffs <- data %>% 
      filter(event_type == "FAC") %>% 
      group_by(event_player_1, game_id, season) %>% 
      mutate(FOW = 1 * (away_team == event_team), 
             FOL = 1 * (home_team == event_team)
             ) %>% 
      summarise(Team = first(away_team),
                FOW = sum(FOW), 
                FOL = sum(FOL)
                ) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      rename(player = event_player_1) %>% 
      ungroup() %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(faceoffs)
  
    }

  }
fun.penalty <- function(data, venue) {
  
  if (venue == "home_team") {
    
    pen_1 <- data %>% 
      filter(event_team == home_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      group_by(event_player_1, game_id, season) %>% 
      summarise(Team = first(home_team),
                iPENT2 = sum(na.omit(1 * (event_type == "PENL") + # Manny Perry's Filter - found on GitHub linked above
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHF_o = sum(event_type == "HIT" & event_zone == "Off"), 
                iHF_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHF_d = sum(event_type == "HIT" & event_zone == "Def"), 
                
                iHF_adj = sum((event_type == "HIT") * score_adj_EV$home_HF_adj[home_lead])
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    
    pen_2 <- data %>% 
      filter(event_team == away_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      group_by(event_player_2, game_id, season) %>% 
      summarise(Team = first(home_team), 
                iPEND2 = sum(na.omit(1 * (event_type == "PENL") + # Manny Perry's Filter - found on GitHub linked above
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHA_o = sum(event_type == "HIT" & event_zone == "Def"), 
                iHA_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHA_d = sum(event_type == "HIT" & event_zone == "Off"), 
                
                iHA_adj = sum((event_type == "HIT") * score_adj_EV$home_HA_adj[home_lead]), 
                iBLK = sum(event_type == "BLOCK"), 
                iBLK_adj = sum((event_type == "BLOCK") * score_adj_EV$home_block_adj[home_lead])
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    
    merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      select(player, game_id, season, Team, 
             iPENT2, iPEND2, iPENT5, iPEND5, iBLK, 
             iHF_o, iHF_n, iHF_d, iHA_o, iHA_n, iHA_d, 
             iBLK_adj, iHF_adj, iHA_adj
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
  
    }
  else {
    
    pen_1 <- data %>% 
      filter(event_team == away_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      group_by(event_player_1, game_id, season) %>% 
      summarise(Team = first(away_team),
                iPENT2 = sum(na.omit(1 * (event_type == "PENL") + # Manny Perry's Filter - found on GitHub linked above
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHF_o = sum(event_type == "HIT" & event_zone == "Off"), 
                iHF_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHF_d = sum(event_type == "HIT" & event_zone == "Def"), 
                
                iHF_adj = sum((event_type == "HIT") * score_adj_EV$away_HF_adj[home_lead])
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    pen_2 <- data %>% 
      filter(event_team == home_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      group_by(event_player_2, game_id, season) %>% 
      summarise(Team = first(away_team),
                iPEND2 = sum(na.omit(1 * (event_type == "PENL") + # Manny Perry's Filter - found on GitHub linked above
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHA_o = sum(event_type == "HIT" & event_zone == "Def"), 
                iHA_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHA_d = sum(event_type == "HIT" & event_zone == "Off"), 
                
                iHA_adj = sum((event_type == "HIT") * score_adj_EV$away_HA_adj[home_lead]), 
                iBLK = sum(event_type == "BLOCK"), 
                iBLK_adj = sum((event_type == "BLOCK") * score_adj_EV$away_block_adj[home_lead])
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    
    merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      select(player, game_id, season, Team, 
             iPENT2, iPEND2, iPENT5, iPEND5, iBLK, 
             iHF_o, iHF_n, iHF_d, iHA_o, iHA_n, iHA_d, 
             iBLK_adj, iHF_adj, iHA_adj
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
  
    }

  }

###  Run all functions
fun.combine_counts <- function(data) { 
  
  print(paste("season:", unique(data$season)), quote = F)
  
  # Filter pbp / join prior face time for corsi events
  data <- data %>% 
    filter(game_strength_state %in% st.even_strength, 
           game_period < 5
           ) %>% 
    select(-c(face_index:shift_length)) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, 
                              ifelse(scradj <= -3, -3, scradj)),
           home_lead_state = ifelse(home_lead < 0, 1, 
                                    ifelse(home_lead == 0, 2, 
                                           ifelse(home_lead > 0, 3, home_lead))), 
           str_state = ifelse(game_strength_state == "5v5", 1, 
                              ifelse(game_strength_state == "4v4", 2, 
                                     ifelse(game_strength_state == "3v3", 3, NA))), 
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           ) %>% 
    rename(pred_goal = pred_XGB_7)
  
  
  ### Run functions
  shot_metrics <- fun.shotmetrics(data)
  zone_start <- fun.ZS_compute(data)
  
  print("counts", quote = F)
  counts_all <- fun.counts(data, "home_team") %>% 
    rbind(., fun.counts(data, "away_team")) %>% 
    arrange(player, game_id)
  
  print("faceoffs", quote = F)
  faceoff_all <- fun.faceoff(data, "home_team") %>% 
    rbind(., fun.faceoff(data, "away_team")) %>% 
    arrange(player, game_id)
  
  print("penalties", quote = F)
  penalty_all <- fun.penalty(data, "home_team") %>% 
    rbind(., fun.penalty(data, "away_team")) %>% 
    arrange(player, game_id)
  
  
  # Join
  print("combine", quote = F)
  
  metrics_combined <- shot_metrics %>% 
    left_join(., zone_start,  by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., counts_all,  by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., faceoff_all, by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., penalty_all, by = c("player", "game_id", "season", "Team")) %>% 
    select(player, game_id, game_date, season, Team, Opponent, is_home, TOI, 
           G, A1, A2, Points, G_adj, A1_adj, A2_adj, Points_adj, 
           iSF, iFF, iCF, ixG, iCF_adj, ixG_adj, 
           iBLK, iBLK_adj, 
           GIVE_o:TAKE_d, GIVE_adj, TAKE_adj, 
           iHF_o:iHA_d, iHF_adj, iHA_adj, 
           FOW, FOL, 
           OZS, NZS, DZS, 
           iPENT2, iPEND2, iPENT5, iPEND5, 
           onGF:onxGA_state, 
           t_TOI:t_xGA_state
           ) %>% 
    mutate(player = ifelse(player == "SEBASTIAN.AHO" & Team == "NYI", "5EBASTIAN.AHO", player)) %>% # FIX NYI AHO
    arrange(player, game_id) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
    mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
    data.frame()
  
  return(metrics_combined)
  
  }


#############################


## ------------------- ##
##   Powerplay Games   ##
## ------------------- ##

#########################

# Skaters: On Ice Corsi For / Against, TOI, Team - per game
fun.oniceCorsiH_PP <- function(data, venue) {
  
  hold <- data %>% 
    filter(game_strength_state %in% c("5v4", "5v3", "4v3")) %>% 
    summarise(TOI = sum(event_length) / 60, 
              
              Team = first(home_team), 
              Opponent = first(away_team), 
              is_home = 1, 
              
              GF = sum((event_type == "GOAL" & event_team == home_team) * score_adj_PP$home_goal_adj[home_lead_state]) , 
              GA = sum(event_type == "GOAL" & event_team == away_team),
              
              SF = sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_PP$home_shot_adj[home_lead_state]), 
              SA = sum(event_type %in% st.shot_events & event_team == away_team), 
              FF = sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_PP$home_fenwick_adj[home_lead_state]), 
              FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
              CF = sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_PP$home_corsi_adj[home_lead_state]), 
              CA = sum(event_type %in% st.corsi_events & event_team == away_team),
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_PP$home_xG_adj[home_lead_state])),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              
              GF_state = sum((event_type == "GOAL" & event_team == home_team) * score_adj_PP$home_goal_adj[home_lead_state] * state_adj_PP$Goals[str_state]), 
              GA_state = sum(event_type == "GOAL" & event_team == away_team),
              SF_state = sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_PP$home_shot_adj[home_lead_state] * state_adj_PP$Shots[str_state]), 
              SA_state = sum(event_type %in% st.shot_events & event_team == away_team), 
              
              FF_state = sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_PP$home_fenwick_adj[home_lead_state] * state_adj_PP$Fenwick[str_state]), 
              FA_state = sum(event_type %in% st.fenwick_events & event_team == away_team),
              CF_state = sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_PP$home_corsi_adj[home_lead_state] * state_adj_PP$Corsi[str_state]), 
              CA_state = sum(event_type %in% st.corsi_events & event_team == away_team),
              
              xGF_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_PP$home_xG_adj[home_lead_state] * state_adj_PP$xG[str_state])),
              xGA_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal))
              )
  
  return(hold)

  }
fun.oniceCorsiA_PP <- function(data, venue) {
  
  hold <- data %>% 
    filter(game_strength_state %in% c("4v5", "3v5", "3v4")) %>% 
    summarise(TOI = sum(event_length) / 60, 
              
              Team = first(away_team), 
              Opponent = first(home_team), 
              is_home = 0, 
              
              GF = sum((event_type == "GOAL" & event_team == away_team) * score_adj_PP$away_goal_adj[home_lead_state]) , 
              GA = sum(event_type == "GOAL" & event_team == home_team),
              
              SF = sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_PP$away_shot_adj[home_lead_state]), 
              SA = sum(event_type %in% st.shot_events & event_team == home_team), 
              FF = sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_PP$away_fenwick_adj[home_lead_state]), 
              FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
              CF = sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_PP$away_corsi_adj[home_lead_state]), 
              CA = sum(event_type %in% st.corsi_events & event_team == home_team),
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_PP$away_xG_adj[home_lead_state])),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              
              GF_state = sum((event_type == "GOAL" & event_team == away_team) * score_adj_PP$away_goal_adj[home_lead_state] * state_adj_PP$Goals[str_state]), 
              GA_state = sum(event_type == "GOAL" & event_team == home_team),
              SF_state = sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_PP$away_shot_adj[home_lead_state] * state_adj_PP$Shots[str_state]), 
              SA_state = sum(event_type %in% st.shot_events & event_team == home_team), 
              
              FF_state = sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_PP$away_fenwick_adj[home_lead_state] * state_adj_PP$Fenwick[str_state]), 
              FA_state = sum(event_type %in% st.fenwick_events & event_team == home_team),
              CF_state = sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_PP$away_corsi_adj[home_lead_state] * state_adj_PP$Corsi[str_state]), 
              CA_state = sum(event_type %in% st.corsi_events & event_team == home_team),
              
              xGF_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_PP$away_xG_adj[home_lead_state] * state_adj_PP$xG[str_state])),
              xGA_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal))
              )
  
  return(hold)

  }
fun.componiceCorsi_PP <- function(data) {
  
  print("on_ice_home", quote = F)
  h1 <- data %>% group_by(game_id, game_date, season, home_on_1, home_team) %>% fun.oniceCorsiH_PP(., "home_on_1") %>% rename(player = home_on_1) %>% data.frame()
  h2 <- data %>% group_by(game_id, game_date, season, home_on_2, home_team) %>% fun.oniceCorsiH_PP(., "home_on_2") %>% rename(player = home_on_2) %>% data.frame()
  h3 <- data %>% group_by(game_id, game_date, season, home_on_3, home_team) %>% fun.oniceCorsiH_PP(., "home_on_3") %>% rename(player = home_on_3) %>% data.frame()
  h4 <- data %>% group_by(game_id, game_date, season, home_on_4, home_team) %>% fun.oniceCorsiH_PP(., "home_on_4") %>% rename(player = home_on_4) %>% data.frame()
  h5 <- data %>% group_by(game_id, game_date, season, home_on_5, home_team) %>% fun.oniceCorsiH_PP(., "home_on_5") %>% rename(player = home_on_5) %>% data.frame()
  h6 <- data %>% group_by(game_id, game_date, season, home_on_6, home_team) %>% fun.oniceCorsiH_PP(., "home_on_6") %>% rename(player = home_on_6) %>% data.frame()
  
  print("on_ice_away", quote = F)
  a1 <- data %>% group_by(game_id, game_date, season, away_on_1, away_team) %>% fun.oniceCorsiA_PP(., "away_on_1") %>% rename(player = away_on_1) %>% data.frame()
  a2 <- data %>% group_by(game_id, game_date, season, away_on_2, away_team) %>% fun.oniceCorsiA_PP(., "away_on_2") %>% rename(player = away_on_2) %>% data.frame()
  a3 <- data %>% group_by(game_id, game_date, season, away_on_3, away_team) %>% fun.oniceCorsiA_PP(., "away_on_3") %>% rename(player = away_on_3) %>% data.frame()
  a4 <- data %>% group_by(game_id, game_date, season, away_on_4, away_team) %>% fun.oniceCorsiA_PP(., "away_on_4") %>% rename(player = away_on_4) %>% data.frame()
  a5 <- data %>% group_by(game_id, game_date, season, away_on_5, away_team) %>% fun.oniceCorsiA_PP(., "away_on_5") %>% rename(player = away_on_5) %>% data.frame()
  a6 <- data %>% group_by(game_id, game_date, season, away_on_6, away_team) %>% fun.oniceCorsiA_PP(., "away_on_6") %>% rename(player = away_on_6) %>% data.frame()
  
  
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
  
  merge_return <- merged %>% 
    group_by(player, game_id, game_date, season) %>%  
    summarise(Team =     first(Team), 
              Opponent = first(Opponent), 
              is_home =  first(is_home), 
              
              TOI =      sum(TOI), 
              GF =       sum(GF), 
              GA =       sum(GA), 
              SF =       sum(SF), 
              SA =       sum(SA), 
              FF =       sum(FF), 
              FA =       sum(FA), 
              CF =       sum(CF), 
              CA =       sum(CA), 
              xGF =      sum(xGF), 
              xGA =      sum(xGA), 
              
              GF_state =  sum(GF_state), 
              GA_state =  sum(GA_state), 
              SF_state =  sum(SF_state), 
              SA_state =  sum(SA_state), 
              FF_state =  sum(FF_state), 
              FA_state =  sum(FA_state), 
              CF_state =  sum(CF_state), 
              CA_state =  sum(CA_state), 
              xGF_state = sum(xGF_state), 
              xGA_state = sum(xGA_state)
              ) %>% 
    filter(!is.na(player)) %>% 
    data.frame()
  
  return(merge_return)

  }

# Teams: On Ice Corsi For / Against, TOI - per game
fun.oniceTeamCorsiH_PP <- function(data) {
  
  hold <- data %>% 
    filter(game_strength_state %in% c("5v4", "5v3", "4v3")) %>% 
    summarise(TOI = sum(event_length) / 60, 
              Team = first(home_team), 
              
              GF = sum((event_type == "GOAL" & event_team == home_team) * score_adj_PP$home_goal_adj[home_lead_state]) , 
              GA = sum(event_type == "GOAL" & event_team == away_team),
              
              SF = sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_PP$home_shot_adj[home_lead_state]), 
              SA = sum(event_type %in% st.shot_events & event_team == away_team), 
              FF = sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_PP$home_fenwick_adj[home_lead_state]), 
              FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
              CF = sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_PP$home_corsi_adj[home_lead_state]), 
              CA = sum(event_type %in% st.corsi_events & event_team == away_team),
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_PP$home_xG_adj[home_lead_state])),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              
              GF_state = sum((event_type == "GOAL" & event_team == home_team) * score_adj_PP$home_goal_adj[home_lead_state] * state_adj_PP$Goals[str_state]), 
              GA_state = sum(event_type == "GOAL" & event_team == away_team),
              SF_state = sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_PP$home_shot_adj[home_lead_state] * state_adj_PP$Shots[str_state]), 
              SA_state = sum(event_type %in% st.shot_events & event_team == away_team), 
              
              FF_state = sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_PP$home_fenwick_adj[home_lead_state] * state_adj_PP$Fenwick[str_state]), 
              FA_state = sum(event_type %in% st.fenwick_events & event_team == away_team),
              CF_state = sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_PP$home_corsi_adj[home_lead_state] * state_adj_PP$Corsi[str_state]), 
              CA_state = sum(event_type %in% st.corsi_events & event_team == away_team),
              
              xGF_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_PP$home_xG_adj[home_lead_state] * state_adj_PP$xG[str_state])),
              xGA_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal))
              )
  
  return(hold)

  }
fun.oniceTeamCorsiA_PP <- function(data) {
  
  hold <- data %>% 
    filter(game_strength_state %in% c("4v5", "3v5", "3v4")) %>% 
    summarise(TOI = sum(event_length) / 60, 
              Team = first(away_team), 
              
              GF = sum((event_type == "GOAL" & event_team == away_team) * score_adj_PP$away_goal_adj[home_lead_state]) , 
              GA = sum(event_type == "GOAL" & event_team == home_team),
              
              SF = sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_PP$away_shot_adj[home_lead_state]), 
              SA = sum(event_type %in% st.shot_events & event_team == home_team), 
              FF = sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_PP$away_fenwick_adj[home_lead_state]), 
              FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
              CF = sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_PP$away_corsi_adj[home_lead_state]), 
              CA = sum(event_type %in% st.corsi_events & event_team == home_team),
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_PP$away_xG_adj[home_lead_state])),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              
              GF_state = sum((event_type == "GOAL" & event_team == away_team) * score_adj_PP$away_goal_adj[home_lead_state] * state_adj_PP$Goals[str_state]), 
              GA_state = sum(event_type == "GOAL" & event_team == home_team),
              SF_state = sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_PP$away_shot_adj[home_lead_state] * state_adj_PP$Shots[str_state]), 
              SA_state = sum(event_type %in% st.shot_events & event_team == home_team), 
              
              FF_state = sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_PP$away_fenwick_adj[home_lead_state] * state_adj_PP$Fenwick[str_state]), 
              FA_state = sum(event_type %in% st.fenwick_events & event_team == home_team),
              CF_state = sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_PP$away_corsi_adj[home_lead_state] * state_adj_PP$Corsi[str_state]), 
              CA_state = sum(event_type %in% st.corsi_events & event_team == home_team),
              
              xGF_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_PP$away_xG_adj[home_lead_state] * state_adj_PP$xG[str_state])),
              xGA_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal))
              )
  
  return(hold)

  }
fun.componiceCorsiTeam_PP <- function(data) {
  
  print("team_on_ice_home", quote = F)
  h_df <- data %>% 
    group_by(game_id, season, home_team) %>% 
    fun.oniceTeamCorsiH_PP(.) 
  
  print("team_on_ice_away", quote = F)
  a_df <- data %>% 
    group_by(game_id, season, away_team) %>% 
    fun.oniceTeamCorsiA_PP(.) 
  
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h_df, a_df))
  
  merge_return <- merged %>% 
    group_by(Team, game_id, season) %>% 
    summarise_at(vars(TOI, 
                      GF, GA, SF, SA, FF, FA, CF, CA, xGF, xGA, 
                      GF_state, GA_state, SF_state, SA_state, FF_state, FA_state, CF_state, CA_state, xGF_state, xGA_state), 
                 funs(sum)
                 ) %>% 
    data.frame()
  
  return(merge_return)

  }

# Merge skaters and team on-ice metrics
fun.shotmetrics_PP <- function(data) {
  
  # Run functions
  skater <- fun.componiceCorsi_PP(data)
  team <- fun.componiceCorsiTeam_PP(data)
  
  joined <- left_join(skater, team, by = c("game_id", "Team", "season"))
  
  names(joined) <- c("player", "game_id", "game_date", "season", "Team", "Opponent", "is_home", 
                     
                     "TOI", 
                     "onGF", "onGA", "onSF", "onSA", "onFF", "onFA", "onCF", "onCA", "onxGF", "onxGA",
                     "onGF_state", "onGA_state", "onSF_state", "onSA_state", "onFF_state", "onFA_state", "onCF_state", "onCA_state", "onxGF_state", "onxGA_state",
                     
                     "t_TOI", 
                     "t_GF", "t_GA", "t_SF", "t_SA", "t_FF", "t_FA", "t_CF", "t_CA", "t_xGF", "t_xGA", 
                     "t_GF_state", "t_GA_state", "t_SF_state", "t_SA_state", "t_FF_state", "t_FA_state", "t_CF_state", "t_CA_state", "t_xGF_state", "t_xGA_state"
                     )
  
  # Remove goalies
  fun.goalie_remove <- function(data_) {
    
    # Identifies goalies within a given pbp data.frame & returns a data.frame to join for removal
    goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data_$home_goalie, data_$away_goalie))))), 
                                is_goalie = 1)
    
    goalie_return$player <- as.character(goalie_return$player)
    
    return(goalie_return)
  
    }
  goalieremove <- fun.goalie_remove(data_ = data)
  
  all <- joined %>% 
    left_join(., goalieremove, "player") %>% 
    filter(is.na(is_goalie)) %>% 
    select(-c(is_goalie)) %>% 
    arrange(player, game_id) %>% 
    data.frame()
  
  return(all)

  }

# Zone Starts
fun.ZS_H_PP <- function(data, venue) {
  
  hold <- data %>% 
    filter(game_strength_state %in% c("5v4", "5v3", "4v3"), 
           event_type == "FAC"
           ) %>%  
    summarise(Team = first(home_team),
              OZS = sum(event_type == "FAC" & home_zone == "Off"), 
              NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
              DZS = sum(event_type == "FAC" & home_zone == "Def")
              )
  
  return(hold)

  }
fun.ZS_A_PP <- function(data, venue) {
  
  hold <- data %>% 
    filter(game_strength_state %in% c("4v5", "3v5", "3v4"), 
           event_type == "FAC"
           ) %>%  
    summarise(Team = first(away_team),
              OZS = sum(event_type == "FAC" & home_zone == "Def"), 
              NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
              DZS = sum(event_type == "FAC" & home_zone == "Off")
              )
  
  return(hold)

  }
fun.ZS_compute_PP <- function(data) {
  
  print("zone_starts_home", quote = F)
  h1 <- data %>% group_by(game_id, season, home_on_1) %>% fun.ZS_H_PP(., "home_on_1") %>% rename(player = home_on_1) %>% data.frame()
  h2 <- data %>% group_by(game_id, season, home_on_2) %>% fun.ZS_H_PP(., "home_on_2") %>% rename(player = home_on_2) %>% data.frame()
  h3 <- data %>% group_by(game_id, season, home_on_3) %>% fun.ZS_H_PP(., "home_on_3") %>% rename(player = home_on_3) %>% data.frame()
  h4 <- data %>% group_by(game_id, season, home_on_4) %>% fun.ZS_H_PP(., "home_on_4") %>% rename(player = home_on_4) %>% data.frame()
  h5 <- data %>% group_by(game_id, season, home_on_5) %>% fun.ZS_H_PP(., "home_on_5") %>% rename(player = home_on_5) %>% data.frame()
  h6 <- data %>% group_by(game_id, season, home_on_6) %>% fun.ZS_H_PP(., "home_on_6") %>% rename(player = home_on_6) %>% data.frame()
  
  print("zone_starts_away", quote = F)
  a1 <- data %>% group_by(game_id, season, away_on_1) %>% fun.ZS_A_PP(., "away_on_1") %>% rename(player = away_on_1) %>% data.frame()
  a2 <- data %>% group_by(game_id, season, away_on_2) %>% fun.ZS_A_PP(., "away_on_2") %>% rename(player = away_on_2) %>% data.frame()
  a3 <- data %>% group_by(game_id, season, away_on_3) %>% fun.ZS_A_PP(., "away_on_3") %>% rename(player = away_on_3) %>% data.frame()
  a4 <- data %>% group_by(game_id, season, away_on_4) %>% fun.ZS_A_PP(., "away_on_4") %>% rename(player = away_on_4) %>% data.frame()
  a5 <- data %>% group_by(game_id, season, away_on_5) %>% fun.ZS_A_PP(., "away_on_5") %>% rename(player = away_on_5) %>% data.frame()
  a6 <- data %>% group_by(game_id, season, away_on_6) %>% fun.ZS_A_PP(., "away_on_6") %>% rename(player = away_on_6) %>% data.frame()
  
  # Join
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
  
  summed <- merged %>% 
    group_by(player, game_id, season) %>%  
    summarise(Team = first(Team),
              OZS =  sum(OZS), 
              NZS =  sum(NZS), 
              DZS =  sum(DZS)
              ) %>% 
    arrange(player)
  
  return(summed)

  }

# Boxscore
fun.counts_PP <- function(data, venue) {
  
  if (venue == "home_team") {
    
    hold <- data %>% 
      filter(game_strength_state %in% c("5v4", "5v3", "4v3"), 
             event_team == home_team)
    
    # Counts
    counts_1 <- hold %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE")) %>% 
      summarise(Team = first(home_team),
                
                G = sum(event_type == "GOAL"),
                G_adj = sum((event_type == "GOAL") * score_adj_PP$home_goal_adj[home_lead_state]),
                iSF = sum(event_type %in% st.shot_events),
                iCF = sum(event_type %in% st.corsi_events), 
                iCF_adj = sum((event_type %in% st.corsi_events) * score_adj_PP$home_corsi_adj[home_lead_state]), 
                iFF = sum(event_type %in% st.fenwick_events),
                ixG = sum(na.omit(pred_goal)), 
                ixG_adj = sum(na.omit(pred_goal * score_adj_PP$home_xG_adj[home_lead_state])), 
                
                GIVE_o = sum(event_type == "GIVE" & event_zone == "Off"), 
                GIVE_n = sum(event_type == "GIVE" & event_zone == "Neu"), 
                GIVE_d = sum(event_type == "GIVE" & event_zone == "Def"), 
                
                TAKE_o = sum(event_type == "TAKE" & event_zone == "Off"), 
                TAKE_n = sum(event_type == "TAKE" & event_zone == "Neu"), 
                TAKE_d = sum(event_type == "TAKE" & event_zone == "Def"), 
                
                GIVE_adj = sum((event_type == "GIVE") * score_adj_PP$home_GIVE_adj[home_lead_state]),
                TAKE_adj = sum((event_type == "TAKE") * score_adj_PP$home_TAKE_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    counts_2 <- hold %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(event_type %in% c("GOAL")) %>% 
      summarise(Team = first(home_team), 
                A1 = sum(event_type == "GOAL"), 
                A1_adj = sum((event_type == "GOAL") * score_adj_PP$home_A1_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    counts_3 <- hold %>% 
      group_by(event_player_3, game_id, season) %>%
      filter(event_type == "GOAL") %>% 
      summarise(Team = first(home_team), 
                A2 = sum(event_type == "GOAL"), 
                A2_adj = sum((event_type == "GOAL") * score_adj_PP$home_A2_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_3) %>% 
      data.frame()
    
    # Join
    merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      mutate(Points = G + A1 + A2, 
             Points_adj = G_adj + A1_adj + A2_adj
             ) %>% 
      select(player, game_id, season, Team, 
             G, A1, A2, Points, 
             G_adj, A1_adj, A2_adj, Points_adj,
             iSF, iCF, iFF, ixG, iCF_adj, ixG_adj, 
             GIVE_o, GIVE_n, GIVE_d, 
             TAKE_o, TAKE_n, TAKE_d,
             GIVE_adj, TAKE_adj
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
  
    }
  else {
    
    hold <- data %>% 
      filter(game_strength_state %in% c("4v5", "3v5", "3v4"),  
             event_team == away_team)
    
    # Compile
    counts_1 <- hold %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE")) %>% 
      summarise(Team = first(away_team),
                
                G = sum(event_type == "GOAL"),
                G_adj = sum((event_type == "GOAL") * score_adj_PP$away_goal_adj[home_lead_state]),
                iSF = sum(event_type %in% st.shot_events),
                iCF = sum(event_type %in% st.corsi_events),
                iCF_adj = sum((event_type %in% st.corsi_events) * score_adj_PP$away_corsi_adj[home_lead_state]), 
                iFF = sum(event_type %in% st.fenwick_events), 
                ixG = sum(na.omit(pred_goal)), 
                ixG_adj = sum(na.omit(pred_goal * score_adj_PP$away_xG_adj[home_lead_state])), 
                
                GIVE_o = sum(event_type == "GIVE" & event_zone == "Off"), 
                GIVE_n = sum(event_type == "GIVE" & event_zone == "Neu"), 
                GIVE_d = sum(event_type == "GIVE" & event_zone == "Def"), 
                
                TAKE_o = sum(event_type == "TAKE" & event_zone == "Off"), 
                TAKE_n = sum(event_type == "TAKE" & event_zone == "Neu"), 
                TAKE_d = sum(event_type == "TAKE" & event_zone == "Def"), 
                
                GIVE_adj = sum((event_type == "GIVE") * score_adj_PP$away_GIVE_adj[home_lead_state]), 
                TAKE_adj = sum((event_type == "TAKE") * score_adj_PP$away_TAKE_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    counts_2 <- hold %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(event_type %in% c("GOAL")) %>% 
      summarise(Team = first(away_team), 
                A1 = sum(event_type == "GOAL"), 
                A1_adj = sum((event_type == "GOAL") * score_adj_PP$away_A1_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    counts_3 <- hold %>% 
      group_by(event_player_3, game_id, season) %>%
      filter(event_type == "GOAL") %>% 
      summarise(Team = first(away_team), 
                A2 = sum(event_type == "GOAL"), 
                A2_adj = sum((event_type == "GOAL") * score_adj_PP$away_A2_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_3) %>% 
      data.frame()
    
    # Join
    merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      mutate(Points = G + A1 + A2, 
             Points_adj = G_adj + A1_adj + A2_adj
             ) %>% 
      select(player, game_id, season, Team, 
             G, A1, A2, Points, 
             G_adj, A1_adj, A2_adj, Points_adj,
             iSF, iCF, iFF, ixG, iCF_adj, ixG_adj, 
             GIVE_o, GIVE_n, GIVE_d, 
             TAKE_o, TAKE_n, TAKE_d,
             GIVE_adj, TAKE_adj
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
  
    }

  }
fun.faceoff_PP <- function(data, venue) {
  
  if (venue == "home_team") {
    
    faceoffs <- data %>% 
      filter(game_strength_state %in% c("5v4", "5v3", "4v3"), 
             event_type == "FAC"
             ) %>% 
      group_by(event_player_2, game_id, season) %>% 
      mutate(FOW = 1 * (home_team == event_team), 
             FOL = 1 * (away_team == event_team)
             ) %>% 
      summarise(Team = first(home_team),
                FOW = sum(FOW), 
                FOL = sum(FOL)
                ) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      rename(player = event_player_2) %>% 
      ungroup() %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(faceoffs)
  
    }
  else {
    
    faceoffs <- data %>% 
      filter(event_type == "FAC", 
             game_strength_state %in% c("4v5", "3v5", "3v4")
             ) %>% 
      group_by(event_player_1, game_id, season) %>% 
      mutate(FOW = 1 * (away_team == event_team), 
             FOL = 1 * (home_team == event_team)
             ) %>% 
      summarise(Team = first(away_team),
                FOW = sum(FOW), 
                FOL = sum(FOL)
                ) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      rename(player = event_player_1) %>% 
      ungroup() %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(faceoffs)
  
    }

  }
fun.penalty_PP <- function(data, venue) {
  
  if (venue == "home_team") {
    
    pen_1 <- data %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(game_strength_state %in% c("5v4", "5v3", "4v3"),  
             event_team == home_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(home_team),
                iPENT2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHF_o = sum(event_type == "HIT" & event_zone == "Off"), 
                iHF_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHF_d = sum(event_type == "HIT" & event_zone == "Def"), 
                
                iHF_adj = sum((event_type == "HIT") * score_adj_PP$home_HF_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    
    pen_2 <- data %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(game_strength_state %in% c("5v4", "5v3", "4v3"),  
             event_team == away_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(home_team), 
                iPEND2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHA_o = sum(event_type == "HIT" & event_zone == "Def"), 
                iHA_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHA_d = sum(event_type == "HIT" & event_zone == "Off"), 
                
                iHA_adj = sum((event_type == "HIT") * score_adj_PP$home_HA_adj[home_lead_state]) 
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    
    merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      select(player, game_id, season, Team, 
             iPENT2, iPEND2, iPENT5, iPEND5,
             iHF_o, iHF_n, iHF_d, 
             iHA_o, iHA_n, iHA_d, 
             iHF_adj, iHA_adj
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
  
    }
  else {
    
    pen_1 <- data %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(game_strength_state %in% c("4v5", "3v5", "3v4"),  
             event_team == away_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(away_team),
                iPENT2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHF_o = sum(event_type == "HIT" & event_zone == "Off"), 
                iHF_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHF_d = sum(event_type == "HIT" & event_zone == "Def"), 
                
                iHF_adj = sum((event_type == "HIT") * score_adj_PP$away_HF_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    pen_2 <- data %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(game_strength_state %in% c("4v5", "3v5", "3v4"), 
             event_team == home_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(away_team),
                iPEND2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHA_o = sum(event_type == "HIT" & event_zone == "Def"), 
                iHA_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHA_d = sum(event_type == "HIT" & event_zone == "Off"), 
                
                iHA_adj = sum((event_type == "HIT") * score_adj_PP$away_HA_adj[home_lead_state]) 
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    
    merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      select(player, game_id, season, Team, 
             iPENT2, iPEND2, iPENT5, iPEND5,
             iHF_o, iHF_n, iHF_d, 
             iHA_o, iHA_n, iHA_d, 
             iHF_adj, iHA_adj
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
  
    }

  }

###  Run all functions
fun.combine_counts_PP <- function(data) { 
  
  print(paste("season:", unique(data$season)), quote = F)
  
  # Filter pbp / join prior face time for corsi events
  data <- data %>% 
    filter(game_strength_state %in% st.pp_strength, 
           game_period < 5
           ) %>% 
    select(-c(face_index:shift_length)) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, 
                              ifelse(scradj <= -3, -3, scradj)),
           home_lead_state = ifelse(home_lead < 0, 1, 
                                    ifelse(home_lead == 0, 2, 
                                           ifelse(home_lead > 0, 3, home_lead))), 
           str_state = ifelse(game_strength_state %in% c("5v4", "4v5"), 1, 
                              ifelse(game_strength_state %in% c("5v3", "3v5"), 2, 
                                     ifelse(game_strength_state %in% c("4v3", "3v4"), 3, NA))), 
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           ) %>% 
    rename(pred_goal = pred_XGB_7)
  
  
  ### Run functions
  shot_metrics <- fun.shotmetrics_PP(data)
  zone_start <- fun.ZS_compute_PP(data)
  
  print("counts", quote = F)
  counts_all <- fun.counts_PP(data, "home_team") %>% 
    rbind(., fun.counts_PP(data, "away_team")) %>% 
    arrange(player, game_id)
  
  print("faceoffs", quote = F)
  faceoff_all <- fun.faceoff_PP(data, "home_team") %>% 
    rbind(., fun.faceoff_PP(data, "away_team")) %>% 
    arrange(player, game_id)
  
  print("penalties", quote = F)
  penalty_all <- fun.penalty_PP(data, "home_team") %>% 
    rbind(., fun.penalty_PP(data, "away_team")) %>% 
    arrange(player, game_id)
  
  
  # Join
  print("combine", quote = F)
  metrics_combined <- shot_metrics %>% 
    left_join(., zone_start,  by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., counts_all,  by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., faceoff_all, by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., penalty_all, by = c("player", "game_id", "season", "Team")) %>% 
    select(player, game_id, game_date, season, Team, Opponent, is_home, 
           TOI, 
           G, A1, A2, Points, 
           G_adj, A1_adj, A2_adj, Points_adj, 
           iSF, iFF, iCF, ixG, iCF_adj, ixG_adj, 
           GIVE_o:TAKE_d, GIVE_adj, TAKE_adj, 
           iHF_o:iHA_d, iHF_adj, iHA_adj, 
           FOW, FOL, 
           OZS, NZS, DZS, 
           iPENT2, iPEND2, iPENT5, iPEND5, 
           onGF:onxGA_state, 
           t_TOI:t_xGA_state
           ) %>% 
    # Patch for if no PP goals were scored
    mutate(A1 = ifelse(is.na(A1), 0, A1), 
           A2 = ifelse(is.na(A2), 0, A2)
           ) %>% 
    arrange(player, game_id) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
    mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
    data.frame()
  
  return(metrics_combined)
  
  }


#########################


## --------------------- ##
##   Shorthanded Games   ##
## --------------------- ##

###########################

# Skaters: On Ice Corsi For / Against, TOI, Team - per game
fun.oniceCorsiH_SH <- function(data, venue) {
  
  hold <- data %>% 
    filter(game_strength_state %in% c("4v5", "3v5", "3v4")) %>% 
    summarise(TOI = sum(event_length) / 60, 
              
              Team = first(home_team), 
              Opponent = first(away_team), 
              is_home = 1, 
              
              GF = sum(event_type == "GOAL" & event_team == home_team),
              GA = sum((event_type == "GOAL" & event_team == away_team) * score_adj_PP$away_goal_adj[home_lead_state]) , 
              
              SF = sum(event_type %in% st.shot_events & event_team == home_team), 
              SA = sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_PP$away_shot_adj[home_lead_state]), 
              FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
              FA = sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_PP$away_fenwick_adj[home_lead_state]), 
              CF = sum(event_type %in% st.corsi_events & event_team == home_team),
              CA = sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_PP$away_corsi_adj[home_lead_state]), 
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_PP$away_xG_adj[home_lead_state])),
              
              GF_state = sum(event_type == "GOAL" & event_team == home_team),
              GA_state = sum((event_type == "GOAL" & event_team == away_team) * score_adj_PP$away_goal_adj[home_lead_state] * state_adj_PP$Goals[str_state]), 
              SF_state = sum(event_type %in% st.shot_events & event_team == home_team), 
              SA_state = sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_PP$away_shot_adj[home_lead_state] * state_adj_PP$Shots[str_state]), 
              
              FF_state = sum(event_type %in% st.fenwick_events & event_team == home_team),
              FA_state = sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_PP$away_fenwick_adj[home_lead_state] * state_adj_PP$Fenwick[str_state]), 
              CF_state = sum(event_type %in% st.corsi_events & event_team == home_team),
              CA_state = sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_PP$away_corsi_adj[home_lead_state] * state_adj_PP$Corsi[str_state]), 
              
              xGF_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)),
              xGA_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_PP$away_xG_adj[home_lead_state] * state_adj_PP$xG[str_state]))
              )
  
  return(hold)

  }
fun.oniceCorsiA_SH <- function(data, venue) {
  
  hold <- data %>% 
    filter(game_strength_state %in% c("5v4", "5v3", "4v3")) %>% 
    summarise(TOI = sum(event_length) / 60, 
              
              Team = first(away_team), 
              Opponent = first(home_team), 
              is_home = 0, 
              
              GF = sum(event_type == "GOAL" & event_team == away_team),
              GA = sum((event_type == "GOAL" & event_team == home_team) * score_adj_PP$home_goal_adj[home_lead_state]) , 
              
              SF = sum(event_type %in% st.shot_events & event_team == away_team), 
              SA = sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_PP$home_shot_adj[home_lead_state]), 
              FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
              FA = sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_PP$home_fenwick_adj[home_lead_state]), 
              CF = sum(event_type %in% st.corsi_events & event_team == away_team),
              CA = sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_PP$home_corsi_adj[home_lead_state]), 
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_PP$home_xG_adj[home_lead_state])),
              
              GF_state = sum(event_type == "GOAL" & event_team == away_team),
              GA_state = sum((event_type == "GOAL" & event_team == home_team) * score_adj_PP$home_goal_adj[home_lead_state] * state_adj_PP$Goals[str_state]), 
              SF_state = sum(event_type %in% st.shot_events & event_team == away_team), 
              SA_state = sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_PP$home_shot_adj[home_lead_state] * state_adj_PP$Shots[str_state]), 
              
              FF_state = sum(event_type %in% st.fenwick_events & event_team == away_team),
              FA_state = sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_PP$home_fenwick_adj[home_lead_state] * state_adj_PP$Fenwick[str_state]), 
              CF_state = sum(event_type %in% st.corsi_events & event_team == away_team),
              CA_state = sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_PP$home_corsi_adj[home_lead_state] * state_adj_PP$Corsi[str_state]), 
              
              xGF_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)),
              xGA_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_PP$home_xG_adj[home_lead_state] * state_adj_PP$xG[str_state]))
              )
  
  return(hold)

  }
fun.componiceCorsi_SH <- function(data) {
  
  print("on_ice_home", quote = F)
  h1 <- data %>% group_by(game_id, game_date, season, home_on_1, home_team) %>% fun.oniceCorsiH_SH(., "home_on_1") %>% rename(player = home_on_1) %>% data.frame()
  h2 <- data %>% group_by(game_id, game_date, season, home_on_2, home_team) %>% fun.oniceCorsiH_SH(., "home_on_2") %>% rename(player = home_on_2) %>% data.frame()
  h3 <- data %>% group_by(game_id, game_date, season, home_on_3, home_team) %>% fun.oniceCorsiH_SH(., "home_on_3") %>% rename(player = home_on_3) %>% data.frame()
  h4 <- data %>% group_by(game_id, game_date, season, home_on_4, home_team) %>% fun.oniceCorsiH_SH(., "home_on_4") %>% rename(player = home_on_4) %>% data.frame()
  h5 <- data %>% group_by(game_id, game_date, season, home_on_5, home_team) %>% fun.oniceCorsiH_SH(., "home_on_5") %>% rename(player = home_on_5) %>% data.frame()
  h6 <- data %>% group_by(game_id, game_date, season, home_on_6, home_team) %>% fun.oniceCorsiH_SH(., "home_on_6") %>% rename(player = home_on_6) %>% data.frame()
  
  print("on_ice_away", quote = F)
  a1 <- data %>% group_by(game_id, game_date, season, away_on_1, away_team) %>% fun.oniceCorsiA_SH(., "away_on_1") %>% rename(player = away_on_1) %>% data.frame()
  a2 <- data %>% group_by(game_id, game_date, season, away_on_2, away_team) %>% fun.oniceCorsiA_SH(., "away_on_2") %>% rename(player = away_on_2) %>% data.frame()
  a3 <- data %>% group_by(game_id, game_date, season, away_on_3, away_team) %>% fun.oniceCorsiA_SH(., "away_on_3") %>% rename(player = away_on_3) %>% data.frame()
  a4 <- data %>% group_by(game_id, game_date, season, away_on_4, away_team) %>% fun.oniceCorsiA_SH(., "away_on_4") %>% rename(player = away_on_4) %>% data.frame()
  a5 <- data %>% group_by(game_id, game_date, season, away_on_5, away_team) %>% fun.oniceCorsiA_SH(., "away_on_5") %>% rename(player = away_on_5) %>% data.frame()
  a6 <- data %>% group_by(game_id, game_date, season, away_on_6, away_team) %>% fun.oniceCorsiA_SH(., "away_on_6") %>% rename(player = away_on_6) %>% data.frame()
  
  
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
  
  merge_return <- merged %>% 
    group_by(player, game_id, game_date, season) %>%  
    summarise(Team =     first(Team), 
              Opponent = first(Opponent), 
              is_home =  first(is_home), 
              
              TOI =      sum(TOI), 
              GF =       sum(GF), 
              GA =       sum(GA), 
              SF =       sum(SF), 
              SA =       sum(SA), 
              FF =       sum(FF), 
              FA =       sum(FA), 
              CF =       sum(CF), 
              CA =       sum(CA), 
              xGF =      sum(xGF), 
              xGA =      sum(xGA), 
              
              GF_state =  sum(GF_state), 
              GA_state =  sum(GA_state), 
              SF_state =  sum(SF_state), 
              SA_state =  sum(SA_state), 
              FF_state =  sum(FF_state), 
              FA_state =  sum(FA_state), 
              CF_state =  sum(CF_state), 
              CA_state =  sum(CA_state), 
              xGF_state = sum(xGF_state), 
              xGA_state = sum(xGA_state)
              ) %>% 
    filter(!is.na(player)) %>% 
    data.frame()
  
  return(merge_return)

  }

# Teams: On Ice Corsi For / Against, TOI - per game
fun.oniceTeamCorsiH_SH <- function(data) {
  
  hold <- data %>% 
    filter(game_strength_state %in% c("4v5", "3v5", "3v4")) %>% 
    summarise(TOI = sum(event_length) / 60, 
              Team = first(home_team), 
              
              GF = sum(event_type == "GOAL" & event_team == home_team),
              GA = sum((event_type == "GOAL" & event_team == away_team) * score_adj_PP$away_goal_adj[home_lead_state]) , 
              
              SF = sum(event_type %in% st.shot_events & event_team == home_team), 
              SA = sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_PP$away_shot_adj[home_lead_state]), 
              FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
              FA = sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_PP$away_fenwick_adj[home_lead_state]), 
              CF = sum(event_type %in% st.corsi_events & event_team == home_team),
              CA = sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_PP$away_corsi_adj[home_lead_state]), 
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_PP$away_xG_adj[home_lead_state])),
              
              GF_state = sum(event_type == "GOAL" & event_team == home_team),
              GA_state = sum((event_type == "GOAL" & event_team == away_team) * score_adj_PP$away_goal_adj[home_lead_state] * state_adj_PP$Goals[str_state]), 
              SF_state = sum(event_type %in% st.shot_events & event_team == home_team), 
              SA_state = sum((event_type %in% st.shot_events & event_team == away_team) * score_adj_PP$away_shot_adj[home_lead_state] * state_adj_PP$Shots[str_state]), 
              
              FF_state = sum(event_type %in% st.fenwick_events & event_team == home_team),
              FA_state = sum((event_type %in% st.fenwick_events & event_team == away_team) * score_adj_PP$away_fenwick_adj[home_lead_state] * state_adj_PP$Fenwick[str_state]), 
              CF_state = sum(event_type %in% st.corsi_events & event_team == home_team),
              CA_state = sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_PP$away_corsi_adj[home_lead_state] * state_adj_PP$Corsi[str_state]), 
              
              xGF_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)),
              xGA_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * score_adj_PP$away_xG_adj[home_lead_state] * state_adj_PP$xG[str_state]))
              )
  
  return(hold)

  }
fun.oniceTeamCorsiA_SH <- function(data) {
  
  hold <- data %>% 
    filter(game_strength_state %in% c("5v4", "5v3", "4v3")) %>% 
    summarise(TOI = sum(event_length) / 60, 
              Team = first(away_team), 
              
              GF = sum(event_type == "GOAL" & event_team == away_team),
              GA = sum((event_type == "GOAL" & event_team == home_team) * score_adj_PP$home_goal_adj[home_lead_state]) , 
              
              SF = sum(event_type %in% st.shot_events & event_team == away_team), 
              SA = sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_PP$home_shot_adj[home_lead_state]), 
              FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
              FA = sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_PP$home_fenwick_adj[home_lead_state]), 
              CF = sum(event_type %in% st.corsi_events & event_team == away_team),
              CA = sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_PP$home_corsi_adj[home_lead_state]), 
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_PP$home_xG_adj[home_lead_state])),
              
              GF_state = sum(event_type == "GOAL" & event_team == away_team),
              GA_state = sum((event_type == "GOAL" & event_team == home_team) * score_adj_PP$home_goal_adj[home_lead_state] * state_adj_PP$Goals[str_state]), 
              SF_state = sum(event_type %in% st.shot_events & event_team == away_team), 
              SA_state = sum((event_type %in% st.shot_events & event_team == home_team) * score_adj_PP$home_shot_adj[home_lead_state] * state_adj_PP$Shots[str_state]), 
              
              FF_state = sum(event_type %in% st.fenwick_events & event_team == away_team),
              FA_state = sum((event_type %in% st.fenwick_events & event_team == home_team) * score_adj_PP$home_fenwick_adj[home_lead_state] * state_adj_PP$Fenwick[str_state]), 
              CF_state = sum(event_type %in% st.corsi_events & event_team == away_team),
              CA_state = sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_PP$home_corsi_adj[home_lead_state] * state_adj_PP$Corsi[str_state]), 
              
              xGF_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)),
              xGA_state = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * score_adj_PP$home_xG_adj[home_lead_state] * state_adj_PP$xG[str_state]))
              )
  
  return(hold)

  }
fun.componiceCorsiTeam_SH <- function(data) {
  
  print("team_on_ice_home", quote = F)
  h_df <- data %>% 
    group_by(game_id, season, home_team) %>% 
    fun.oniceTeamCorsiH_SH(.) 
  
  print("team_on_ice_away", quote = F)
  a_df <- data %>% 
    group_by(game_id, season, away_team) %>% 
    fun.oniceTeamCorsiA_SH(.) 
  
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h_df, a_df))
  
  merge_return <- merged %>% 
    group_by(Team, game_id, season) %>% 
    summarise_at(vars(TOI, 
                      GF, GA, SF, SA, FF, FA, CF, CA, xGF, xGA, 
                      GF_state, GA_state, SF_state, SA_state, FF_state, FA_state, CF_state, CA_state, xGF_state, xGA_state), 
                 funs(sum)
                 ) %>% 
    data.frame()
  
  return(merge_return)

  }

# Merge skaters and team on-ice metrics
fun.shotmetrics_SH <- function(data) {
  
  # Run functions
  skater <- fun.componiceCorsi_SH(data)
  team <- fun.componiceCorsiTeam_SH(data)
  
  joined <- left_join(skater, team, by = c("game_id", "Team", "season"))
  
  names(joined) <- c("player", "game_id", "game_date", "season", "Team", "Opponent", "is_home", 
                     
                     "TOI", 
                     "onGF", "onGA", "onSF", "onSA", "onFF", "onFA", "onCF", "onCA", "onxGF", "onxGA",
                     "onGF_state", "onGA_state", "onSF_state", "onSA_state", "onFF_state", "onFA_state", "onCF_state", "onCA_state", "onxGF_state", "onxGA_state",
                     
                     "t_TOI", 
                     "t_GF", "t_GA", "t_SF", "t_SA", "t_FF", "t_FA", "t_CF", "t_CA", "t_xGF", "t_xGA", 
                     "t_GF_state", "t_GA_state", "t_SF_state", "t_SA_state", "t_FF_state", "t_FA_state", "t_CF_state", "t_CA_state", "t_xGF_state", "t_xGA_state"
                     )
  
  # Remove goalies
  fun.goalie_remove <- function(data_) {
    
    # Identifies goalies within a given pbp data.frame & returns a data.frame to join for removal
    goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data_$home_goalie, data_$away_goalie))))), 
                                is_goalie = 1)
    
    goalie_return$player <- as.character(goalie_return$player)
    
    return(goalie_return)
  
    }
  goalieremove <- fun.goalie_remove(data_ = data)
  
  all <- joined %>% 
    left_join(., goalieremove, "player") %>% 
    filter(is.na(is_goalie)) %>% 
    select(-c(is_goalie)) %>% 
    arrange(player, game_id) %>% 
    data.frame()
  
  return(all)

  }

# Zone Starts
fun.ZS_H_SH <- function(data, venue) {
  
  hold <- data %>% 
    filter(game_strength_state %in% c("4v5", "3v5", "3v4"), 
           event_type == "FAC"
           ) %>% 
    summarise(Team = first(home_team),
              OZS = sum(event_type == "FAC" & home_zone == "Off"), 
              NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
              DZS = sum(event_type == "FAC" & home_zone == "Def")
              )
  
  return(hold)
  
  }
fun.ZS_A_SH <- function(data, venue) {
  
  hold <- data %>% 
    filter(game_strength_state %in% c("5v4", "5v3", "4v3"), 
           event_type == "FAC"
           ) %>%  
    summarise(Team = first(away_team),
              OZS = sum(event_type == "FAC" & home_zone == "Def"), 
              NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
              DZS = sum(event_type == "FAC" & home_zone == "Off")
              )
  
  return(hold)
  
  }
fun.ZS_compute_SH <- function(data) {
  
  print("zone_starts_home", quote = F)
  h1 <- data %>% group_by(game_id, season, home_on_1) %>% fun.ZS_H_SH(., "home_on_1") %>% rename(player = home_on_1) %>% data.frame()
  h2 <- data %>% group_by(game_id, season, home_on_2) %>% fun.ZS_H_SH(., "home_on_2") %>% rename(player = home_on_2) %>% data.frame()
  h3 <- data %>% group_by(game_id, season, home_on_3) %>% fun.ZS_H_SH(., "home_on_3") %>% rename(player = home_on_3) %>% data.frame()
  h4 <- data %>% group_by(game_id, season, home_on_4) %>% fun.ZS_H_SH(., "home_on_4") %>% rename(player = home_on_4) %>% data.frame()
  h5 <- data %>% group_by(game_id, season, home_on_5) %>% fun.ZS_H_SH(., "home_on_5") %>% rename(player = home_on_5) %>% data.frame()
  h6 <- data %>% group_by(game_id, season, home_on_6) %>% fun.ZS_H_SH(., "home_on_6") %>% rename(player = home_on_6) %>% data.frame()
  
  print("zone_starts_away", quote = F)
  a1 <- data %>% group_by(game_id, season, away_on_1) %>% fun.ZS_A_SH(., "away_on_1") %>% rename(player = away_on_1) %>% data.frame()
  a2 <- data %>% group_by(game_id, season, away_on_2) %>% fun.ZS_A_SH(., "away_on_2") %>% rename(player = away_on_2) %>% data.frame()
  a3 <- data %>% group_by(game_id, season, away_on_3) %>% fun.ZS_A_SH(., "away_on_3") %>% rename(player = away_on_3) %>% data.frame()
  a4 <- data %>% group_by(game_id, season, away_on_4) %>% fun.ZS_A_SH(., "away_on_4") %>% rename(player = away_on_4) %>% data.frame()
  a5 <- data %>% group_by(game_id, season, away_on_5) %>% fun.ZS_A_SH(., "away_on_5") %>% rename(player = away_on_5) %>% data.frame()
  a6 <- data %>% group_by(game_id, season, away_on_6) %>% fun.ZS_A_SH(., "away_on_6") %>% rename(player = away_on_6) %>% data.frame()
  
  # Join
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
  
  summed <- merged %>% 
    group_by(player, game_id, season) %>%  
    summarise(Team = first(Team),
              OZS =  sum(OZS), 
              NZS =  sum(NZS), 
              DZS =  sum(DZS)
              ) %>% 
    arrange(player)
  
  return(summed)
  
  }

# Boxscore
fun.counts_SH <- function(data, venue) {
  
  if (venue == "home_team") {
    
    hold <- data %>% 
      filter(game_strength_state %in% c("4v5", "3v5", "3v4"),  
             event_team == home_team)
    
    # Counts
    counts_1 <- hold %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE")) %>% 
      summarise(Team = first(home_team),
                G = sum(event_type == "GOAL"),
                iSF = sum(event_type %in% st.shot_events),
                iCF = sum(event_type %in% st.corsi_events), 
                iFF = sum(event_type %in% st.fenwick_events),
                ixG = sum(na.omit(pred_goal)), 
                
                GIVE_o = sum(event_type == "GIVE" & event_zone == "Off"), 
                GIVE_n = sum(event_type == "GIVE" & event_zone == "Neu"), 
                GIVE_d = sum(event_type == "GIVE" & event_zone == "Def"), 
                
                TAKE_o = sum(event_type == "TAKE" & event_zone == "Off"), 
                TAKE_n = sum(event_type == "TAKE" & event_zone == "Neu"), 
                TAKE_d = sum(event_type == "TAKE" & event_zone == "Def"), 
                
                GIVE_adj = sum((event_type == "GIVE") * score_adj_SH$home_GIVE_adj[home_lead_state]),
                TAKE_adj = sum((event_type == "TAKE") * score_adj_SH$home_TAKE_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    counts_2 <- hold %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(event_type %in% c("GOAL")) %>% 
      summarise(Team = first(home_team), 
                A1 = sum(event_type == "GOAL") 
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    counts_3 <- hold %>% 
      group_by(event_player_3, game_id, season) %>%
      filter(event_type == "GOAL") %>% 
      summarise(Team = first(home_team), 
                A2 = sum(event_type == "GOAL") 
                ) %>% 
      rename(player = event_player_3) %>% 
      data.frame()
    
    # Join
    merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate(Points = G + A1 + A2) %>% 
      select(player, game_id, season, Team, 
             G, A1, A2, Points, 
             iSF, iCF, iFF, ixG, 
             GIVE_o, GIVE_n, GIVE_d, 
             TAKE_o, TAKE_n, TAKE_d,
             GIVE_adj, TAKE_adj
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
  
    }
  else {
    
    hold <- data %>% 
      filter(game_strength_state %in% c("5v4", "5v3", "4v3"),  
             event_team == away_team)
    
    # Compile
    counts_1 <- hold %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE")) %>% 
      summarise(Team = first(away_team),
                G = sum(event_type == "GOAL"),
                iSF = sum(event_type %in% st.shot_events),
                iCF = sum(event_type %in% st.corsi_events),
                iFF = sum(event_type %in% st.fenwick_events), 
                ixG = sum(na.omit(pred_goal)), 
                
                GIVE_o = sum(event_type == "GIVE" & event_zone == "Off"), 
                GIVE_n = sum(event_type == "GIVE" & event_zone == "Neu"), 
                GIVE_d = sum(event_type == "GIVE" & event_zone == "Def"), 
                
                TAKE_o = sum(event_type == "TAKE" & event_zone == "Off"), 
                TAKE_n = sum(event_type == "TAKE" & event_zone == "Neu"), 
                TAKE_d = sum(event_type == "TAKE" & event_zone == "Def"), 
                
                GIVE_adj = sum((event_type == "GIVE") * score_adj_SH$away_GIVE_adj[home_lead_state]), 
                TAKE_adj = sum((event_type == "TAKE") * score_adj_SH$away_TAKE_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    counts_2 <- hold %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(event_type %in% c("GOAL")) %>% 
      summarise(Team = first(away_team), 
                A1 = sum(event_type == "GOAL")
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    counts_3 <- hold %>% 
      group_by(event_player_3, game_id, season) %>%
      filter(event_type == "GOAL") %>% 
      summarise(Team = first(away_team), 
                A2 = sum(event_type == "GOAL")
                ) %>% 
      rename(player = event_player_3) %>% 
      data.frame()
    
    # Join
    merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate(Points = G + A1 + A2) %>% 
      select(player, game_id, season, Team, 
             G, A1, A2, Points,
             iSF, iCF, iFF, ixG,
             GIVE_o, GIVE_n, GIVE_d, 
             TAKE_o, TAKE_n, TAKE_d,
             GIVE_adj, TAKE_adj
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
  
    }

  }
fun.faceoff_SH <- function(data, venue) {
  
  if (venue == "home_team") {
    
    faceoffs <- data %>% 
      filter(game_strength_state %in% c("4v5", "3v5", "3v4"), 
             event_type == "FAC"
             ) %>% 
      group_by(event_player_2, game_id, season) %>% 
      mutate(FOW = 1 * (home_team == event_team), 
             FOL = 1 * (away_team == event_team)
             ) %>% 
      summarise(Team = first(home_team),
                FOW = sum(FOW), 
                FOL = sum(FOL)
                ) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      rename(player = event_player_2) %>% 
      ungroup() %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(faceoffs)
  
    }
  else {
    
    faceoffs <- data %>% 
      filter(game_strength_state %in% c("5v4", "5v3", "4v3"), 
             event_type == "FAC"
             ) %>% 
      group_by(event_player_1, game_id, season) %>% 
      mutate(FOW = 1 * (away_team == event_team), 
             FOL = 1 * (home_team == event_team)
             ) %>% 
      summarise(Team = first(away_team),
                FOW = sum(FOW), 
                FOL = sum(FOL)
                ) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      rename(player = event_player_1) %>% 
      ungroup() %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(faceoffs)
  
    }

  }
fun.penalty_SH <- function(data, venue) {
  
  if (venue == "home_team") {
    
    pen_1 <- data %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(game_strength_state %in% c("4v5", "3v5", "3v4"),  
             event_team == home_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(home_team),
                iPENT2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHF_o = sum(event_type == "HIT" & event_zone == "Off"), 
                iHF_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHF_d = sum(event_type == "HIT" & event_zone == "Def"), 
                
                iHF_adj = sum((event_type == "HIT") * score_adj_SH$home_HF_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    
    pen_2 <- data %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(game_strength_state %in% c("4v5", "3v5", "3v4"),  
             event_team == away_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(home_team), 
                iPEND2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHA_o = sum(event_type == "HIT" & event_zone == "Def"), 
                iHA_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHA_d = sum(event_type == "HIT" & event_zone == "Off"), 
                
                iHA_adj = sum((event_type == "HIT") * score_adj_SH$home_HA_adj[home_lead_state]), 
                
                iBLK = sum(event_type == "BLOCK"), 
                iBLK_adj = sum((event_type == "BLOCK") * score_adj_SH$home_block_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    
    merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      select(player, game_id, season, Team, 
             iPENT2, iPEND2, iPENT5, iPEND5, 
             iBLK, iBLK_adj, 
             iHF_o, iHF_n, iHF_d, 
             iHA_o, iHA_n, iHA_d, 
             iHF_adj, iHA_adj
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
  
    }
  else {
    
    pen_1 <- data %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(game_strength_state %in% c("5v4", "5v3", "4v3"),  
             event_team == away_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(away_team),
                iPENT2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHF_o = sum(event_type == "HIT" & event_zone == "Off"), 
                iHF_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHF_d = sum(event_type == "HIT" & event_zone == "Def"), 
                
                iHF_adj = sum((event_type == "HIT") * score_adj_SH$away_HF_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    pen_2 <- data %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(game_strength_state %in% c("5v4", "5v3", "4v3"),  
             event_team == home_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(away_team),
                iPEND2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHA_o = sum(event_type == "HIT" & event_zone == "Def"), 
                iHA_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHA_d = sum(event_type == "HIT" & event_zone == "Off"), 
                
                iHA_adj = sum((event_type == "HIT") * score_adj_SH$away_HA_adj[home_lead_state]), 
                
                iBLK = sum(event_type == "BLOCK"), 
                iBLK_adj = sum((event_type == "BLOCK") * score_adj_SH$away_block_adj[home_lead_state])
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    
    merged <- Reduce(function(...) merge(..., all=TRUE), list(pen_1, pen_2))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      select(player, game_id, season, Team, 
             iPENT2, iPEND2, iPENT5, iPEND5, 
             iBLK, iBLK_adj, 
             iHF_o, iHF_n, iHF_d, 
             iHA_o, iHA_n, iHA_d, 
             iHF_adj, iHA_adj
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
  
    }

  }

###  Run all functions
fun.combine_counts_SH <- function(data) { 
  
  print(paste("season:", unique(data$season)), quote = F)
  
  # Filter pbp / join prior face time for corsi events
  data <- data %>% 
    filter(game_strength_state %in% st.pp_strength, 
           game_period < 5
           ) %>% 
    select(-c(face_index:shift_length)) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, 
                              ifelse(scradj <= -3, -3, scradj)),
           home_lead_state = ifelse(home_lead < 0, 1, 
                                    ifelse(home_lead == 0, 2, 
                                           ifelse(home_lead > 0, 3, home_lead))), 
           str_state = ifelse(game_strength_state %in% c("5v4", "4v5"), 1, 
                              ifelse(game_strength_state %in% c("5v3", "3v5"), 2, 
                                     ifelse(game_strength_state %in% c("4v3", "3v4"), 3, NA))), 
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           ) %>% 
    rename(pred_goal = pred_XGB_7)
  
  
  ### Run functions
  shot_metrics <- fun.shotmetrics_SH(data)
  zone_start <- fun.ZS_compute_SH(data)
  
  print("counts", quote = F)
  counts_all <- fun.counts_SH(data, "home_team") %>% 
    rbind(., fun.counts_SH(data, "away_team")) %>% 
    arrange(player, game_id)
  
  print("faceoffs", quote = F)
  faceoff_all <- fun.faceoff_SH(data, "home_team") %>% 
    rbind(., fun.faceoff_SH(data, "away_team")) %>% 
    arrange(player, game_id)
  
  print("penalties", quote = F)
  penalty_all <- fun.penalty_SH(data, "home_team") %>% 
    rbind(., fun.penalty_SH(data, "away_team")) %>% 
    arrange(player, game_id)
  
  
  # Join
  print("combine", quote = F)
  metrics_combined <- shot_metrics %>% 
    left_join(., zone_start,  by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., counts_all,  by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., faceoff_all, by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., penalty_all, by = c("player", "game_id", "season", "Team")) %>% 
    select(player, game_id, game_date, season, Team, Opponent, is_home, TOI, 
           G, A1, A2, Points, 
           iSF, iFF, iCF, ixG,
           iBLK, iBLK_adj, 
           GIVE_o:TAKE_d, GIVE_adj, TAKE_adj, 
           iHF_o:iHA_d, iHF_adj, iHA_adj, 
           FOW, FOL, 
           OZS, NZS, DZS, 
           iPENT2, iPEND2, iPENT5, iPEND5, 
           onGF:onxGA_state, 
           t_TOI:t_xGA_state
           ) %>% 
    # Patch for if no SH goals were scored
    mutate(A1 = ifelse(is.na(A1), 0, A1), 
           A2 = ifelse(is.na(A2), 0, A2)
           ) %>% 
    arrange(player, game_id) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
    mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
    data.frame()
  
  
  return(metrics_combined)
  
  }


###########################


## ------------------- ##
##   5v5, etc. Games   ##
## ------------------- ##

#########################

# Skaters: On Ice Corsi For / Against, TOI, Team - per game
fun.oniceCorsiH_EV_strength <- function(data, player_slot, scr_adj_list) {
  
  hold <- data %>% 
    summarise(TOI = sum(event_length) / 60, 
              
              Team = first(home_team), 
              Opponent = first(away_team), 
              is_home = 1, 
              
              # Unadjusted
              GF =  sum(event_type == "GOAL" & event_team == home_team), 
              GA =  sum(event_type == "GOAL" & event_team == away_team), 
              SF =  sum(event_type %in% st.shot_events & event_team == home_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == away_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == home_team), 
              FA =  sum(event_type %in% st.fenwick_events & event_team == away_team),
              CF =  sum(event_type %in% st.corsi_events & event_team == home_team), 
              CA =  sum(event_type %in% st.corsi_events & event_team == away_team),
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              
              # Normal Score Adjustment
              GF_adj =  sum((event_type == "GOAL" & event_team == home_team) * scr_adj_list$home_goal_adj[home_lead_state]), 
              GA_adj =  sum((event_type == "GOAL" & event_team == away_team) * scr_adj_list$away_goal_adj[home_lead_state]), 
              SF_adj =  sum((event_type %in% st.shot_events & event_team == home_team) * scr_adj_list$home_shots_adj[home_lead]), 
              SA_adj =  sum((event_type %in% st.shot_events & event_team == away_team) * scr_adj_list$away_shots_adj[home_lead]), 
              FF_adj =  sum((event_type %in% st.fenwick_events & event_team == home_team) * scr_adj_list$home_fenwick_adj[home_lead]), 
              FA_adj =  sum((event_type %in% st.fenwick_events & event_team == away_team) * scr_adj_list$away_fenwick_adj[home_lead]),
              CF_adj =  sum((event_type %in% st.corsi_events & event_team == home_team) * scr_adj_list$home_corsi_adj[home_lead]), 
              CA_adj =  sum((event_type %in% st.corsi_events & event_team == away_team) * scr_adj_list$away_corsi_adj[home_lead]),
              xGF_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * scr_adj_list$home_xG_adj[home_lead_state])),
              xGA_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * scr_adj_list$away_xG_adj[home_lead_state]))
              )
  
  return(hold)
  
  }
fun.oniceCorsiA_EV_strength <- function(data, player_slot, scr_adj_list) {
  
  hold <- data %>% 
    summarise(TOI = sum(event_length) / 60, 
              
              Team = first(away_team), 
              Opponent = first(home_team), 
              is_home = 0, 
              
              # Unadjusted
              GF =  sum(event_type == "GOAL" & event_team == away_team), 
              GA =  sum(event_type == "GOAL" & event_team == home_team), 
              SF =  sum(event_type %in% st.shot_events & event_team == away_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == home_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == away_team), 
              FA =  sum(event_type %in% st.fenwick_events & event_team == home_team),
              CF =  sum(event_type %in% st.corsi_events & event_team == away_team), 
              CA =  sum(event_type %in% st.corsi_events & event_team == home_team),
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              
              # Normal Score Adjustment
              GF_adj =  sum((event_type == "GOAL" & event_team == away_team) * scr_adj_list$away_goal_adj[home_lead_state]), 
              GA_adj =  sum((event_type == "GOAL" & event_team == home_team) * scr_adj_list$home_goal_adj[home_lead_state]), 
              SF_adj =  sum((event_type %in% st.shot_events & event_team == away_team) * scr_adj_list$away_shots_adj[home_lead]), 
              SA_adj =  sum((event_type %in% st.shot_events & event_team == home_team) * scr_adj_list$home_shots_adj[home_lead]), 
              FF_adj =  sum((event_type %in% st.fenwick_events & event_team == away_team) * scr_adj_list$away_fenwick_adj[home_lead]), 
              FA_adj =  sum((event_type %in% st.fenwick_events & event_team == home_team) * scr_adj_list$home_fenwick_adj[home_lead]),
              CF_adj =  sum((event_type %in% st.corsi_events & event_team == away_team) * scr_adj_list$away_corsi_adj[home_lead]), 
              CA_adj =  sum((event_type %in% st.corsi_events & event_team == home_team) * scr_adj_list$home_corsi_adj[home_lead]),
              xGF_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * scr_adj_list$away_xG_adj[home_lead_state])),
              xGA_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * scr_adj_list$home_xG_adj[home_lead_state]))
              )
  
  return(hold)
  
  }
fun.componiceCorsi_EV_strength <- function(data, scr_adj_list) {
  
  print("on_ice_home", quote = F)
  h1 <- data %>% group_by(game_id, game_date, season, home_on_1, home_team) %>% fun.oniceCorsiH_EV_strength(., "home_on_1", scr_adj_list) %>% rename(player = home_on_1) %>% data.frame()
  h2 <- data %>% group_by(game_id, game_date, season, home_on_2, home_team) %>% fun.oniceCorsiH_EV_strength(., "home_on_2", scr_adj_list) %>% rename(player = home_on_2) %>% data.frame()
  h3 <- data %>% group_by(game_id, game_date, season, home_on_3, home_team) %>% fun.oniceCorsiH_EV_strength(., "home_on_3", scr_adj_list) %>% rename(player = home_on_3) %>% data.frame()
  h4 <- data %>% group_by(game_id, game_date, season, home_on_4, home_team) %>% fun.oniceCorsiH_EV_strength(., "home_on_4", scr_adj_list) %>% rename(player = home_on_4) %>% data.frame()
  h5 <- data %>% group_by(game_id, game_date, season, home_on_5, home_team) %>% fun.oniceCorsiH_EV_strength(., "home_on_5", scr_adj_list) %>% rename(player = home_on_5) %>% data.frame()
  h6 <- data %>% group_by(game_id, game_date, season, home_on_6, home_team) %>% fun.oniceCorsiH_EV_strength(., "home_on_6", scr_adj_list) %>% rename(player = home_on_6) %>% data.frame()
  
  print("on_ice_away", quote = F)
  a1 <- data %>% group_by(game_id, game_date, season, away_on_1, away_team) %>% fun.oniceCorsiA_EV_strength(., "away_on_1", scr_adj_list) %>% rename(player = away_on_1) %>% data.frame()
  a2 <- data %>% group_by(game_id, game_date, season, away_on_2, away_team) %>% fun.oniceCorsiA_EV_strength(., "away_on_2", scr_adj_list) %>% rename(player = away_on_2) %>% data.frame()
  a3 <- data %>% group_by(game_id, game_date, season, away_on_3, away_team) %>% fun.oniceCorsiA_EV_strength(., "away_on_3", scr_adj_list) %>% rename(player = away_on_3) %>% data.frame()
  a4 <- data %>% group_by(game_id, game_date, season, away_on_4, away_team) %>% fun.oniceCorsiA_EV_strength(., "away_on_4", scr_adj_list) %>% rename(player = away_on_4) %>% data.frame()
  a5 <- data %>% group_by(game_id, game_date, season, away_on_5, away_team) %>% fun.oniceCorsiA_EV_strength(., "away_on_5", scr_adj_list) %>% rename(player = away_on_5) %>% data.frame()
  a6 <- data %>% group_by(game_id, game_date, season, away_on_6, away_team) %>% fun.oniceCorsiA_EV_strength(., "away_on_6", scr_adj_list) %>% rename(player = away_on_6) %>% data.frame()
  
  
  # Join all data.frames
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
  
  merge_return <- merged %>% 
    group_by(player, game_id, game_date, season) %>%  
    summarise(Team =     first(Team), 
              Opponent = first(Opponent), 
              is_home =  first(is_home), 
              TOI =      sum(TOI), 
              
              GF =      sum(GF), 
              GA =      sum(GA), 
              SF =      sum(SF), 
              SA =      sum(SA), 
              FF =      sum(FF), 
              FA =      sum(FA), 
              CF =      sum(CF), 
              CA =      sum(CA), 
              xGF =     sum(xGF), 
              xGA =     sum(xGA), 
              
              GF_adj =  sum(GF_adj), 
              GA_adj =  sum(GA_adj), 
              SF_adj =  sum(SF_adj), 
              SA_adj =  sum(SA_adj), 
              FF_adj =  sum(FF_adj), 
              FA_adj =  sum(FA_adj), 
              CF_adj =  sum(CF_adj), 
              CA_adj =  sum(CA_adj), 
              xGF_adj = sum(xGF_adj), 
              xGA_adj = sum(xGA_adj)
              ) %>% 
    filter(!is.na(player)) %>% 
    data.frame()
  
  return(merge_return)
  
  }

# Teams: On Ice Corsi For / Against, TOI - per game
fun.oniceTeamCorsiH_EV_strength <- function(data, scr_adj_list) {
  
  hold <- data %>% 
    summarise(TOI = sum(event_length) / 60, 
              Team = first(home_team), 
              
              # Unadjusted
              GF =  sum(event_type == "GOAL" & event_team == home_team), 
              GA =  sum(event_type == "GOAL" & event_team == away_team), 
              SF =  sum(event_type %in% st.shot_events & event_team == home_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == away_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == home_team), 
              FA =  sum(event_type %in% st.fenwick_events & event_team == away_team),
              CF =  sum(event_type %in% st.corsi_events & event_team == home_team), 
              CA =  sum(event_type %in% st.corsi_events & event_team == away_team),
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              
              # Normal Score Adjustment
              GF_adj =  sum((event_type == "GOAL" & event_team == home_team) * scr_adj_list$home_goal_adj[home_lead_state]), 
              GA_adj =  sum((event_type == "GOAL" & event_team == away_team) * scr_adj_list$away_goal_adj[home_lead_state]), 
              SF_adj =  sum((event_type %in% st.shot_events & event_team == home_team) * scr_adj_list$home_shots_adj[home_lead]), 
              SA_adj =  sum((event_type %in% st.shot_events & event_team == away_team) * scr_adj_list$away_shots_adj[home_lead]), 
              FF_adj =  sum((event_type %in% st.fenwick_events & event_team == home_team) * scr_adj_list$home_fenwick_adj[home_lead]), 
              FA_adj =  sum((event_type %in% st.fenwick_events & event_team == away_team) * scr_adj_list$away_fenwick_adj[home_lead]),
              CF_adj =  sum((event_type %in% st.corsi_events & event_team == home_team) * scr_adj_list$home_corsi_adj[home_lead]), 
              CA_adj =  sum((event_type %in% st.corsi_events & event_team == away_team) * scr_adj_list$away_corsi_adj[home_lead]),
              xGF_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * scr_adj_list$home_xG_adj[home_lead_state])),
              xGA_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * scr_adj_list$away_xG_adj[home_lead_state]))
              )
  
  return(hold)
  
  }
fun.oniceTeamCorsiA_EV_strength <- function(data, scr_adj_list) {
  
  hold <- data %>% 
    summarise(TOI = sum(event_length) / 60, 
              Team = first(away_team), 
              
              # Unadjusted
              GF =  sum(event_type == "GOAL" & event_team == away_team), 
              GA =  sum(event_type == "GOAL" & event_team == home_team), 
              SF =  sum(event_type %in% st.shot_events & event_team == away_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == home_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == away_team), 
              FA =  sum(event_type %in% st.fenwick_events & event_team == home_team),
              CF =  sum(event_type %in% st.corsi_events & event_team == away_team), 
              CA =  sum(event_type %in% st.corsi_events & event_team == home_team),
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              
              # Normal Score Adjustment
              GF_adj =  sum((event_type == "GOAL" & event_team == away_team) * scr_adj_list$away_goal_adj[home_lead_state]), 
              GA_adj =  sum((event_type == "GOAL" & event_team == home_team) * scr_adj_list$home_goal_adj[home_lead_state]), 
              SF_adj =  sum((event_type %in% st.shot_events & event_team == away_team) * scr_adj_list$away_shots_adj[home_lead]), 
              SA_adj =  sum((event_type %in% st.shot_events & event_team == home_team) * scr_adj_list$home_shots_adj[home_lead]), 
              FF_adj =  sum((event_type %in% st.fenwick_events & event_team == away_team) * scr_adj_list$away_fenwick_adj[home_lead]), 
              FA_adj =  sum((event_type %in% st.fenwick_events & event_team == home_team) * scr_adj_list$home_fenwick_adj[home_lead]),
              CF_adj =  sum((event_type %in% st.corsi_events & event_team == away_team) * scr_adj_list$away_corsi_adj[home_lead]), 
              CA_adj =  sum((event_type %in% st.corsi_events & event_team == home_team) * scr_adj_list$home_corsi_adj[home_lead]),
              xGF_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * scr_adj_list$away_xG_adj[home_lead_state])),
              xGA_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * scr_adj_list$home_xG_adj[home_lead_state]))
              )
  
  return(hold)
  
  }
fun.componiceCorsiTeam_EV_strength <- function(data, scr_adj_list) {
  
  print("team_on_ice_home", quote = F)
  h_df <- data %>% 
    group_by(game_id, season, home_team) %>% 
    fun.oniceTeamCorsiH_EV_strength(., scr_adj_list) %>% 
    data.frame()
  
  print("team_on_ice_away", quote = F)
  a_df <- data %>% 
    group_by(game_id, season, away_team) %>% 
    fun.oniceTeamCorsiA_EV_strength(., scr_adj_list) %>% 
    data.frame()
  
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h_df, a_df))
  
  merge_return <- merged %>% 
    group_by(Team, game_id, season) %>% 
    summarise_at(vars(TOI, 
                      GF, GA, SF, SA, FF, FA, CF, CA, xGF, xGA, 
                      GF_adj, GA_adj, SF_adj, SA_adj, FF_adj, FA_adj, CF_adj, CA_adj, xGF_adj, xGA_adj), 
                 funs(sum)
                 ) %>% 
    data.frame()
  
  return(merge_return)
  
  }

# Merge skaters and team on-ice metrics
fun.shotmetrics_EV_strength <- function(data, scr_adj_list) {
  
  # Run functions
  skater <- fun.componiceCorsi_EV_strength(data, scr_adj_list)
  team <-   fun.componiceCorsiTeam_EV_strength(data, scr_adj_list)
  
  joined <- left_join(skater, team, by = c("game_id", "Team", "season"))
  
  names(joined) <- c("player", "game_id", "game_date", "season", "Team", "Opponent", "is_home", 
                     
                     "TOI", 
                     "onGF", "onGA", "onSF", "onSA", "onFF", "onFA", "onCF", "onCA", "onxGF", "onxGA", 
                     "onGF_adj", "onGA_adj", "onSF_adj", "onSA_adj", "onFF_adj", "onFA_adj", "onCF_adj", "onCA_adj", "onxGF_adj", "onxGA_adj", 
                     
                     "t_TOI", 
                     "t_GF", "t_GA", "t_SF", "t_SA", "t_FF", "t_FA", "t_CF", "t_CA", "t_xGF", "t_xGA", 
                     "t_GF_adj", "t_GA_adj", "t_SF_adj", "t_SA_adj", "t_FF_adj", "t_FA_adj", "t_CF_adj", "t_CA_adj", "t_xGF_adj", "t_xGA_adj"
                     )
  
  # Remove goalies
  fun.goalie_remove <- function(data_) {
    
    # Identifies goalies within a given pbp data.frame & returns a data.frame to join for removal
    goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data_$home_goalie, data_$away_goalie))))), 
                                is_goalie = 1)
    
    goalie_return$player <- as.character(goalie_return$player)
    
    return(goalie_return)
    
    }
  
  if (nrow(data) > 0) { 
    goalieremove <- fun.goalie_remove(data_ = data)
    
    }
  
  if (nrow(data) > 0) { 
    all <- joined %>% 
      left_join(., goalieremove, "player") %>% 
      filter(is.na(is_goalie)) %>% 
      select(-c(is_goalie)) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    } else { 
      all <- joined %>% 
        arrange(player, game_id) %>% 
        data.frame()
    
      }
  
  return(all)
  
  }

# Zone Starts
fun.ZS_H_EV_strength <- function(data, venue) {
  
  hold <- data %>% 
    filter(event_type == "FAC") %>%  
    summarise(Team = first(home_team),
              OZS = sum(event_type == "FAC" & home_zone == "Off"), 
              NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
              DZS = sum(event_type == "FAC" & home_zone == "Def")
              )
  
  return(hold)
  
  }
fun.ZS_A_EV_strength <- function(data, venue) {
  
  hold <- data %>% 
    filter(event_type == "FAC") %>%  
    summarise(Team = first(away_team),
              OZS = sum(event_type == "FAC" & home_zone == "Def"), 
              NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
              DZS = sum(event_type == "FAC" & home_zone == "Off")
              )
  
  return(hold)
  
  }
fun.ZS_compute_EV_strength <- function(data) {
  
  print("zone_starts_home", quote = F)
  h1 <- data %>% group_by(game_id, season, home_on_1) %>% fun.ZS_H_EV_strength(., "home_on_1") %>% rename(player = home_on_1) %>% data.frame()
  h2 <- data %>% group_by(game_id, season, home_on_2) %>% fun.ZS_H_EV_strength(., "home_on_2") %>% rename(player = home_on_2) %>% data.frame()
  h3 <- data %>% group_by(game_id, season, home_on_3) %>% fun.ZS_H_EV_strength(., "home_on_3") %>% rename(player = home_on_3) %>% data.frame()
  h4 <- data %>% group_by(game_id, season, home_on_4) %>% fun.ZS_H_EV_strength(., "home_on_4") %>% rename(player = home_on_4) %>% data.frame()
  h5 <- data %>% group_by(game_id, season, home_on_5) %>% fun.ZS_H_EV_strength(., "home_on_5") %>% rename(player = home_on_5) %>% data.frame()
  h6 <- data %>% group_by(game_id, season, home_on_6) %>% fun.ZS_H_EV_strength(., "home_on_6") %>% rename(player = home_on_6) %>% data.frame()
  
  print("zone_starts_away", quote = F)
  a1 <- data %>% group_by(game_id, season, away_on_1) %>% fun.ZS_A_EV_strength(., "away_on_1") %>% rename(player = away_on_1) %>% data.frame()
  a2 <- data %>% group_by(game_id, season, away_on_2) %>% fun.ZS_A_EV_strength(., "away_on_2") %>% rename(player = away_on_2) %>% data.frame()
  a3 <- data %>% group_by(game_id, season, away_on_3) %>% fun.ZS_A_EV_strength(., "away_on_3") %>% rename(player = away_on_3) %>% data.frame()
  a4 <- data %>% group_by(game_id, season, away_on_4) %>% fun.ZS_A_EV_strength(., "away_on_4") %>% rename(player = away_on_4) %>% data.frame()
  a5 <- data %>% group_by(game_id, season, away_on_5) %>% fun.ZS_A_EV_strength(., "away_on_5") %>% rename(player = away_on_5) %>% data.frame()
  a6 <- data %>% group_by(game_id, season, away_on_6) %>% fun.ZS_A_EV_strength(., "away_on_6") %>% rename(player = away_on_6) %>% data.frame()
  
  # Join
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
  
  summed <- merged %>% 
    group_by(player, game_id, season) %>%  
    summarise(Team = first(Team),
              OZS =  sum(OZS), 
              NZS =  sum(NZS), 
              DZS =  sum(DZS)
              ) %>% 
    arrange(player) %>% 
    data.frame()
  
  return(summed)
  
  }

# Boxscore
fun.counts_EV_strength <- function(data, venue) {
  
  if (venue == "home_team") {
    
    # Counts
    counts_1 <- data %>% 
      filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE"), 
             event_team == home_team
             ) %>% 
      group_by(event_player_1, game_id, season) %>% 
      summarise(Team = first(home_team),
                G = sum(event_type == "GOAL"),
                iSF = sum(event_type %in% st.shot_events),
                iCF = sum(event_type %in% st.corsi_events), 
                iFF = sum(event_type %in% st.fenwick_events),
                ixG = sum(na.omit(pred_goal)), 
                
                GIVE_o = sum(event_type == "GIVE" & event_zone == "Off"), 
                GIVE_n = sum(event_type == "GIVE" & event_zone == "Neu"), 
                GIVE_d = sum(event_type == "GIVE" & event_zone == "Def"), 
                
                TAKE_o = sum(event_type == "TAKE" & event_zone == "Off"), 
                TAKE_n = sum(event_type == "TAKE" & event_zone == "Neu"), 
                TAKE_d = sum(event_type == "TAKE" & event_zone == "Def")
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    counts_2 <- data %>% 
      filter(event_type %in% c("GOAL"), 
             event_team == home_team
             ) %>% 
      group_by(event_player_2, game_id, season) %>% 
      summarise(Team = first(home_team), 
                A1 = sum(event_type == "GOAL")
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    counts_3 <- data %>% 
      filter(event_type == "GOAL", 
             event_team == home_team
             ) %>% 
      group_by(event_player_3, game_id, season) %>%
      summarise(Team = first(home_team), 
                A2 = sum(event_type == "GOAL")
                ) %>% 
      rename(player = event_player_3) %>% 
      data.frame()
    
    # Join
    merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate(Points = G + A1 + A2) %>% 
      select(player, game_id, season, Team, 
             G, A1, A2, Points, 
             iSF, iCF, iFF, ixG,
             GIVE_o, GIVE_n, GIVE_d, TAKE_o, TAKE_n, TAKE_d
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
    
    }
  else {
    
    # Compile
    counts_1 <- data %>% 
      filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE"), 
             event_team == away_team
             ) %>% 
      group_by(event_player_1, game_id, season) %>% 
      summarise(Team = first(away_team),
                G = sum(event_type == "GOAL"),
                iSF = sum(event_type %in% st.shot_events),
                iCF = sum(event_type %in% st.corsi_events),
                iFF = sum(event_type %in% st.fenwick_events), 
                ixG = sum(na.omit(pred_goal)), 
                
                GIVE_o = sum(event_type == "GIVE" & event_zone == "Off"), 
                GIVE_n = sum(event_type == "GIVE" & event_zone == "Neu"), 
                GIVE_d = sum(event_type == "GIVE" & event_zone == "Def"), 
                
                TAKE_o = sum(event_type == "TAKE" & event_zone == "Off"), 
                TAKE_n = sum(event_type == "TAKE" & event_zone == "Neu"), 
                TAKE_d = sum(event_type == "TAKE" & event_zone == "Def")
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    counts_2 <- data %>% 
      filter(event_type %in% c("GOAL"), 
             event_team == away_team
             ) %>% 
      group_by(event_player_2, game_id, season) %>% 
      summarise(Team = first(away_team), 
                A1 = sum(event_type == "GOAL")
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    counts_3 <- data %>% 
      filter(event_type == "GOAL", 
             event_team == away_team
             ) %>% 
      group_by(event_player_3, game_id, season) %>%
      summarise(Team = first(away_team), 
                A2 = sum(event_type == "GOAL")
                ) %>% 
      rename(player = event_player_3) %>% 
      data.frame()
    
    # Join
    merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      mutate(Points = G + A1 + A2) %>% 
      select(player, game_id, season, Team, 
             G, A1, A2, Points,
             iSF, iCF, iFF, ixG,
             GIVE_o, GIVE_n, GIVE_d, TAKE_o, TAKE_n, TAKE_d
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
    
    }
  
  }
fun.faceoff_EV_strength <- function(data, venue) {
  
  if (venue == "home_team") {
    
    faceoffs <- data %>% 
      filter(event_type == "FAC") %>% 
      group_by(event_player_2, game_id, season) %>% 
      mutate(FOW = 1 * (home_team == event_team), 
             FOL = 1 * (away_team == event_team)
             ) %>% 
      summarise(Team = first(home_team),
                FOW = sum(FOW), 
                FOL = sum(FOL)
                ) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      rename(player = event_player_2) %>% 
      ungroup() %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(faceoffs)
    
    }
  else {
    
    faceoffs <- data %>% 
      filter(event_type == "FAC") %>% 
      group_by(event_player_1, game_id, season) %>% 
      mutate(FOW = 1 * (away_team == event_team), 
             FOL = 1 * (home_team == event_team)
             ) %>% 
      summarise(Team = first(away_team),
                FOW = sum(FOW), 
                FOL = sum(FOL)
                ) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      rename(player = event_player_1) %>% 
      ungroup() %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(faceoffs)
    
    }
  
  }
fun.penalty_EV_strength <- function(data, venue) {
  
  if (venue == "home_team") {
    
    pen_1 <- data %>% 
      filter(event_team == home_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      group_by(event_player_1, game_id, season) %>% 
      summarise(Team = first(home_team),
                iPENT2 = sum(na.omit(1 * (event_type == "PENL") + # Manny Perry's Filter - found on GitHub linked above
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHF_o = sum(event_type == "HIT" & event_zone == "Off"), 
                iHF_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHF_d = sum(event_type == "HIT" & event_zone == "Def")
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    
    pen_2 <- data %>% 
      filter(event_team == away_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      group_by(event_player_2, game_id, season) %>% 
      summarise(Team = first(home_team), 
                iPEND2 = sum(na.omit(1 * (event_type == "PENL") + # Manny Perry's Filter - found on GitHub linked above
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHA_o = sum(event_type == "HIT" & event_zone == "Def"), 
                iHA_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHA_d = sum(event_type == "HIT" & event_zone == "Off"), 
                
                iBLK = sum(event_type == "BLOCK")
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    
    merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      select(player, game_id, season, Team, 
             iPENT2, iPEND2, iPENT5, iPEND5, iBLK, 
             iHF_o, iHF_n, iHF_d, iHA_o, iHA_n, iHA_d
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
    
    }
  else {
    
    pen_1 <- data %>% 
      filter(event_team == away_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      group_by(event_player_1, game_id, season) %>% 
      summarise(Team = first(away_team),
                iPENT2 = sum(na.omit(1 * (event_type == "PENL") + # Manny Perry's Filter - found on GitHub linked above
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHF_o = sum(event_type == "HIT" & event_zone == "Off"), 
                iHF_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHF_d = sum(event_type == "HIT" & event_zone == "Def")
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    pen_2 <- data %>% 
      filter(event_team == home_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      group_by(event_player_2, game_id, season) %>% 
      summarise(Team = first(away_team),
                iPEND2 = sum(na.omit(1 * (event_type == "PENL") + # Manny Perry's Filter - found on GitHub linked above
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHA_o = sum(event_type == "HIT" & event_zone == "Def"), 
                iHA_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHA_d = sum(event_type == "HIT" & event_zone == "Off"), 
                
                iBLK = sum(event_type == "BLOCK")
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    
    merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      select(player, game_id, season, Team, 
             iPENT2, iPEND2, iPENT5, iPEND5, iBLK, 
             iHF_o, iHF_n, iHF_d, iHA_o, iHA_n, iHA_d
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
    
    }
  
  }

###  Run all functions
fun.combine_counts_EV_strength <- function(data, strength, scr_adj_list) { 
  
  print(paste("season:", unique(data$season)), quote = F)
  
  # Filter pbp / join prior face time for corsi events
  data <- data %>% 
    filter(game_strength_state %in% strength, 
           game_period < 5
           ) %>% 
    select(-c(face_index:shift_length)) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, 
                              ifelse(scradj <= -3, -3, scradj)),
           home_lead_state = ifelse(home_lead < 0, 1, 
                                    ifelse(home_lead == 0, 2, 
                                           ifelse(home_lead > 0, 3, home_lead))), 
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           ) %>% 
    rename(pred_goal = pred_XGB_7)
  
  
  ### Run functions
  shot_metrics <- fun.shotmetrics_EV_strength(data, scr_adj_list)
  zone_start <-   fun.ZS_compute_EV_strength(data)
  
  print("counts", quote = F)
  counts_all <- fun.counts_EV_strength(data, "home_team") %>% 
    rbind(., fun.counts_EV_strength(data, "away_team")) %>% 
    arrange(player, game_id)
  
  print("faceoffs", quote = F)
  faceoff_all <- fun.faceoff_EV_strength(data, "home_team") %>% 
    rbind(., fun.faceoff_EV_strength(data, "away_team")) %>% 
    arrange(player, game_id)
  
  print("penalties", quote = F)
  penalty_all <- fun.penalty_EV_strength(data, "home_team") %>% 
    rbind(., fun.penalty_EV_strength(data, "away_team")) %>% 
    arrange(player, game_id)
  
  
  # Join
  print("combine", quote = F)
  
  metrics_combined <- shot_metrics %>% 
    left_join(., zone_start,  by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., counts_all,  by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., faceoff_all, by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., penalty_all, by = c("player", "game_id", "season", "Team")) %>% 
    select(player, game_id, game_date, season, Team, Opponent, is_home, TOI, 
           G, A1, A2, Points, 
           iSF, iFF, iCF, ixG,
           iBLK, 
           GIVE_o:TAKE_d, 
           iHF_o:iHA_d, 
           FOW, FOL, 
           OZS, NZS, DZS, 
           iPENT2, iPEND2, iPENT5, iPEND5, 
           onGF:onxGA_adj, 
           t_TOI:t_xGA_adj
           ) %>% 
    mutate(player = ifelse(player == "SEBASTIAN.AHO" & Team == "NYI", "5EBASTIAN.AHO", player)) %>% # FIX NYI AHO
    arrange(player, game_id) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
    mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
    data.frame()
  
  return(metrics_combined)
  
  }


#########################


## ------------------- ##
##   5v4, etc. Games   ##
## ------------------- ##

#########################

# Skaters: On Ice Corsi For / Against, TOI, Team - per game
fun.oniceCorsiH_PP_strength <- function(data, venue, strength, scr_adj_list) {
  
  hold <- data %>% 
    filter(game_strength_state %in% strength) %>% 
    summarise(TOI = sum(event_length) / 60, 
              
              Team = first(home_team), 
              Opponent = first(away_team), 
              is_home = 1, 
              
              GF =  sum(event_type == "GOAL" & event_team == home_team), 
              GA =  sum(event_type == "GOAL" & event_team == away_team),
              SF =  sum(event_type %in% st.shot_events & event_team == home_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == away_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == home_team), 
              FA =  sum(event_type %in% st.fenwick_events & event_team == away_team),
              CF =  sum(event_type %in% st.corsi_events & event_team == home_team), 
              CA =  sum(event_type %in% st.corsi_events & event_team == away_team), 
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              
              GF_adj =  sum((event_type == "GOAL" & event_team == home_team) * scr_adj_list$home_goal_adj[home_lead_state]), 
              SF_adj =  sum((event_type %in% st.shot_events & event_team == home_team) * scr_adj_list$home_shot_adj[home_lead_state]), 
              FF_adj =  sum((event_type %in% st.fenwick_events & event_team == home_team) * scr_adj_list$home_fenwick_adj[home_lead_state]), 
              CF_adj =  sum((event_type %in% st.corsi_events & event_team == home_team) * scr_adj_list$home_corsi_adj[home_lead_state]), 
              xGF_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * scr_adj_list$home_xG_adj[home_lead_state]))
              )
  
  return(hold)
  
  }
fun.oniceCorsiA_PP_strength <- function(data, venue, strength, scr_adj_list) {
  
  hold <- data %>% 
    filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = "")) %>%  # reverse strength state entered into function
    summarise(TOI = sum(event_length) / 60, 
              
              Team = first(away_team), 
              Opponent = first(home_team), 
              is_home = 0, 
              
              GF =  sum(event_type == "GOAL" & event_team == away_team), 
              GA =  sum(event_type == "GOAL" & event_team == home_team),
              SF =  sum(event_type %in% st.shot_events & event_team == away_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == home_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == away_team), 
              FA =  sum(event_type %in% st.fenwick_events & event_team == home_team),
              CF =  sum(event_type %in% st.corsi_events & event_team == away_team), 
              CA =  sum(event_type %in% st.corsi_events & event_team == home_team),
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              
              GF_adj =  sum((event_type == "GOAL" & event_team == away_team) * scr_adj_list$away_goal_adj[home_lead_state]),
              SF_adj =  sum((event_type %in% st.shot_events & event_team == away_team) * scr_adj_list$away_shot_adj[home_lead_state]), 
              FF_adj =  sum((event_type %in% st.fenwick_events & event_team == away_team) * scr_adj_list$away_fenwick_adj[home_lead_state]), 
              CF_adj =  sum((event_type %in% st.corsi_events & event_team == away_team) * scr_adj_list$away_corsi_adj[home_lead_state]), 
              xGF_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * scr_adj_list$away_xG_adj[home_lead_state]))
              )
  
  return(hold)
  
  }
fun.componiceCorsi_PP_strength <- function(data, strength, scr_adj_list) {
  
  print("on_ice_home", quote = F)
  h1 <- data %>% group_by(game_id, game_date, season, home_on_1, home_team) %>% fun.oniceCorsiH_PP_strength(., "home_on_1", strength, scr_adj_list) %>% rename(player = home_on_1) %>% data.frame()
  h2 <- data %>% group_by(game_id, game_date, season, home_on_2, home_team) %>% fun.oniceCorsiH_PP_strength(., "home_on_2", strength, scr_adj_list) %>% rename(player = home_on_2) %>% data.frame()
  h3 <- data %>% group_by(game_id, game_date, season, home_on_3, home_team) %>% fun.oniceCorsiH_PP_strength(., "home_on_3", strength, scr_adj_list) %>% rename(player = home_on_3) %>% data.frame()
  h4 <- data %>% group_by(game_id, game_date, season, home_on_4, home_team) %>% fun.oniceCorsiH_PP_strength(., "home_on_4", strength, scr_adj_list) %>% rename(player = home_on_4) %>% data.frame()
  h5 <- data %>% group_by(game_id, game_date, season, home_on_5, home_team) %>% fun.oniceCorsiH_PP_strength(., "home_on_5", strength, scr_adj_list) %>% rename(player = home_on_5) %>% data.frame()
  h6 <- data %>% group_by(game_id, game_date, season, home_on_6, home_team) %>% fun.oniceCorsiH_PP_strength(., "home_on_6", strength, scr_adj_list) %>% rename(player = home_on_6) %>% data.frame()
  
  print("on_ice_away", quote = F)
  a1 <- data %>% group_by(game_id, game_date, season, away_on_1, away_team) %>% fun.oniceCorsiA_PP_strength(., "away_on_1", strength, scr_adj_list) %>% rename(player = away_on_1) %>% data.frame()
  a2 <- data %>% group_by(game_id, game_date, season, away_on_2, away_team) %>% fun.oniceCorsiA_PP_strength(., "away_on_2", strength, scr_adj_list) %>% rename(player = away_on_2) %>% data.frame()
  a3 <- data %>% group_by(game_id, game_date, season, away_on_3, away_team) %>% fun.oniceCorsiA_PP_strength(., "away_on_3", strength, scr_adj_list) %>% rename(player = away_on_3) %>% data.frame()
  a4 <- data %>% group_by(game_id, game_date, season, away_on_4, away_team) %>% fun.oniceCorsiA_PP_strength(., "away_on_4", strength, scr_adj_list) %>% rename(player = away_on_4) %>% data.frame()
  a5 <- data %>% group_by(game_id, game_date, season, away_on_5, away_team) %>% fun.oniceCorsiA_PP_strength(., "away_on_5", strength, scr_adj_list) %>% rename(player = away_on_5) %>% data.frame()
  a6 <- data %>% group_by(game_id, game_date, season, away_on_6, away_team) %>% fun.oniceCorsiA_PP_strength(., "away_on_6", strength, scr_adj_list) %>% rename(player = away_on_6) %>% data.frame()
  
  
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
  
  merge_return <- merged %>% 
    group_by(player, game_id, game_date, season) %>%  
    summarise(Team =     first(Team), 
              Opponent = first(Opponent), 
              is_home =  first(is_home), 
              TOI =     sum(TOI), 
              GF =      sum(GF), 
              GA =      sum(GA), 
              SF =      sum(SF), 
              SA =      sum(SA), 
              FF =      sum(FF), 
              FA =      sum(FA), 
              CF =      sum(CF), 
              CA =      sum(CA), 
              xGF =     sum(xGF), 
              xGA =     sum(xGA), 
              GF_adj =  sum(GF_adj), 
              SF_adj =  sum(SF_adj), 
              FF_adj =  sum(FF_adj), 
              CF_adj =  sum(CF_adj), 
              xGF_adj = sum(xGF_adj) 
              ) %>% 
    filter(!is.na(player)) %>% 
    data.frame()
  
  return(merge_return)
  
  }

# Teams: On Ice Corsi For / Against, TOI - per game
fun.oniceTeamCorsiH_PP_strength <- function(data, strength, scr_adj_list) {
  
  hold <- data %>% 
    filter(game_strength_state %in% strength) %>% 
    summarise(TOI = sum(event_length) / 60, 
              Team = first(home_team), 
              
              GF =  sum(event_type == "GOAL" & event_team == home_team), 
              GA =  sum(event_type == "GOAL" & event_team == away_team),
              SF =  sum(event_type %in% st.shot_events & event_team == home_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == away_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == home_team), 
              FA =  sum(event_type %in% st.fenwick_events & event_team == away_team),
              CF =  sum(event_type %in% st.corsi_events & event_team == home_team), 
              CA =  sum(event_type %in% st.corsi_events & event_team == away_team), 
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              
              GF_adj =  sum((event_type == "GOAL" & event_team == home_team) * scr_adj_list$home_goal_adj[home_lead_state]), 
              SF_adj =  sum((event_type %in% st.shot_events & event_team == home_team) * scr_adj_list$home_shot_adj[home_lead_state]), 
              FF_adj =  sum((event_type %in% st.fenwick_events & event_team == home_team) * scr_adj_list$home_fenwick_adj[home_lead_state]), 
              CF_adj =  sum((event_type %in% st.corsi_events & event_team == home_team) * scr_adj_list$home_corsi_adj[home_lead_state]), 
              xGF_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * scr_adj_list$home_xG_adj[home_lead_state]))
              )
  
  return(hold)
  
  }
fun.oniceTeamCorsiA_PP_strength <- function(data, strength, scr_adj_list) {
  
  hold <- data %>% 
    filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = "")) %>%  # reverse strength state entered into function
    summarise(TOI = sum(event_length) / 60, 
              Team = first(away_team), 
              
              GF =  sum(event_type == "GOAL" & event_team == away_team), 
              GA =  sum(event_type == "GOAL" & event_team == home_team),
              SF =  sum(event_type %in% st.shot_events & event_team == away_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == home_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == away_team), 
              FA =  sum(event_type %in% st.fenwick_events & event_team == home_team),
              CF =  sum(event_type %in% st.corsi_events & event_team == away_team), 
              CA =  sum(event_type %in% st.corsi_events & event_team == home_team),
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              
              GF_adj =  sum((event_type == "GOAL" & event_team == away_team) * scr_adj_list$away_goal_adj[home_lead_state]),
              SF_adj =  sum((event_type %in% st.shot_events & event_team == away_team) * scr_adj_list$away_shot_adj[home_lead_state]), 
              FF_adj =  sum((event_type %in% st.fenwick_events & event_team == away_team) * scr_adj_list$away_fenwick_adj[home_lead_state]), 
              CF_adj =  sum((event_type %in% st.corsi_events & event_team == away_team) * scr_adj_list$away_corsi_adj[home_lead_state]), 
              xGF_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * scr_adj_list$away_xG_adj[home_lead_state]))
              )
  
  return(hold)
  
  }
fun.componiceCorsiTeam_PP_strength <- function(data, strength, scr_adj_list) {
  
  print("team_on_ice_home", quote = F)
  h_df <- data %>% 
    group_by(game_id, season, home_team) %>% 
    fun.oniceTeamCorsiH_PP_strength(., strength, scr_adj_list) 
  
  print("team_on_ice_away", quote = F)
  a_df <- data %>% 
    group_by(game_id, season, away_team) %>% 
    fun.oniceTeamCorsiA_PP_strength(., strength, scr_adj_list) 
  
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h_df, a_df))
  
  merge_return <- merged %>% 
    group_by(Team, game_id, season) %>% 
    summarise_at(vars(TOI, 
                      GF, GA, SF, SA, FF, FA, CF, CA, xGF, xGA, 
                      GF_adj, SF_adj, FF_adj, CF_adj, xGF_adj), 
                 funs(sum)
                 ) %>% 
    data.frame()
  
  return(merge_return)
  
  }

# Merge skaters and team on-ice metrics
fun.shotmetrics_PP_strength <- function(data, strength, scr_adj_list) {
  
  # Run functions
  skater <- fun.componiceCorsi_PP_strength(data, strength, scr_adj_list)
  team <-   fun.componiceCorsiTeam_PP_strength(data, strength, scr_adj_list)
  
  joined <- left_join(skater, team, by = c("game_id", "Team", "season"))
  
  names(joined) <- c("player", "game_id", "game_date", "season", "Team", "Opponent", "is_home", 
                     
                     "TOI", 
                     "onGF", "onGA", "onSF", "onSA", "onFF", "onFA", "onCF", "onCA", "onxGF", "onxGA",
                     "onGF_adj", "onSF_adj", "onFF_adj", "onCF_adj", "onxGF_adj",
                     
                     "t_TOI", 
                     "t_GF", "t_GA", "t_SF", "t_SA", "t_FF", "t_FA", "t_CF", "t_CA", "t_xGF", "t_xGA", 
                     "t_GF_adj", "t_SF_adj", "t_FF_adj", "t_CF_adj", "t_xGF_adj"
                     )
  
  # Remove goalies
  fun.goalie_remove <- function(data_) {
    
    # Identifies goalies within a given pbp data.frame & returns a data.frame to join for removal
    goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data_$home_goalie, data_$away_goalie))))), 
                                is_goalie = 1)
    
    goalie_return$player <- as.character(goalie_return$player)
    
    return(goalie_return)
    
    }
  goalieremove <- fun.goalie_remove(data_ = data)
  
  all <- joined %>% 
    left_join(., goalieremove, "player") %>% 
    filter(is.na(is_goalie)) %>% 
    select(-c(is_goalie)) %>% 
    arrange(player, game_id) %>% 
    data.frame()
  
  return(all)
  
  }

# Zone Starts
fun.ZS_H_PP_strength <- function(data, venue, strength) {
  
  hold <- data %>% 
    filter(game_strength_state %in% strength,
           event_type == "FAC"
           ) %>%  
    summarise(Team = first(home_team),
              OZS = sum(event_type == "FAC" & home_zone == "Off"), 
              NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
              DZS = sum(event_type == "FAC" & home_zone == "Def")
              )
  
  return(hold)
  
  }
fun.ZS_A_PP_strength <- function(data, venue, strength) {
  
  hold <- data %>% 
    filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = ""),  # reverse strength state entered into function
           event_type == "FAC"
           ) %>%  
    summarise(Team = first(away_team),
              OZS = sum(event_type == "FAC" & home_zone == "Def"), 
              NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
              DZS = sum(event_type == "FAC" & home_zone == "Off")
              )
  
  return(hold)
  
  }
fun.ZS_compute_PP_strength <- function(data, strength) {
  
  print("zone_starts_home", quote = F)
  h1 <- data %>% group_by(game_id, season, home_on_1) %>% fun.ZS_H_PP_strength(., "home_on_1", strength) %>% rename(player = home_on_1) %>% data.frame()
  h2 <- data %>% group_by(game_id, season, home_on_2) %>% fun.ZS_H_PP_strength(., "home_on_2", strength) %>% rename(player = home_on_2) %>% data.frame()
  h3 <- data %>% group_by(game_id, season, home_on_3) %>% fun.ZS_H_PP_strength(., "home_on_3", strength) %>% rename(player = home_on_3) %>% data.frame()
  h4 <- data %>% group_by(game_id, season, home_on_4) %>% fun.ZS_H_PP_strength(., "home_on_4", strength) %>% rename(player = home_on_4) %>% data.frame()
  h5 <- data %>% group_by(game_id, season, home_on_5) %>% fun.ZS_H_PP_strength(., "home_on_5", strength) %>% rename(player = home_on_5) %>% data.frame()
  h6 <- data %>% group_by(game_id, season, home_on_6) %>% fun.ZS_H_PP_strength(., "home_on_6", strength) %>% rename(player = home_on_6) %>% data.frame()
  
  print("zone_starts_away", quote = F)
  a1 <- data %>% group_by(game_id, season, away_on_1) %>% fun.ZS_A_PP_strength(., "away_on_1", strength) %>% rename(player = away_on_1) %>% data.frame()
  a2 <- data %>% group_by(game_id, season, away_on_2) %>% fun.ZS_A_PP_strength(., "away_on_2", strength) %>% rename(player = away_on_2) %>% data.frame()
  a3 <- data %>% group_by(game_id, season, away_on_3) %>% fun.ZS_A_PP_strength(., "away_on_3", strength) %>% rename(player = away_on_3) %>% data.frame()
  a4 <- data %>% group_by(game_id, season, away_on_4) %>% fun.ZS_A_PP_strength(., "away_on_4", strength) %>% rename(player = away_on_4) %>% data.frame()
  a5 <- data %>% group_by(game_id, season, away_on_5) %>% fun.ZS_A_PP_strength(., "away_on_5", strength) %>% rename(player = away_on_5) %>% data.frame()
  a6 <- data %>% group_by(game_id, season, away_on_6) %>% fun.ZS_A_PP_strength(., "away_on_6", strength) %>% rename(player = away_on_6) %>% data.frame()
  
  # Join
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
  
  summed <- merged %>% 
    group_by(player, game_id, season) %>%  
    summarise(Team = first(Team),
              OZS =  sum(OZS), 
              NZS =  sum(NZS), 
              DZS =  sum(DZS)
              ) %>% 
    arrange(player)
  
  return(summed)
  
  }

# Boxscore
fun.counts_PP_strength <- function(data, venue, strength) {
  
  if (venue == "home_team") {
    
    hold <- data %>% 
      filter(game_strength_state %in% strength, 
             event_team == home_team)
    
    # Counts
    counts_1 <- hold %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE")) %>% 
      summarise(Team = first(home_team),
                
                G = sum(event_type == "GOAL"),
                iSF = sum(event_type %in% st.shot_events),
                iCF = sum(event_type %in% st.corsi_events), 
                iFF = sum(event_type %in% st.fenwick_events),
                ixG = sum(na.omit(pred_goal)), 
                
                GIVE_o = sum(event_type == "GIVE" & event_zone == "Off"), 
                GIVE_n = sum(event_type == "GIVE" & event_zone == "Neu"), 
                GIVE_d = sum(event_type == "GIVE" & event_zone == "Def"), 
                
                TAKE_o = sum(event_type == "TAKE" & event_zone == "Off"), 
                TAKE_n = sum(event_type == "TAKE" & event_zone == "Neu"), 
                TAKE_d = sum(event_type == "TAKE" & event_zone == "Def")
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    counts_2 <- hold %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(event_type %in% c("GOAL")) %>% 
      summarise(Team = first(home_team), 
                A1 = sum(event_type == "GOAL")
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    counts_3 <- hold %>% 
      group_by(event_player_3, game_id, season) %>%
      filter(event_type == "GOAL") %>% 
      summarise(Team = first(home_team), 
                A2 = sum(event_type == "GOAL")
                ) %>% 
      rename(player = event_player_3) %>% 
      data.frame()
    
    # Join
    merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      mutate(Points = G + A1 + A2) %>% 
      select(player, game_id, season, Team, 
             G, A1, A2, Points, 
             iSF, iCF, iFF, ixG,
             GIVE_o, GIVE_n, GIVE_d, 
             TAKE_o, TAKE_n, TAKE_d
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
    
    }
  else {
    
    hold <- data %>% 
      filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = ""),  # reverse strength state entered into function
             event_team == away_team)
    
    # Compile
    counts_1 <- hold %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE")) %>% 
      summarise(Team = first(away_team),
                
                G = sum(event_type == "GOAL"),
                iSF = sum(event_type %in% st.shot_events),
                iCF = sum(event_type %in% st.corsi_events),
                iFF = sum(event_type %in% st.fenwick_events), 
                ixG = sum(na.omit(pred_goal)), 
                
                GIVE_o = sum(event_type == "GIVE" & event_zone == "Off"), 
                GIVE_n = sum(event_type == "GIVE" & event_zone == "Neu"), 
                GIVE_d = sum(event_type == "GIVE" & event_zone == "Def"), 
                
                TAKE_o = sum(event_type == "TAKE" & event_zone == "Off"), 
                TAKE_n = sum(event_type == "TAKE" & event_zone == "Neu"), 
                TAKE_d = sum(event_type == "TAKE" & event_zone == "Def")
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    counts_2 <- hold %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(event_type %in% c("GOAL")) %>% 
      summarise(Team = first(away_team), 
                A1 = sum(event_type == "GOAL")
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    counts_3 <- hold %>% 
      group_by(event_player_3, game_id, season) %>%
      filter(event_type == "GOAL") %>% 
      summarise(Team = first(away_team), 
                A2 = sum(event_type == "GOAL")
                ) %>% 
      rename(player = event_player_3) %>% 
      data.frame()
    
    # Join
    merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      mutate(Points = G + A1 + A2) %>% 
      select(player, game_id, season, Team, 
             G, A1, A2, Points, 
             iSF, iCF, iFF, ixG,
             GIVE_o, GIVE_n, GIVE_d, 
             TAKE_o, TAKE_n, TAKE_d
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
    
    }
  
  }
fun.faceoff_PP_strength <- function(data, venue, strength) {
  
  if (venue == "home_team") {
    
    faceoffs <- data %>% 
      filter(game_strength_state %in% strength, 
             event_type == "FAC"
             ) %>% 
      group_by(event_player_2, game_id, season) %>% 
      mutate(FOW = 1 * (home_team == event_team), 
             FOL = 1 * (away_team == event_team)
             ) %>% 
      summarise(Team = first(home_team),
                FOW = sum(FOW), 
                FOL = sum(FOL)
                ) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      rename(player = event_player_2) %>% 
      ungroup() %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(faceoffs)
    
    }
  else {
    
    faceoffs <- data %>% 
      filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = ""),  # reverse strength state entered into function
             event_type == "FAC" 
             ) %>% 
      group_by(event_player_1, game_id, season) %>% 
      mutate(FOW = 1 * (away_team == event_team), 
             FOL = 1 * (home_team == event_team)
             ) %>% 
      summarise(Team = first(away_team),
                FOW = sum(FOW), 
                FOL = sum(FOL)
                ) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      rename(player = event_player_1) %>% 
      ungroup() %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(faceoffs)
    
    }
  
  }
fun.penalty_PP_strength <- function(data, venue, strength) {
  
  if (venue == "home_team") {
    
    pen_1 <- data %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(game_strength_state %in% strength,  
             event_team == home_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(home_team),
                iPENT2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHF_o = sum(event_type == "HIT" & event_zone == "Off"), 
                iHF_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHF_d = sum(event_type == "HIT" & event_zone == "Def")
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    
    pen_2 <- data %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(game_strength_state %in% strength,  
             event_team == away_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(home_team), 
                iPEND2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHA_o = sum(event_type == "HIT" & event_zone == "Def"), 
                iHA_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHA_d = sum(event_type == "HIT" & event_zone == "Off")
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    
    merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      select(player, game_id, season, Team, 
             iPENT2, iPEND2, iPENT5, iPEND5,
             iHF_o, iHF_n, iHF_d, 
             iHA_o, iHA_n, iHA_d
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
    
    }
  else {
    
    pen_1 <- data %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = ""),  # reverse strength state entered into function
             event_team == away_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(away_team),
                iPENT2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHF_o = sum(event_type == "HIT" & event_zone == "Off"), 
                iHF_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHF_d = sum(event_type == "HIT" & event_zone == "Def")
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    pen_2 <- data %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = ""),  # reverse strength state entered into function
             event_team == home_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(away_team),
                iPEND2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHA_o = sum(event_type == "HIT" & event_zone == "Def"), 
                iHA_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHA_d = sum(event_type == "HIT" & event_zone == "Off")
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    
    merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      select(player, game_id, season, Team, 
             iPENT2, iPEND2, iPENT5, iPEND5,
             iHF_o, iHF_n, iHF_d, 
             iHA_o, iHA_n, iHA_d
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
    
    }
  
  }

###  Run all functions
fun.combine_counts_PP_strength <- function(data, strength, scr_adj_list) { 
  
  print(paste("season:", unique(data$season)), quote = F)
  
  # Filter pbp / join prior face time for corsi events
  data <- data %>% 
    filter(game_strength_state %in% c(strength, paste(rev(unlist(strsplit(strength, NULL))), collapse = "")), 
           game_period < 5
           ) %>% 
    select(-c(face_index:shift_length)) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, 
                              ifelse(scradj <= -3, -3, scradj)),
           home_lead_state = ifelse(home_lead < 0, 1, 
                                    ifelse(home_lead == 0, 2, 
                                           ifelse(home_lead > 0, 3, home_lead))), 
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           ) %>% 
    rename(pred_goal = pred_XGB_7)
  
  
  ### Run functions
  shot_metrics <- fun.shotmetrics_PP_strength(data, strength, scr_adj_list)
  zone_start <-   fun.ZS_compute_PP_strength(data, strength)
  
  print("counts", quote = F)
  counts_all <- fun.counts_PP_strength(data, "home_team", strength) %>% 
    rbind(., fun.counts_PP_strength(data, "away_team", strength)) %>% 
    arrange(player, game_id)
  
  print("faceoffs", quote = F)
  faceoff_all <- fun.faceoff_PP_strength(data, "home_team", strength) %>% 
    rbind(., fun.faceoff_PP_strength(data, "away_team", strength)) %>% 
    arrange(player, game_id)
  
  print("penalties", quote = F)
  penalty_all <- fun.penalty_PP_strength(data, "home_team", strength) %>% 
    rbind(., fun.penalty_PP_strength(data, "away_team", strength)) %>% 
    arrange(player, game_id)
  
  
  # Join
  print("combine", quote = F)
  metrics_combined <- shot_metrics %>% 
    left_join(., zone_start,  by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., counts_all,  by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., faceoff_all, by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., penalty_all, by = c("player", "game_id", "season", "Team")) %>% 
    select(player, game_id, game_date, season, Team, Opponent, is_home, 
           TOI, 
           G, A1, A2, Points, 
           iSF, iFF, iCF, ixG,
           GIVE_o:TAKE_d,
           iHF_o:iHA_d,
           FOW, FOL, 
           OZS, NZS, DZS, 
           iPENT2, iPEND2, iPENT5, iPEND5, 
           onGF:onxGF_adj, 
           t_TOI:t_xGF_adj
           ) %>% 
    # Patch for if no PP goals were scored
    mutate(A1 = ifelse(is.na(A1), 0, A1), 
           A2 = ifelse(is.na(A2), 0, A2)
           ) %>% 
    arrange(player, game_id) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
    mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
    data.frame()
  
  return(metrics_combined)
  
  }


#########################


## ------------------- ##
##   4v5, etc. Games   ##
## ------------------- ##

#########################

# Skaters: On Ice Corsi For / Against, TOI, Team - per game
fun.oniceCorsiH_SH_strength <- function(data, venue, strength, scr_adj_list) {
  
  hold <- data %>% 
    filter(game_strength_state %in% strength) %>% 
    summarise(TOI = sum(event_length) / 60, 
              
              Team = first(home_team), 
              Opponent = first(away_team), 
              is_home = 1, 
              
              GF =  sum(event_type == "GOAL" & event_team == home_team),
              GA =  sum(event_type == "GOAL" & event_team == away_team), 
              SF =  sum(event_type %in% st.shot_events & event_team == home_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == away_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == home_team),
              FA =  sum(event_type %in% st.fenwick_events & event_team == away_team), 
              CF =  sum(event_type %in% st.corsi_events & event_team == home_team),
              CA =  sum(event_type %in% st.corsi_events & event_team == away_team), 
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              
              GA_adj =  sum((event_type == "GOAL" & event_team == away_team) * scr_adj_list$away_goal_adj[home_lead_state]), 
              SA_adj =  sum((event_type %in% st.shot_events & event_team == away_team) * scr_adj_list$away_shot_adj[home_lead_state]), 
              FA_adj =  sum((event_type %in% st.fenwick_events & event_team == away_team) * scr_adj_list$away_fenwick_adj[home_lead_state]), 
              CA_adj =  sum((event_type %in% st.corsi_events & event_team == away_team) * scr_adj_list$away_corsi_adj[home_lead_state]), 
              xGA_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * scr_adj_list$away_xG_adj[home_lead_state]))
              )
  
  return(hold)
  
  }
fun.oniceCorsiA_SH_strength <- function(data, venue, strength, scr_adj_list) {
  
  hold <- data %>% 
    filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = "")) %>%  # reverse strength state entered into function
    summarise(TOI = sum(event_length) / 60, 
              
              Team = first(away_team), 
              Opponent = first(home_team), 
              is_home = 0, 
              
              GF =  sum(event_type == "GOAL" & event_team == away_team),
              GA =  sum(event_type == "GOAL" & event_team == home_team), 
              SF =  sum(event_type %in% st.shot_events & event_team == away_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == home_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == away_team),
              FA =  sum(event_type %in% st.fenwick_events & event_team == home_team), 
              CF =  sum(event_type %in% st.corsi_events & event_team == away_team),
              CA =  sum(event_type %in% st.corsi_events & event_team == home_team), 
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              
              GA_adj =  sum((event_type == "GOAL" & event_team == home_team) * scr_adj_list$home_goal_adj[home_lead_state]), 
              SA_adj =  sum((event_type %in% st.shot_events & event_team == home_team) * scr_adj_list$home_shot_adj[home_lead_state]), 
              FA_adj =  sum((event_type %in% st.fenwick_events & event_team == home_team) * scr_adj_list$home_fenwick_adj[home_lead_state]), 
              CA_adj =  sum((event_type %in% st.corsi_events & event_team == home_team) * scr_adj_list$home_corsi_adj[home_lead_state]), 
              xGA_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * scr_adj_list$home_xG_adj[home_lead_state]))
              )
  
  return(hold)
  
  }
fun.componiceCorsi_SH_strength <- function(data, strength, scr_adj_list) {
  
  print("on_ice_home", quote = F)
  h1 <- data %>% group_by(game_id, game_date, season, home_on_1, home_team) %>% fun.oniceCorsiH_SH_strength(., "home_on_1", strength, scr_adj_list) %>% rename(player = home_on_1) %>% data.frame()
  h2 <- data %>% group_by(game_id, game_date, season, home_on_2, home_team) %>% fun.oniceCorsiH_SH_strength(., "home_on_2", strength, scr_adj_list) %>% rename(player = home_on_2) %>% data.frame()
  h3 <- data %>% group_by(game_id, game_date, season, home_on_3, home_team) %>% fun.oniceCorsiH_SH_strength(., "home_on_3", strength, scr_adj_list) %>% rename(player = home_on_3) %>% data.frame()
  h4 <- data %>% group_by(game_id, game_date, season, home_on_4, home_team) %>% fun.oniceCorsiH_SH_strength(., "home_on_4", strength, scr_adj_list) %>% rename(player = home_on_4) %>% data.frame()
  h5 <- data %>% group_by(game_id, game_date, season, home_on_5, home_team) %>% fun.oniceCorsiH_SH_strength(., "home_on_5", strength, scr_adj_list) %>% rename(player = home_on_5) %>% data.frame()
  h6 <- data %>% group_by(game_id, game_date, season, home_on_6, home_team) %>% fun.oniceCorsiH_SH_strength(., "home_on_6", strength, scr_adj_list) %>% rename(player = home_on_6) %>% data.frame()
  
  print("on_ice_away", quote = F)
  a1 <- data %>% group_by(game_id, game_date, season, away_on_1, away_team) %>% fun.oniceCorsiA_SH_strength(., "away_on_1", strength, scr_adj_list) %>% rename(player = away_on_1) %>% data.frame()
  a2 <- data %>% group_by(game_id, game_date, season, away_on_2, away_team) %>% fun.oniceCorsiA_SH_strength(., "away_on_2", strength, scr_adj_list) %>% rename(player = away_on_2) %>% data.frame()
  a3 <- data %>% group_by(game_id, game_date, season, away_on_3, away_team) %>% fun.oniceCorsiA_SH_strength(., "away_on_3", strength, scr_adj_list) %>% rename(player = away_on_3) %>% data.frame()
  a4 <- data %>% group_by(game_id, game_date, season, away_on_4, away_team) %>% fun.oniceCorsiA_SH_strength(., "away_on_4", strength, scr_adj_list) %>% rename(player = away_on_4) %>% data.frame()
  a5 <- data %>% group_by(game_id, game_date, season, away_on_5, away_team) %>% fun.oniceCorsiA_SH_strength(., "away_on_5", strength, scr_adj_list) %>% rename(player = away_on_5) %>% data.frame()
  a6 <- data %>% group_by(game_id, game_date, season, away_on_6, away_team) %>% fun.oniceCorsiA_SH_strength(., "away_on_6", strength, scr_adj_list) %>% rename(player = away_on_6) %>% data.frame()
  
  
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
  
  merge_return <- merged %>% 
    group_by(player, game_id, game_date, season) %>%  
    summarise(Team =     first(Team), 
              Opponent = first(Opponent), 
              is_home =  first(is_home), 
              TOI =     sum(TOI), 
              
              GF =       sum(GF), 
              GA =       sum(GA), 
              SF =       sum(SF), 
              SA =       sum(SA), 
              FF =       sum(FF), 
              FA =       sum(FA), 
              CF =       sum(CF), 
              CA =       sum(CA), 
              xGF =      sum(xGF), 
              xGA =      sum(xGA), 
              
              GA_adj =   sum(GA_adj), 
              SA_adj =   sum(SA_adj), 
              FA_adj =   sum(FA_adj), 
              CA_adj =   sum(CA_adj), 
              xGA_adj =  sum(xGA_adj)
              ) %>% 
    filter(!is.na(player)) %>% 
    data.frame()
  
  return(merge_return)
  
  }

# Teams: On Ice Corsi For / Against, TOI - per game
fun.oniceTeamCorsiH_SH_strength <- function(data, strength, scr_adj_list) {
  
  hold <- data %>% 
    filter(game_strength_state %in% strength) %>% 
    summarise(TOI = sum(event_length) / 60, 
              Team = first(home_team), 
              
              GF =  sum(event_type == "GOAL" & event_team == home_team),
              GA =  sum(event_type == "GOAL" & event_team == away_team), 
              SF =  sum(event_type %in% st.shot_events & event_team == home_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == away_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == home_team),
              FA =  sum(event_type %in% st.fenwick_events & event_team == away_team), 
              CF =  sum(event_type %in% st.corsi_events & event_team == home_team),
              CA =  sum(event_type %in% st.corsi_events & event_team == away_team), 
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              
              GA_adj =  sum((event_type == "GOAL" & event_team == away_team) * scr_adj_list$away_goal_adj[home_lead_state]), 
              SA_adj =  sum((event_type %in% st.shot_events & event_team == away_team) * scr_adj_list$away_shot_adj[home_lead_state]), 
              FA_adj =  sum((event_type %in% st.fenwick_events & event_team == away_team) * scr_adj_list$away_fenwick_adj[home_lead_state]), 
              CA_adj =  sum((event_type %in% st.corsi_events & event_team == away_team) * scr_adj_list$away_corsi_adj[home_lead_state]), 
              xGA_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * scr_adj_list$away_xG_adj[home_lead_state]))
              )
  
  return(hold)
  
  }
fun.oniceTeamCorsiA_SH_strength <- function(data, strength, scr_adj_list) {
  
  hold <- data %>% 
    filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = "")) %>%  # reverse strength state entered into function
    summarise(TOI = sum(event_length) / 60, 
              Team = first(away_team), 
              
              GF =  sum(event_type == "GOAL" & event_team == away_team),
              GA =  sum(event_type == "GOAL" & event_team == home_team), 
              SF =  sum(event_type %in% st.shot_events & event_team == away_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == home_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == away_team),
              FA =  sum(event_type %in% st.fenwick_events & event_team == home_team), 
              CF =  sum(event_type %in% st.corsi_events & event_team == away_team),
              CA =  sum(event_type %in% st.corsi_events & event_team == home_team), 
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              
              GA_adj =  sum((event_type == "GOAL" & event_team == home_team) * scr_adj_list$home_goal_adj[home_lead_state]), 
              SA_adj =  sum((event_type %in% st.shot_events & event_team == home_team) * scr_adj_list$home_shot_adj[home_lead_state]), 
              FA_adj =  sum((event_type %in% st.fenwick_events & event_team == home_team) * scr_adj_list$home_fenwick_adj[home_lead_state]), 
              CA_adj =  sum((event_type %in% st.corsi_events & event_team == home_team) * scr_adj_list$home_corsi_adj[home_lead_state]), 
              xGA_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * scr_adj_list$home_xG_adj[home_lead_state]))
              )
  
  return(hold)
  
  }
fun.componiceCorsiTeam_SH_strength <- function(data, strength, scr_adj_list) {
  
  print("team_on_ice_home", quote = F)
  h_df <- data %>% 
    group_by(game_id, season, home_team) %>% 
    fun.oniceTeamCorsiH_SH_strength(., strength, scr_adj_list) 
  
  print("team_on_ice_away", quote = F)
  a_df <- data %>% 
    group_by(game_id, season, away_team) %>% 
    fun.oniceTeamCorsiA_SH_strength(., strength, scr_adj_list) 
  
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h_df, a_df))
  
  merge_return <- merged %>% 
    group_by(Team, game_id, season) %>% 
    summarise_at(vars(TOI, 
                      GF, GA, SF, SA, FF, FA, CF, CA, xGF, xGA, 
                      GA_adj, SA_adj, FA_adj, CA_adj, xGA_adj), 
                 funs(sum)
                 ) %>% 
    data.frame()
  
  return(merge_return)
  
  }

# Merge skaters and team on-ice metrics
fun.shotmetrics_SH_strength <- function(data, strength, scr_adj_list) {
  
  # Run functions
  skater <- fun.componiceCorsi_SH_strength(data, strength, scr_adj_list)
  team <-   fun.componiceCorsiTeam_SH_strength(data, strength, scr_adj_list)
  
  joined <- left_join(skater, team, by = c("game_id", "Team", "season"))
  
  names(joined) <- c("player", "game_id", "game_date", "season", "Team", "Opponent", "is_home", 
                     
                     "TOI", 
                     "onGF", "onGA", "onSF", "onSA", "onFF", "onFA", "onCF", "onCA", "onxGF", "onxGA",
                     "onGA_adj", "onSA_adj", "onFA_adj", "onCA_adj", "onxGA_adj", 
                     
                     "t_TOI", 
                     "t_GF", "t_GA", "t_SF", "t_SA", "t_FF", "t_FA", "t_CF", "t_CA", "t_xGF", "t_xGA", 
                     "t_GA_adj", "t_SA_adj", "t_FA_adj", "t_CA_adj", "t_xGA_adj"
                     )
  
  # Remove goalies
  fun.goalie_remove <- function(data_) {
    
    # Identifies goalies within a given pbp data.frame & returns a data.frame to join for removal
    goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data_$home_goalie, data_$away_goalie))))), 
                                is_goalie = 1)
    
    goalie_return$player <- as.character(goalie_return$player)
    
    return(goalie_return)
    
    }
  goalieremove <- fun.goalie_remove(data_ = data)
  
  all <- joined %>% 
    left_join(., goalieremove, "player") %>% 
    filter(is.na(is_goalie)) %>% 
    select(-c(is_goalie)) %>% 
    arrange(player, game_id) %>% 
    data.frame()
  
  return(all)
  
  }

# Zone Starts
fun.ZS_H_SH_strength <- function(data, venue, strength) {
  
  hold <- data %>% 
    filter(game_strength_state %in% strength, 
           event_type == "FAC"
           ) %>% 
    summarise(Team = first(home_team),
              OZS = sum(event_type == "FAC" & home_zone == "Off"), 
              NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
              DZS = sum(event_type == "FAC" & home_zone == "Def")
              )
  
  return(hold)
  
  }
fun.ZS_A_SH_strength <- function(data, venue, strength) {
  
  hold <- data %>% 
    filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = ""),  # reverse strength state entered into function
           event_type == "FAC"
           ) %>%  
    summarise(Team = first(away_team),
              OZS = sum(event_type == "FAC" & home_zone == "Def"), 
              NZS = sum(event_type == "FAC" & home_zone == "Neu"), 
              DZS = sum(event_type == "FAC" & home_zone == "Off")
              )
  
  return(hold)
  
  }
fun.ZS_compute_SH_strength <- function(data, strength) {
  
  print("zone_starts_home", quote = F)
  h1 <- data %>% group_by(game_id, season, home_on_1) %>% fun.ZS_H_SH_strength(., "home_on_1", strength) %>% rename(player = home_on_1) %>% data.frame()
  h2 <- data %>% group_by(game_id, season, home_on_2) %>% fun.ZS_H_SH_strength(., "home_on_2", strength) %>% rename(player = home_on_2) %>% data.frame()
  h3 <- data %>% group_by(game_id, season, home_on_3) %>% fun.ZS_H_SH_strength(., "home_on_3", strength) %>% rename(player = home_on_3) %>% data.frame()
  h4 <- data %>% group_by(game_id, season, home_on_4) %>% fun.ZS_H_SH_strength(., "home_on_4", strength) %>% rename(player = home_on_4) %>% data.frame()
  h5 <- data %>% group_by(game_id, season, home_on_5) %>% fun.ZS_H_SH_strength(., "home_on_5", strength) %>% rename(player = home_on_5) %>% data.frame()
  h6 <- data %>% group_by(game_id, season, home_on_6) %>% fun.ZS_H_SH_strength(., "home_on_6", strength) %>% rename(player = home_on_6) %>% data.frame()
  
  print("zone_starts_away", quote = F)
  a1 <- data %>% group_by(game_id, season, away_on_1) %>% fun.ZS_A_SH_strength(., "away_on_1", strength) %>% rename(player = away_on_1) %>% data.frame()
  a2 <- data %>% group_by(game_id, season, away_on_2) %>% fun.ZS_A_SH_strength(., "away_on_2", strength) %>% rename(player = away_on_2) %>% data.frame()
  a3 <- data %>% group_by(game_id, season, away_on_3) %>% fun.ZS_A_SH_strength(., "away_on_3", strength) %>% rename(player = away_on_3) %>% data.frame()
  a4 <- data %>% group_by(game_id, season, away_on_4) %>% fun.ZS_A_SH_strength(., "away_on_4", strength) %>% rename(player = away_on_4) %>% data.frame()
  a5 <- data %>% group_by(game_id, season, away_on_5) %>% fun.ZS_A_SH_strength(., "away_on_5", strength) %>% rename(player = away_on_5) %>% data.frame()
  a6 <- data %>% group_by(game_id, season, away_on_6) %>% fun.ZS_A_SH_strength(., "away_on_6", strength) %>% rename(player = away_on_6) %>% data.frame()
  
  # Join
  merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
  
  summed <- merged %>% 
    group_by(player, game_id, season) %>%  
    summarise(Team = first(Team),
              OZS =  sum(OZS), 
              NZS =  sum(NZS), 
              DZS =  sum(DZS)
              ) %>% 
    arrange(player)
  
  return(summed)
  
  }

# Boxscore
fun.counts_SH_strength <- function(data, venue, strength) {
  
  if (venue == "home_team") {
    
    hold <- data %>% 
      filter(game_strength_state %in% strength,  
             event_team == home_team)
    
    # Counts
    counts_1 <- hold %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE")) %>% 
      summarise(Team = first(home_team),
                G = sum(event_type == "GOAL"),
                iSF = sum(event_type %in% st.shot_events),
                iCF = sum(event_type %in% st.corsi_events), 
                iFF = sum(event_type %in% st.fenwick_events),
                ixG = sum(na.omit(pred_goal)), 
                
                GIVE_o = sum(event_type == "GIVE" & event_zone == "Off"), 
                GIVE_n = sum(event_type == "GIVE" & event_zone == "Neu"), 
                GIVE_d = sum(event_type == "GIVE" & event_zone == "Def"), 
                
                TAKE_o = sum(event_type == "TAKE" & event_zone == "Off"), 
                TAKE_n = sum(event_type == "TAKE" & event_zone == "Neu"), 
                TAKE_d = sum(event_type == "TAKE" & event_zone == "Def")
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    counts_2 <- hold %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(event_type %in% c("GOAL")) %>% 
      summarise(Team = first(home_team), 
                A1 = sum(event_type == "GOAL") 
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    counts_3 <- hold %>% 
      group_by(event_player_3, game_id, season) %>%
      filter(event_type == "GOAL") %>% 
      summarise(Team = first(home_team), 
                A2 = sum(event_type == "GOAL") 
                ) %>% 
      rename(player = event_player_3) %>% 
      data.frame()
    
    # Join
    merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate(Points = G + A1 + A2) %>% 
      select(player, game_id, season, Team, 
             G, A1, A2, Points, 
             iSF, iCF, iFF, ixG, 
             GIVE_o, GIVE_n, GIVE_d, 
             TAKE_o, TAKE_n, TAKE_d
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
    
    }
  else {
    
    hold <- data %>% 
      filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = ""),  # reverse strength state entered into function
             event_team == away_team)
    
    # Compile
    counts_1 <- hold %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE")) %>% 
      summarise(Team = first(away_team),
                G = sum(event_type == "GOAL"),
                iSF = sum(event_type %in% st.shot_events),
                iCF = sum(event_type %in% st.corsi_events),
                iFF = sum(event_type %in% st.fenwick_events), 
                ixG = sum(na.omit(pred_goal)), 
                
                GIVE_o = sum(event_type == "GIVE" & event_zone == "Off"), 
                GIVE_n = sum(event_type == "GIVE" & event_zone == "Neu"), 
                GIVE_d = sum(event_type == "GIVE" & event_zone == "Def"), 
                
                TAKE_o = sum(event_type == "TAKE" & event_zone == "Off"), 
                TAKE_n = sum(event_type == "TAKE" & event_zone == "Neu"), 
                TAKE_d = sum(event_type == "TAKE" & event_zone == "Def")
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    counts_2 <- hold %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(event_type %in% c("GOAL")) %>% 
      summarise(Team = first(away_team), 
                A1 = sum(event_type == "GOAL")
      ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    counts_3 <- hold %>% 
      group_by(event_player_3, game_id, season) %>%
      filter(event_type == "GOAL") %>% 
      summarise(Team = first(away_team), 
                A2 = sum(event_type == "GOAL")
                ) %>% 
      rename(player = event_player_3) %>% 
      data.frame()
    
    # Join
    merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate(Points = G + A1 + A2) %>% 
      select(player, game_id, season, Team, 
             G, A1, A2, Points,
             iSF, iCF, iFF, ixG,
             GIVE_o, GIVE_n, GIVE_d, 
             TAKE_o, TAKE_n, TAKE_d
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
    
    }
  
  }
fun.faceoff_SH_strength <- function(data, venue, strength) {
  
  if (venue == "home_team") {
    
    faceoffs <- data %>% 
      filter(game_strength_state %in% strength, 
             event_type == "FAC"
             ) %>% 
      group_by(event_player_2, game_id, season) %>% 
      mutate(FOW = 1 * (home_team == event_team), 
             FOL = 1 * (away_team == event_team)
             ) %>% 
      summarise(Team = first(home_team),
                FOW = sum(FOW), 
                FOL = sum(FOL)
                ) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      rename(player = event_player_2) %>% 
      ungroup() %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(faceoffs)
    
    }
  else {
    
    faceoffs <- data %>% 
      filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = ""),  # reverse strength state entered into function
             event_type == "FAC"
             ) %>% 
      group_by(event_player_1, game_id, season) %>% 
      mutate(FOW = 1 * (away_team == event_team), 
             FOL = 1 * (home_team == event_team)
             ) %>% 
      summarise(Team = first(away_team),
                FOW = sum(FOW), 
                FOL = sum(FOL)
                ) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      rename(player = event_player_1) %>% 
      ungroup() %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(faceoffs)
    
    }
  
  }
fun.penalty_SH_strength <- function(data, venue, strength) {
  
  if (venue == "home_team") {
    
    pen_1 <- data %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(game_strength_state %in% strength,  
             event_team == home_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(home_team),
                iPENT2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHF_o = sum(event_type == "HIT" & event_zone == "Off"), 
                iHF_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHF_d = sum(event_type == "HIT" & event_zone == "Def")
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    
    pen_2 <- data %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(game_strength_state %in% strength,  
             event_team == away_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(home_team), 
                iPEND2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHA_o = sum(event_type == "HIT" & event_zone == "Def"), 
                iHA_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHA_d = sum(event_type == "HIT" & event_zone == "Off"), 
                
                iBLK = sum(event_type == "BLOCK")
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    
    merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      select(player, game_id, season, Team, 
             iPENT2, iPEND2, iPENT5, iPEND5, 
             iBLK,
             iHF_o, iHF_n, iHF_d, 
             iHA_o, iHA_n, iHA_d
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
    
    }
  else {
    
    pen_1 <- data %>% 
      group_by(event_player_1, game_id, season) %>% 
      filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = ""),  # reverse strength state entered into function
             event_team == away_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(away_team),
                iPENT2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPENT5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHF_o = sum(event_type == "HIT" & event_zone == "Off"), 
                iHF_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHF_d = sum(event_type == "HIT" & event_zone == "Def")
                ) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    pen_2 <- data %>% 
      group_by(event_player_2, game_id, season) %>% 
      filter(game_strength_state %in% paste(rev(unlist(strsplit(strength, NULL))), collapse = ""),  # reverse strength state entered into function
             event_team == home_team, 
             event_type %in% c("PENL", "HIT", "BLOCK")
             ) %>% 
      summarise(Team = first(away_team),
                iPEND2 = sum(na.omit(1 * (event_type == "PENL") +
                                       1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                       1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                       1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                iPEND5 = sum(na.omit(event_type == "PENL" & grepl("fighting|major", tolower(event_detail)) == TRUE)), 
                
                iHA_o = sum(event_type == "HIT" & event_zone == "Def"), 
                iHA_n = sum(event_type == "HIT" & event_zone == "Neu"), 
                iHA_d = sum(event_type == "HIT" & event_zone == "Off"), 
                
                iBLK = sum(event_type == "BLOCK")
                ) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    
    merged <- Reduce(function(...) merge(..., all=TRUE), list(pen_1, pen_2))
    
    joined <- merged %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
      select(player, game_id, season, Team, 
             iPENT2, iPEND2, iPENT5, iPEND5, 
             iBLK, 
             iHF_o, iHF_n, iHF_d, 
             iHA_o, iHA_n, iHA_d
             ) %>% 
      arrange(player, game_id) %>% 
      data.frame()
    
    return(joined)
    
    }
  
  }

###  Run all functions
fun.combine_counts_SH_strength <- function(data, strength, scr_adj_list) { 
  
  print(paste("season:", unique(data$season)), quote = F)
  
  # Filter pbp / join prior face time for corsi events
  data <- data %>% 
    filter(game_strength_state %in% st.pp_strength, 
           game_period < 5
           ) %>% 
    select(-c(face_index:shift_length)) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, 
                              ifelse(scradj <= -3, -3, scradj)),
           home_lead_state = ifelse(home_lead < 0, 1, 
                                    ifelse(home_lead == 0, 2, 
                                           ifelse(home_lead > 0, 3, home_lead))), 
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           ) %>% 
    rename(pred_goal = pred_XGB_7)
  
  
  ### Run functions
  shot_metrics <- fun.shotmetrics_SH_strength(data, strength, scr_adj_list)
  zone_start <-   fun.ZS_compute_SH_strength(data, strength)
  
  print("counts", quote = F)
  counts_all <- fun.counts_SH_strength(data, "home_team", strength) %>% 
    rbind(., fun.counts_SH_strength(data, "away_team", strength)) %>% 
    arrange(player, game_id)
  
  print("faceoffs", quote = F)
  faceoff_all <- fun.faceoff_SH_strength(data, "home_team", strength) %>% 
    rbind(., fun.faceoff_SH_strength(data, "away_team", strength)) %>% 
    arrange(player, game_id)
  
  print("penalties", quote = F)
  penalty_all <- fun.penalty_SH_strength(data, "home_team", strength) %>% 
    rbind(., fun.penalty_SH_strength(data, "away_team", strength)) %>% 
    arrange(player, game_id)
  
  
  # Join
  print("combine", quote = F)
  metrics_combined <- shot_metrics %>% 
    left_join(., zone_start,  by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., counts_all,  by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., faceoff_all, by = c("player", "game_id", "season", "Team")) %>% 
    left_join(., penalty_all, by = c("player", "game_id", "season", "Team")) %>% 
    select(player, game_id, game_date, season, Team, Opponent, is_home, TOI, 
           G, A1, A2, Points, 
           iSF, iFF, iCF, ixG,
           iBLK,  
           GIVE_o:TAKE_d, 
           iHF_o:iHA_d, 
           FOW, FOL, 
           OZS, NZS, DZS, 
           iPENT2, iPEND2, iPENT5, iPEND5, 
           onGF:onxGA_adj, 
           t_TOI:t_xGA_adj
           ) %>% 
    # Patch for if no SH goals were scored
    mutate(A1 = ifelse(is.na(A1), 0, A1), 
           A2 = ifelse(is.na(A2), 0, A2)
           ) %>% 
    arrange(player, game_id) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
    mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
    data.frame()
  
  
  return(metrics_combined)
  
  }


#########################



## ------------------------------- ##
##   Rel_TM - TOI Together Games   ##
## ------------------------------- ##

#####################################

# Combos: On Ice Corsi For / Against, TOI, Team - per game
fun.QoT_H <- function(data) {
  
  hold_player <- data %>% 
    summarise(TOI = sum(event_length) / 60)
  
  return(hold_player)

  }
fun.QoT_A <- function(data) {
  
  hold_player <- data %>% 
    summarise(TOI = sum(event_length) / 60)
  
  return(hold_player)

  }

# For all on-ice pairs
fun.event_playerH <- function(data, player) {
  
  if (player == "home_on_1") {
    
    h1 <- data %>% 
      group_by(game_id, game_date, season, home_on_1, home_on_2, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_2) %>% 
      data.frame()
    
    h2 <- data %>% 
      group_by(game_id, game_date, season, home_on_1, home_on_3, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_3) %>% 
      data.frame()
    
    h3 <- data %>% 
      group_by(game_id, game_date, season, home_on_1, home_on_4, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_4) %>% 
      data.frame()
    
    h4 <- data %>% 
      group_by(game_id, game_date, season, home_on_1, home_on_5, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_5) %>% 
      data.frame()
    
    h5 <- data %>% 
      group_by(game_id, game_date, season, home_on_1, home_on_6, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_6) %>% 
      data.frame()
    
    
    hbind <- h1 %>% 
      rbind(., h2, h3, h4, h5) %>% 
      group_by(player, teammate, game_id, game_date, season, home_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      filter(TOI > 0) %>% 
      data.frame()
    
    return(hbind)
  
    }
  else if (player == "home_on_2") {
    
    h1 <- data %>% 
      group_by(game_id, game_date, season, home_on_2, home_on_1, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_1) %>% 
      data.frame()
    
    h2 <- data %>% 
      group_by(game_id, game_date, season, home_on_2, home_on_3, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_3) %>% 
      data.frame()
    
    h3 <- data %>% 
      group_by(game_id, game_date, season, home_on_2, home_on_4, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_4) %>% 
      data.frame()
    
    h4 <- data %>% 
      group_by(game_id, game_date, season, home_on_2, home_on_5, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_5) %>% 
      data.frame()
    
    h5 <- data %>% 
      group_by(game_id, game_date, season, home_on_2, home_on_6, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_6) %>% 
      data.frame()
    
    
    hbind <- h1 %>% 
      rbind(., h2, h3, h4, h5) %>% 
      group_by(player, teammate, game_id, game_date, season, home_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      filter(TOI > 0) %>% 
      data.frame()
    
    return(hbind)
  
    }
  else if (player == "home_on_3") {
    
    h1 <- data %>% 
      group_by(game_id, game_date, season, home_on_3, home_on_1, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_1) %>% 
      data.frame()
    
    h2 <- data %>% 
      group_by(game_id, game_date, season, home_on_3, home_on_2, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_2) %>% 
      data.frame()
    
    h3 <- data %>% 
      group_by(game_id, game_date, season, home_on_3, home_on_4, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_4) %>% 
      data.frame()
    
    h4 <- data %>% 
      group_by(game_id, game_date, season, home_on_3, home_on_5, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_5) %>% 
      data.frame()
    
    h5 <- data %>% 
      group_by(game_id, game_date, season, home_on_3, home_on_6, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_6) %>% 
      data.frame()
    
    
    hbind <- h1 %>% 
      rbind(., h2, h3, h4, h5) %>% 
      group_by(player, teammate, game_id, game_date, season, home_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      filter(TOI > 0) %>% 
      data.frame()
    
    return(hbind)
  
    }
  else if (player == "home_on_4") {
    
    h1 <- data %>% 
      group_by(game_id, game_date, season, home_on_4, home_on_1, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_1) %>% 
      data.frame()
    
    h2 <- data %>% 
      group_by(game_id, game_date, season, home_on_4, home_on_2, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_2) %>% 
      data.frame()
    
    h3 <- data %>% 
      group_by(game_id, game_date, season, home_on_4, home_on_3, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_3) %>% 
      data.frame()
    
    h4 <- data %>% 
      group_by(game_id, game_date, season, home_on_4, home_on_5, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_5) %>% 
      data.frame()
    
    h5 <- data %>% 
      group_by(game_id, game_date, season, home_on_4, home_on_6, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_6) %>% 
      data.frame()
    
    
    hbind <- h1 %>% 
      rbind(., h2, h3, h4, h5) %>% 
      group_by(player, teammate, game_id, game_date, season, home_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      filter(TOI > 0) %>% 
      data.frame()
    
    return(hbind)
  
    }
  else if (player == "home_on_5") {
    
    h1 <- data %>% 
      group_by(game_id, game_date, season, home_on_5, home_on_1, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_1) %>% 
      data.frame()
    
    h2 <- data %>% 
      group_by(game_id, game_date, season, home_on_5, home_on_2, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_2) %>% 
      data.frame()
    
    h3 <- data %>% 
      group_by(game_id, game_date, season, home_on_5, home_on_3, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_3) %>% 
      data.frame()
    
    h4 <- data %>% 
      group_by(game_id, game_date, season, home_on_5, home_on_4, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_4) %>% 
      data.frame()
    
    h5 <- data %>% 
      group_by(game_id, game_date, season, home_on_5, home_on_6, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_6) %>% 
      data.frame()
    
    
    hbind <- h1 %>% 
      rbind(., h2, h3, h4, h5) %>% 
      group_by(player, teammate, game_id, game_date, season, home_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      filter(TOI > 0) %>% 
      data.frame()
    
    return(hbind)
  
    }
  else if (player == "home_on_6") {
    
    h1 <- data %>% 
      group_by(game_id, game_date, season, home_on_6, home_on_1, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_1) %>% 
      data.frame()
    
    h2 <- data %>% 
      group_by(game_id, game_date, season, home_on_6, home_on_2, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_2) %>% 
      data.frame()
    
    h3 <- data %>% 
      group_by(game_id, game_date, season, home_on_6, home_on_3, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_3) %>% 
      data.frame()
    
    h4 <- data %>% 
      group_by(game_id, game_date, season, home_on_6, home_on_4, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_4) %>% 
      data.frame()
    
    h5 <- data %>% 
      group_by(game_id, game_date, season, home_on_6, home_on_5, home_team) %>% 
      fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_5) %>% 
      data.frame()
    
    
    hbind <- h1 %>% 
      rbind(., h2, h3, h4, h5) %>% 
      group_by(player, teammate, game_id, game_date, season, home_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      filter(TOI > 0) %>% 
      data.frame()
    
    return(hbind)
  
    }
  
  }
fun.event_playerA <- function(data, player) {
  
  if (player == "away_on_1") {
    
    a1 <- data %>% 
      group_by(game_id, game_date, season, away_on_1, away_on_2, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_2) %>% 
      data.frame()
    
    a2 <- data %>% 
      group_by(game_id, game_date, season, away_on_1, away_on_3, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_3) %>% 
      data.frame()
    
    a3 <- data %>% 
      group_by(game_id, game_date, season, away_on_1, away_on_4, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_4) %>% 
      data.frame()
    
    a4 <- data %>% 
      group_by(game_id, game_date, season, away_on_1, away_on_5, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_5) %>% 
      data.frame()
    
    a5 <- data %>% 
      group_by(game_id, game_date, season, away_on_1, away_on_6, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_6) %>% 
      data.frame()
    
    
    abind <- a1 %>% 
      rbind(., a2, a3, a4, a5) %>% 
      group_by(player, teammate, game_id, game_date, season, away_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>% 
      filter(TOI > 0) %>% 
      data.frame()
    
    return(abind)
  
    }
  else if (player == "away_on_2") {
    
    a1 <- data %>% 
      group_by(game_id, game_date, season, away_on_2, away_on_1, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_1) %>% 
      data.frame()
    
    a2 <- data %>% 
      group_by(game_id, game_date, season, away_on_2, away_on_3, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_3) %>% 
      data.frame()
    
    a3 <- data %>% 
      group_by(game_id, game_date, season, away_on_2, away_on_4, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_4) %>% 
      data.frame()
    
    a4 <- data %>% 
      group_by(game_id, game_date, season, away_on_2, away_on_5, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_5) %>% 
      data.frame()
    
    a5 <- data %>% 
      group_by(game_id, game_date, season, away_on_2, away_on_6, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_6) %>% 
      data.frame()
    
    
    abind <- a1 %>% 
      rbind(., a2, a3, a4, a5) %>% 
      group_by(player, teammate, game_id, game_date, season, away_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>% 
      filter(TOI > 0) %>% 
      data.frame()
    
    return(abind)
  
    }
  else if (player == "away_on_3") {
    
    a1 <- data %>% 
      group_by(game_id, game_date, season, away_on_3, away_on_1, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_1) %>% 
      data.frame()
    
    a2 <- data %>% 
      group_by(game_id, game_date, season, away_on_3, away_on_2, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_2) %>% 
      data.frame()
    
    a3 <- data %>% 
      group_by(game_id, game_date, season, away_on_3, away_on_4, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_4) %>% 
      data.frame()
    
    a4 <- data %>% 
      group_by(game_id, game_date, season, away_on_3, away_on_5, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_5) %>% 
      data.frame()
    
    a5 <- data %>% 
      group_by(game_id, game_date, season, away_on_3, away_on_6, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_6) %>% 
      data.frame()
    
    
    abind <- a1 %>% 
      rbind(., a2, a3, a4, a5) %>% 
      group_by(player, teammate, game_id, game_date, season, away_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>% 
      filter(TOI > 0) %>% 
      data.frame()
    
    return(abind)
  
    }
  else if (player == "away_on_4") {
    
    a1 <- data %>% 
      group_by(game_id, game_date, season, away_on_4, away_on_1, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_1) %>% 
      data.frame()
    
    a2 <- data %>% 
      group_by(game_id, game_date, season, away_on_4, away_on_2, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_2) %>% 
      data.frame()
    
    a3 <- data %>% 
      group_by(game_id, game_date, season, away_on_4, away_on_3, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_3) %>% 
      data.frame()
    
    a4 <- data %>% 
      group_by(game_id, game_date, season, away_on_4, away_on_5, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_5) %>% 
      data.frame()
    
    a5 <- data %>% 
      group_by(game_id, game_date, season, away_on_4, away_on_6, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_6) %>% 
      data.frame()
    
    
    abind <- a1 %>% 
      rbind(., a2, a3, a4, a5) %>% 
      group_by(player, teammate, game_id, game_date, season, away_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>% 
      filter(TOI > 0) %>% 
      data.frame()
    
    return(abind)
  
    }
  else if (player == "away_on_5") {
    
    a1 <- data %>% 
      group_by(game_id, game_date, season, away_on_5, away_on_1, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_1) %>% 
      data.frame()
    
    a2 <- data %>% 
      group_by(game_id, game_date, season, away_on_5, away_on_2, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_2) %>% 
      data.frame()
    
    a3 <- data %>% 
      group_by(game_id, game_date, season, away_on_5, away_on_3, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_3) %>% 
      data.frame()
    
    a4 <- data %>% 
      group_by(game_id, game_date, season, away_on_5, away_on_4, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_4) %>% 
      data.frame()
    
    a5 <- data %>% 
      group_by(game_id, game_date, season, away_on_5, away_on_6, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_6) %>% 
      data.frame()
    
    
    abind <- a1 %>% 
      rbind(., a2, a3, a4, a5) %>% 
      group_by(player, teammate, game_id, game_date, season, away_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>% 
      filter(TOI > 0) %>% 
      data.frame()
    
    return(abind)
  
    }
  else if (player == "away_on_6") {
    
    a1 <- data %>% 
      group_by(game_id, game_date, season, away_on_6, away_on_1, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_1) %>% 
      data.frame()
    
    a2 <- data %>% 
      group_by(game_id, game_date, season, away_on_6, away_on_2, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_2) %>% 
      data.frame()
    
    a3 <- data %>% 
      group_by(game_id, game_date, season, away_on_6, away_on_3, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_3) %>% 
      data.frame()
    
    a4 <- data %>% 
      group_by(game_id, game_date, season, away_on_6, away_on_4, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_4) %>% 
      data.frame()
    
    a5 <- data %>% 
      group_by(game_id, game_date, season, away_on_6, away_on_5, away_team) %>% 
      fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_5) %>% 
      data.frame()
    
    
    abind <- a1 %>% 
      rbind(., a2, a3, a4, a5) %>% 
      group_by(player, teammate, game_id, game_date, season, away_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>% 
      filter(TOI > 0) %>% 
      data.frame()
    
    return(abind)
  
    }
  
  }

# Combine 
fun.teammate <- function(pbp_data, strength) {
  
  print(paste("season:", unique(pbp_data$season)), quote = F)
  
  # Home data strength state filter
  if (strength == "even") { 
    data_home <- pbp_data %>% 
      filter(game_strength_state %in% st.even_strength, 
             game_period < 5)
    
    }
  else if (strength == "5v5") { 
    data_home <- pbp_data %>% 
      filter(game_strength_state == "5v5", 
             game_period < 5)
    
    }
  
  else if (strength == "powerplay") { 
    data_home <- pbp_data %>% 
      filter(game_strength_state %in% c("5v4", "5v3", "4v3"), 
             game_period < 5)
    
    }
  else if (strength == "5v4") { 
    data_home <- pbp_data %>% 
      filter(game_strength_state %in% c("5v4"), 
             game_period < 5)
    
    }
  
  else if (strength == "shorthanded") { 
    data_home <- pbp_data %>% 
      filter(game_strength_state %in% c("4v5", "3v5", "3v4"), 
             game_period < 5)
    
    }
  else if (strength == "4v5") { 
    data_home <- pbp_data %>% 
      filter(game_strength_state %in% c("4v5"), 
             game_period < 5)
    
    }
  
  # Away data strength state filter
  if (strength == "even") { 
    data_away <- pbp_data %>% 
      filter(game_strength_state %in% st.even_strength, 
             game_period < 5)
  
    }
  else if (strength == "5v5") { 
    data_away <- pbp_data %>% 
      filter(game_strength_state == "5v5", 
             game_period < 5)
    
    }
  
  else if (strength == "powerplay") { 
    data_away <- pbp_data %>% 
      filter(game_strength_state %in%  c("4v5", "3v5", "3v4"), 
             game_period < 5)
  
    }
  else if (strength == "5v4") { 
    data_away <- pbp_data %>% 
      filter(game_strength_state %in%  c("4v5"), 
             game_period < 5)
    
    }
  
  else if (strength == "shorthanded") { 
    data_away <- pbp_data %>%
      filter(game_strength_state %in% c("5v4", "5v3", "4v3"), 
             game_period < 5)
  
    }
  else if (strength == "4v5") { 
    data_away <- pbp_data %>%
      filter(game_strength_state %in% c("5v4"), 
             game_period < 5)
    
    }
  
  
  # Home functions
  print("home_on_1", quote = F)
  home1 <- fun.event_playerH(data_home, "home_on_1")
  
  print("home_on_2", quote = F)
  home2 <- fun.event_playerH(data_home, "home_on_2")
  
  print("home_on_3", quote = F)
  home3 <- fun.event_playerH(data_home, "home_on_3")
  
  print("home_on_4", quote = F)
  home4 <- fun.event_playerH(data_home, "home_on_4")
  
  print("home_on_5", quote = F)
  home5 <- fun.event_playerH(data_home, "home_on_5")
  
  print("home_on_6", quote = F)
  home6 <- fun.event_playerH(data_home, "home_on_6")
  
  
  # Away functions
  print("away_on_1", quote = F)
  away1 <- fun.event_playerA(data_away, "away_on_1")
  
  print("away_on_2", quote = F)
  away2 <- fun.event_playerA(data_away, "away_on_2")
  
  print("away_on_3", quote = F)
  away3 <- fun.event_playerA(data_away, "away_on_3")
  
  print("away_on_4", quote = F)
  away4 <- fun.event_playerA(data_away, "away_on_4")
  
  print("away_on_5", quote = F)
  away5 <- fun.event_playerA(data_away, "away_on_5")
  
  print("away_on_6", quote = F)
  away6 <- fun.event_playerA(data_away, "away_on_6")
  
  
  # Bind 
  print("bind", quote = F)
  
  home_all <- home1 %>% 
    rbind(., home2, home3, home4, home5, home6) %>% 
    group_by(player, teammate, game_id, game_date, season, home_team) %>% 
    summarise(TOI = sum(TOI)) %>% 
    filter(!is.na(player), 
           !is.na(teammate)
           ) %>% 
    rename(Team = home_team) %>% 
    data.frame()
  
  away_all <- away1 %>% 
    rbind(., away2, away3, away4, away5, away6) %>% 
    group_by(player, teammate, game_id, game_date, season, away_team) %>% 
    summarise(TOI = sum(TOI)) %>% 
    filter(!is.na(player), 
           !is.na(teammate)
           ) %>% 
    rename(Team = away_team) %>% 
    data.frame()
  
  
  # Remove Goalies
  print("remove_goalies", quote = F)
  
  # Functions
  fun.goalie_find <- function(data) {
    
    # Identifies goalies within a given pbp data.frame & returns a data.frame to join for removal
    goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data$home_goalie, data$away_goalie))))), 
                                is_goalie = 1
                                )
    
    goalie_return$player <- as.character(goalie_return$player)
    
    return(goalie_return)
  
    }
  goalie_remove <- fun.goalie_find(pbp_data)
  
  hold_H <- home_all[!(home_all$player %in% goalie_remove$player), ] # player
  hold_H <- hold_H[!(hold_H$teammate %in% goalie_remove$player), ]   # teammate
  
  hold_A <- away_all[!(away_all$player %in% goalie_remove$player), ] # player
  hold_A <- hold_A[!(hold_A$teammate %in% goalie_remove$player), ]   # teammate
  
  
  # Combine
  print("combine", quote = F)
  
  return_df <- hold_H %>% 
    rbind(., hold_A) %>% 
    select(player, teammate, game_id, game_date, season, Team, TOI) %>% 
    mutate(player =   ifelse(player == "SEBASTIAN.AHO" & Team == "NYI", "5EBASTIAN.AHO", player),      # FIX NYI AHO
           teammate = ifelse(teammate == "SEBASTIAN.AHO" & Team == "NYI", "5EBASTIAN.AHO", teammate)
           ) %>% 
    arrange(player, teammate, game_id) %>% 
    data.frame()
  
  return(return_df)

  }


#####################################


## -------------------------- ##
##    TOI Against Opponents   ## *** NOT USED ON SITE AT THIS TIME
## -------------------------- ##

################################

# Combos: On Ice Corsi For / Against, TOI, Team - per game
fun.get_time <- function(data) {
  
  hold_player <- data %>% 
    summarise(TOI = sum(event_length) / 60)
  
  return(hold_player)
  
  }

# For all on-ice pairs
fun.opponent_player_H <- function(data, player) {
  
  if (player == "home_on_1") {
    
    h1 <- data %>% 
      group_by(game_id, game_date, season, home_on_1, away_on_1, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_1, 
             opponent = away_on_1) %>% 
      data.frame()
    
    h2 <- data %>% 
      group_by(game_id, game_date, season, home_on_1, away_on_2, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_1, 
             opponent = away_on_2) %>% 
      data.frame()
    
    h3 <- data %>% 
      group_by(game_id, game_date, season, home_on_1, away_on_3, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_1, 
             opponent = away_on_3) %>% 
      data.frame()
    
    h4 <- data %>% 
      group_by(game_id, game_date, season, home_on_1, away_on_4, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_1, 
             opponent = away_on_4) %>% 
      data.frame()
    
    h5 <- data %>% 
      group_by(game_id, game_date, season, home_on_1, away_on_5, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_1, 
             opponent = away_on_5) %>% 
      data.frame()
    
    h6 <- data %>% 
      group_by(game_id, game_date, season, home_on_1, away_on_6, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_1, 
             opponent = away_on_6) %>% 
      data.frame()
    
    
    hbind <- bind_rows(
      h1, h2, h3, h4, h5, h6
    ) %>% 
      group_by(player, opponent, game_id, game_date, season, home_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      ungroup() %>% 
      filter(!is.na(player),  
             !is.na(opponent),  
             TOI > 0) %>% 
      data.frame()
    
    return(hbind)
    
  }
  else if (player == "home_on_2") {
    
    h1 <- data %>% 
      group_by(game_id, game_date, season, home_on_2, away_on_1, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_2, 
             opponent = away_on_1) %>% 
      data.frame()
    
    h2 <- data %>% 
      group_by(game_id, game_date, season, home_on_2, away_on_2, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_2, 
             opponent = away_on_2) %>% 
      data.frame()
    
    h3 <- data %>% 
      group_by(game_id, game_date, season, home_on_2, away_on_3, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_2, 
             opponent = away_on_3) %>% 
      data.frame()
    
    h4 <- data %>% 
      group_by(game_id, game_date, season, home_on_2, away_on_4, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_2, 
             opponent = away_on_4) %>% 
      data.frame()
    
    h5 <- data %>% 
      group_by(game_id, game_date, season, home_on_2, away_on_5, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_2, 
             opponent = away_on_5) %>% 
      data.frame()
    
    h6 <- data %>% 
      group_by(game_id, game_date, season, home_on_2, away_on_6, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_2, 
             opponent = away_on_6) %>% 
      data.frame()
    
    
    hbind <- bind_rows(h1, h2, h3, h4, h5, h6) %>% 
      group_by(player, opponent, game_id, game_date, season, home_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      ungroup() %>% 
      filter(!is.na(player),  
             !is.na(opponent),  
             TOI > 0) %>% 
      data.frame()
    
    return(hbind)
    
  }
  else if (player == "home_on_3") {
    
    h1 <- data %>% 
      group_by(game_id, game_date, season, home_on_3, away_on_1, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_3, 
             opponent = away_on_1) %>% 
      data.frame()
    
    h2 <- data %>% 
      group_by(game_id, game_date, season, home_on_3, away_on_2, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_3, 
             opponent = away_on_2) %>% 
      data.frame()
    
    h3 <- data %>% 
      group_by(game_id, game_date, season, home_on_3, away_on_3, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_3, 
             opponent = away_on_3) %>% 
      data.frame()
    
    h4 <- data %>% 
      group_by(game_id, game_date, season, home_on_3, away_on_4, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_3, 
             opponent = away_on_4) %>% 
      data.frame()
    
    h5 <- data %>% 
      group_by(game_id, game_date, season, home_on_3, away_on_5, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_3, 
             opponent = away_on_5) %>% 
      data.frame()
    
    h6 <- data %>% 
      group_by(game_id, game_date, season, home_on_3, away_on_6, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_3, 
             opponent = away_on_6) %>% 
      data.frame()
    
    
    hbind <- bind_rows(h1, h2, h3, h4, h5, h6) %>% 
      group_by(player, opponent, game_id, game_date, season, home_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      ungroup() %>% 
      filter(!is.na(player),  
             !is.na(opponent),  
             TOI > 0) %>% 
      data.frame()
    
    return(hbind)
    
  }
  else if (player == "home_on_4") {
    
    h1 <- data %>% 
      group_by(game_id, game_date, season, home_on_4, away_on_1, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_4, 
             opponent = away_on_1) %>% 
      data.frame()
    
    h2 <- data %>% 
      group_by(game_id, game_date, season, home_on_4, away_on_2, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_4, 
             opponent = away_on_2) %>% 
      data.frame()
    
    h3 <- data %>% 
      group_by(game_id, game_date, season, home_on_4, away_on_3, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_4, 
             opponent = away_on_3) %>% 
      data.frame()
    
    h4 <- data %>% 
      group_by(game_id, game_date, season, home_on_4, away_on_4, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_4, 
             opponent = away_on_4) %>% 
      data.frame()
    
    h5 <- data %>% 
      group_by(game_id, game_date, season, home_on_4, away_on_5, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_4, 
             opponent = away_on_5) %>% 
      data.frame()
    
    h6 <- data %>% 
      group_by(game_id, game_date, season, home_on_4, away_on_6, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_4, 
             opponent = away_on_6) %>% 
      data.frame()
    
    
    hbind <- bind_rows(h1, h2, h3, h4, h5, h6) %>% 
      group_by(player, opponent, game_id, game_date, season, home_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      ungroup() %>% 
      filter(!is.na(player),  
             !is.na(opponent),  
             TOI > 0) %>% 
      data.frame()
    
    return(hbind)
    
  }
  else if (player == "home_on_5") {
    
    h1 <- data %>% 
      group_by(game_id, game_date, season, home_on_5, away_on_1, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_5, 
             opponent = away_on_1) %>% 
      data.frame()
    
    h2 <- data %>% 
      group_by(game_id, game_date, season, home_on_5, away_on_2, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_5, 
             opponent = away_on_2) %>% 
      data.frame()
    
    h3 <- data %>% 
      group_by(game_id, game_date, season, home_on_5, away_on_3, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_5, 
             opponent = away_on_3) %>% 
      data.frame()
    
    h4 <- data %>% 
      group_by(game_id, game_date, season, home_on_5, away_on_4, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_5, 
             opponent = away_on_4) %>% 
      data.frame()
    
    h5 <- data %>% 
      group_by(game_id, game_date, season, home_on_5, away_on_5, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_5, 
             opponent = away_on_5) %>% 
      data.frame()
    
    h6 <- data %>% 
      group_by(game_id, game_date, season, home_on_5, away_on_6, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_5, 
             opponent = away_on_6) %>% 
      data.frame()
    
    
    hbind <- bind_rows(h1, h2, h3, h4, h5, h6) %>% 
      group_by(player, opponent, game_id, game_date, season, home_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      ungroup() %>% 
      filter(!is.na(player),  
             !is.na(opponent),  
             TOI > 0) %>% 
      data.frame()
    
    return(hbind)
    
  }
  else if (player == "home_on_6") {
    
    h1 <- data %>% 
      group_by(game_id, game_date, season, home_on_6, away_on_1, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_6, 
             opponent = away_on_1) %>% 
      data.frame()
    
    h2 <- data %>% 
      group_by(game_id, game_date, season, home_on_6, away_on_2, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_6, 
             opponent = away_on_2) %>% 
      data.frame()
    
    h3 <- data %>% 
      group_by(game_id, game_date, season, home_on_6, away_on_3, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_6, 
             opponent = away_on_3) %>% 
      data.frame()
    
    h4 <- data %>% 
      group_by(game_id, game_date, season, home_on_6, away_on_4, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_6, 
             opponent = away_on_4) %>% 
      data.frame()
    
    h5 <- data %>% 
      group_by(game_id, game_date, season, home_on_6, away_on_5, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_6, 
             opponent = away_on_5) %>% 
      data.frame()
    
    h6 <- data %>% 
      group_by(game_id, game_date, season, home_on_6, away_on_6, home_team) %>% 
      fun.get_time(.) %>% 
      rename(player = home_on_6, 
             opponent = away_on_6) %>% 
      data.frame()
    
    
    hbind <- bind_rows(h1, h2, h3, h4, h5, h6) %>% 
      group_by(player, opponent, game_id, game_date, season, home_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      ungroup() %>% 
      filter(!is.na(player),  
             !is.na(opponent),  
             TOI > 0) %>% 
      data.frame()
    
    return(hbind)
    
  }
  
}
fun.opponent_player_A <- function(data, player) {
  
  if (player == "away_on_1") {
    
    a1 <- data %>% 
      group_by(game_id, game_date, season, away_on_1, home_on_1, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_1, 
             opponent = home_on_1) %>% 
      data.frame()
    
    a2 <- data %>% 
      group_by(game_id, game_date, season, away_on_1, home_on_2, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_1, 
             opponent = home_on_2) %>% 
      data.frame()
    
    a3 <- data %>% 
      group_by(game_id, game_date, season, away_on_1, home_on_3, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_1, 
             opponent = home_on_3) %>% 
      data.frame()
    
    a4 <- data %>% 
      group_by(game_id, game_date, season, away_on_1, home_on_4, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_1, 
             opponent = home_on_4) %>% 
      data.frame()
    
    a5 <- data %>% 
      group_by(game_id, game_date, season, away_on_1, home_on_5, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_1, 
             opponent = home_on_5) %>% 
      data.frame()
    
    a6 <- data %>% 
      group_by(game_id, game_date, season, away_on_1, home_on_6, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_1, 
             opponent = home_on_6) %>% 
      data.frame()
    
    
    abind <- bind_rows(
      a1, a2, a3, a4, a5, a6) %>% 
      group_by(player, opponent, game_id, game_date, season, away_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      ungroup() %>% 
      filter(!is.na(player),  
             !is.na(opponent),  
             TOI > 0) %>% 
      data.frame()
    
    return(abind)
    
  }
  else if (player == "away_on_2") {
    
    a1 <- data %>% 
      group_by(game_id, game_date, season, away_on_2, home_on_1, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_2, 
             opponent = home_on_1) %>% 
      data.frame()
    
    a2 <- data %>% 
      group_by(game_id, game_date, season, away_on_2, home_on_2, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_2, 
             opponent = home_on_2) %>% 
      data.frame()
    
    a3 <- data %>% 
      group_by(game_id, game_date, season, away_on_2, home_on_3, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_2, 
             opponent = home_on_3) %>% 
      data.frame()
    
    a4 <- data %>% 
      group_by(game_id, game_date, season, away_on_2, home_on_4, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_2, 
             opponent = home_on_4) %>% 
      data.frame()
    
    a5 <- data %>% 
      group_by(game_id, game_date, season, away_on_2, home_on_5, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_2, 
             opponent = home_on_5) %>% 
      data.frame()
    
    a6 <- data %>% 
      group_by(game_id, game_date, season, away_on_2, home_on_6, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_2, 
             opponent = home_on_6) %>% 
      data.frame()
    
    
    abind <- bind_rows(
      a1, a2, a3, a4, a5, a6) %>% 
      group_by(player, opponent, game_id, game_date, season, away_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      ungroup() %>% 
      filter(!is.na(player),  
             !is.na(opponent),  
             TOI > 0) %>% 
      data.frame()
    
    return(abind)
    
  }
  else if (player == "away_on_3") {
    
    a1 <- data %>% 
      group_by(game_id, game_date, season, away_on_3, home_on_1, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_3, 
             opponent = home_on_1) %>% 
      data.frame()
    
    a2 <- data %>% 
      group_by(game_id, game_date, season, away_on_3, home_on_2, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_3, 
             opponent = home_on_2) %>% 
      data.frame()
    
    a3 <- data %>% 
      group_by(game_id, game_date, season, away_on_3, home_on_3, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_3, 
             opponent = home_on_3) %>% 
      data.frame()
    
    a4 <- data %>% 
      group_by(game_id, game_date, season, away_on_3, home_on_4, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_3, 
             opponent = home_on_4) %>% 
      data.frame()
    
    a5 <- data %>% 
      group_by(game_id, game_date, season, away_on_3, home_on_5, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_3, 
             opponent = home_on_5) %>% 
      data.frame()
    
    a6 <- data %>% 
      group_by(game_id, game_date, season, away_on_3, home_on_6, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_3, 
             opponent = home_on_6) %>% 
      data.frame()
    
    
    abind <- bind_rows(
      a1, a2, a3, a4, a5, a6) %>% 
      group_by(player, opponent, game_id, game_date, season, away_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      ungroup() %>% 
      filter(!is.na(player),  
             !is.na(opponent),  
             TOI > 0) %>% 
      data.frame()
    
    return(abind)
    
  }
  else if (player == "away_on_4") {
    
    a1 <- data %>% 
      group_by(game_id, game_date, season, away_on_4, home_on_1, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_4, 
             opponent = home_on_1) %>% 
      data.frame()
    
    a2 <- data %>% 
      group_by(game_id, game_date, season, away_on_4, home_on_2, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_4, 
             opponent = home_on_2) %>% 
      data.frame()
    
    a3 <- data %>% 
      group_by(game_id, game_date, season, away_on_4, home_on_3, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_4, 
             opponent = home_on_3) %>% 
      data.frame()
    
    a4 <- data %>% 
      group_by(game_id, game_date, season, away_on_4, home_on_4, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_4, 
             opponent = home_on_4) %>% 
      data.frame()
    
    a5 <- data %>% 
      group_by(game_id, game_date, season, away_on_4, home_on_5, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_4, 
             opponent = home_on_5) %>% 
      data.frame()
    
    a6 <- data %>% 
      group_by(game_id, game_date, season, away_on_4, home_on_6, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_4, 
             opponent = home_on_6) %>% 
      data.frame()
    
    
    abind <- bind_rows(
      a1, a2, a3, a4, a5, a6) %>% 
      group_by(player, opponent, game_id, game_date, season, away_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      ungroup() %>% 
      filter(!is.na(player),  
             !is.na(opponent),  
             TOI > 0) %>% 
      data.frame()
    
    return(abind)
    
    }
  else if (player == "away_on_5") {
    
    a1 <- data %>% 
      group_by(game_id, game_date, season, away_on_5, home_on_1, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_5, 
             opponent = home_on_1) %>% 
      data.frame()
    
    a2 <- data %>% 
      group_by(game_id, game_date, season, away_on_5, home_on_2, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_5, 
             opponent = home_on_2) %>% 
      data.frame()
    
    a3 <- data %>% 
      group_by(game_id, game_date, season, away_on_5, home_on_3, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_5, 
             opponent = home_on_3) %>% 
      data.frame()
    
    a4 <- data %>% 
      group_by(game_id, game_date, season, away_on_5, home_on_4, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_5, 
             opponent = home_on_4) %>% 
      data.frame()
    
    a5 <- data %>% 
      group_by(game_id, game_date, season, away_on_5, home_on_5, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_5, 
             opponent = home_on_5) %>% 
      data.frame()
    
    a6 <- data %>% 
      group_by(game_id, game_date, season, away_on_5, home_on_6, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_5, 
             opponent = home_on_6) %>% 
      data.frame()
    
    
    abind <- bind_rows(
      a1, a2, a3, a4, a5, a6) %>% 
      group_by(player, opponent, game_id, game_date, season, away_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      ungroup() %>% 
      filter(!is.na(player),  
             !is.na(opponent),  
             TOI > 0) %>% 
      data.frame()
    
    return(abind)
    
    }
  else if (player == "away_on_6") {
    
    a1 <- data %>% 
      group_by(game_id, game_date, season, away_on_6, home_on_1, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_6, 
             opponent = home_on_1) %>% 
      data.frame()
    
    a2 <- data %>% 
      group_by(game_id, game_date, season, away_on_6, home_on_2, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_6, 
             opponent = home_on_2) %>% 
      data.frame()
    
    a3 <- data %>% 
      group_by(game_id, game_date, season, away_on_6, home_on_3, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_6, 
             opponent = home_on_3) %>% 
      data.frame()
    
    a4 <- data %>% 
      group_by(game_id, game_date, season, away_on_6, home_on_4, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_6, 
             opponent = home_on_4) %>% 
      data.frame()
    
    a5 <- data %>% 
      group_by(game_id, game_date, season, away_on_6, home_on_5, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_6, 
             opponent = home_on_5) %>% 
      data.frame()
    
    a6 <- data %>% 
      group_by(game_id, game_date, season, away_on_6, home_on_6, away_team) %>% 
      fun.get_time(.) %>% 
      rename(player = away_on_6, 
             opponent = home_on_6) %>% 
      data.frame()
    
    
    abind <- bind_rows(
      a1, a2, a3, a4, a5, a6) %>% 
      group_by(player, opponent, game_id, game_date, season, away_team) %>% 
      summarise_at(vars(TOI), funs(sum)) %>%
      ungroup() %>% 
      filter(!is.na(player),  
             !is.na(opponent),  
             TOI > 0) %>% 
      data.frame()
    
    return(abind)
    
    }
  
  }

# Combine 
fun.opponent_TOI <- function(pbp_data, strength) {
  
  print(paste("season:", unique(pbp_data$season)), quote = F)
  
  # Home data strength state filter
  if (strength == "even") { 
    data_home <- pbp_data %>% 
      filter(game_strength_state %in% st.even_strength, 
             game_period < 5)
    
    }
  else if (strength == "5v5") { 
    data_home <- pbp_data %>% 
      filter(game_strength_state == "5v5", 
             game_period < 5)
    
    }
  
  else if (strength == "powerplay") { 
    data_home <- pbp_data %>% 
      filter(game_strength_state %in% c("5v4", "5v3", "4v3"), 
             game_period < 5)
    
    }
  else if (strength == "5v4") { 
    data_home <- pbp_data %>% 
      filter(game_strength_state %in% c("5v4"), 
             game_period < 5)
    
    }
  
  else if (strength == "shorthanded") { 
    data_home <- pbp_data %>% 
      filter(game_strength_state %in% c("4v5", "3v5", "3v4"), 
             game_period < 5)
    
    }
  else if (strength == "4v5") { 
    data_home <- pbp_data %>% 
      filter(game_strength_state %in% c("4v5"), 
             game_period < 5)
    
    }
  
  # Away data strength state filter
  if (strength == "even") { 
    data_away <- pbp_data %>% 
      filter(game_strength_state %in% st.even_strength, 
             game_period < 5)
    
    }
  else if (strength == "5v5") { 
    data_away <- pbp_data %>% 
      filter(game_strength_state == "5v5", 
             game_period < 5)
    
    }
  
  else if (strength == "powerplay") { 
    data_away <- pbp_data %>% 
      filter(game_strength_state %in%  c("4v5", "3v5", "3v4"), 
             game_period < 5)
    
    }
  else if (strength == "5v4") { 
    data_away <- pbp_data %>% 
      filter(game_strength_state %in%  c("4v5"), 
             game_period < 5)
    
    }
  
  else if (strength == "shorthanded") { 
    data_away <- pbp_data %>%
      filter(game_strength_state %in% c("5v4", "5v3", "4v3"), 
             game_period < 5)
    
    }
  else if (strength == "4v5") { 
    data_away <- pbp_data %>%
      filter(game_strength_state %in% c("5v4"), 
             game_period < 5)
    
    }
  
  
  # Home functions
  print("home_on_1", quote = F)
  home1 <- fun.opponent_player_H(data_home, "home_on_1")
  
  print("home_on_2", quote = F)
  home2 <- fun.opponent_player_H(data_home, "home_on_2")
  
  print("home_on_3", quote = F)
  home3 <- fun.opponent_player_H(data_home, "home_on_3")
  
  print("home_on_4", quote = F)
  home4 <- fun.opponent_player_H(data_home, "home_on_4")
  
  print("home_on_5", quote = F)
  home5 <- fun.opponent_player_H(data_home, "home_on_5")
  
  print("home_on_6", quote = F)
  home6 <- fun.opponent_player_H(data_home, "home_on_6")
  
  
  # Away functions
  print("away_on_1", quote = F)
  away1 <- fun.opponent_player_A(data_away, "away_on_1")
  
  print("away_on_2", quote = F)
  away2 <- fun.opponent_player_A(data_away, "away_on_2")
  
  print("away_on_3", quote = F)
  away3 <- fun.opponent_player_A(data_away, "away_on_3")
  
  print("away_on_4", quote = F)
  away4 <- fun.opponent_player_A(data_away, "away_on_4")
  
  print("away_on_5", quote = F)
  away5 <- fun.opponent_player_A(data_away, "away_on_5")
  
  print("away_on_6", quote = F)
  away6 <- fun.opponent_player_A(data_away, "away_on_6")
  
  
  # Bind 
  print("bind", quote = F)
  
  home_all <- bind_rows(home1, home2, home3, home4, home5, home6) %>% 
    group_by(player, opponent, game_id, game_date, season, home_team) %>% 
    summarise(TOI = sum(TOI)) %>% 
    rename(Team = home_team) %>% 
    data.frame()
  
  away_all <- bind_rows(away1, away2, away3, away4, away5, away6) %>% 
    group_by(player, opponent, game_id, game_date, season, away_team) %>% 
    summarise(TOI = sum(TOI)) %>% 
    rename(Team = away_team) %>% 
    data.frame()
  
  # Get goalies
  goalie_vec <- sort(unique(c(
    unique(na.omit(pbp_data$home_goalie)), 
    unique(na.omit(pbp_data$away_goalie))
    )))
  
  # Combine
  print("combine", quote = F)
  
  return_df <- bind_rows(
    home_all, 
    away_all
    ) %>% 
    filter(!player %in% goalie_vec, 
           !opponent %in% goalie_vec
           ) %>% 
    select(player, opponent, game_id, game_date, season, Team, TOI) %>% 
    arrange(player, opponent, game_id) %>% 
    data.frame()
  
  return(return_df)
  
  }


################################


## -------------------------- ##
##   Goalies - Game by Game   ##
## -------------------------- ##

################################

# Calculate Standard Goalie Stats
fun.goalie_games <- function(data) {
  
  data <- data %>% 
    filter(game_period < 5, 
           event_length < 900
           ) %>% 
    rename(pred_goal = pred_XGB_7)
  
  
  ## Time on ice
  time_H <- data %>% 
    group_by(home_goalie, game_id, game_date, season, home_team, away_team) %>% 
    summarise(TOI = sum(event_length) / 60) %>% 
    mutate(is_home = 1) %>% 
    rename(player = home_goalie, 
           Team = home_team, 
           Opponent = away_team
           ) %>% 
    filter(!is.na(player)) %>% 
    data.frame()
  
  time_A <- data %>% 
    group_by(away_goalie, game_id, game_date, season, away_team, home_team) %>% 
    summarise(TOI = sum(event_length) / 60) %>% 
    mutate(is_home = 0) %>% 
    rename(player = away_goalie, 
           Team = away_team, 
           Opponent = home_team
           ) %>% 
    filter(!is.na(player)) %>% 
    data.frame()
  
  time <- time_H %>% 
    rbind(., time_A) %>% 
    group_by(player, game_id, game_date, season, Team, Opponent) %>% 
    summarise(is_home = first(is_home), 
              TOI = sum(TOI)
              ) %>% 
    data.frame()
  
  
  ## Main goals + shots data, all situations
  all_shots <- filter(data, event_type %in% st.corsi_events)
  
  home_goalie_shots <- all_shots %>% 
    group_by(home_goalie, game_id, season, home_team) %>% 
    summarise(GA = sum(event_type == "GOAL" & event_team == away_team), 
              SA = sum(event_type %in% c("GOAL", "SHOT") & event_team == away_team)
              ) %>% 
    rename(player = home_goalie, 
           Team = home_team
           ) %>% 
    filter(!is.na(player)) %>% 
    data.frame()
  
  away_goalie_shots <- all_shots %>% 
    group_by(away_goalie, game_id, season, away_team) %>% 
    summarise(GA = sum(event_type == "GOAL" & event_team == home_team), 
              SA = sum(event_type %in% c("GOAL", "SHOT") & event_team == home_team)
              ) %>% 
    rename(player = away_goalie, 
           Team = away_team
           ) %>% 
    filter(!is.na(player)) %>% 
    data.frame()
  
  joined_shots <- home_goalie_shots %>% 
    rbind(., away_goalie_shots) %>% 
    group_by(player, game_id, season, Team) %>% 
    summarise_at(vars(GA, SA), funs(sum)) %>% 
    data.frame()
  
  
  ## Shot data (xG specific)
  fenwicks <- data %>% 
    filter(event_type %in% st.fenwick_events, 
           !is.na(pred_goal)) 
  
  home_goalie <- fenwicks %>% 
    group_by(home_goalie, game_id, season, home_team) %>% 
    summarise(GA_ = sum(event_type == "GOAL" & event_team == away_team), 
              FA = sum(event_type %in% st.fenwick_events & event_team == away_team), 
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal))
              ) %>% 
    rename(player = home_goalie, 
           Team = home_team
           ) %>% 
    filter(!is.na(player)) %>% 
    data.frame()
  
  away_goalie <- fenwicks %>% 
    group_by(away_goalie, game_id, season, away_team) %>% 
    summarise(GA_ = sum(event_type == "GOAL" & event_team == home_team), 
              FA = sum(event_type %in% st.fenwick_events & event_team == home_team), 
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal))
              ) %>% 
    rename(player = away_goalie, 
           Team = away_team
           ) %>% 
    filter(!is.na(player)) %>% 
    data.frame()
  
  goalies <- home_goalie %>% 
    rbind(., away_goalie) %>% 
    group_by(player, game_id, season, Team) %>% 
    summarise_at(vars(GA_, FA, xGA), funs(sum)) %>% 
    data.frame()
  
  
  ## Join
  joined_return <- time %>% 
    left_join(joined_shots, ., by = c("player", "game_id", "season", "Team")) %>% 
    left_join(goalies, ., by = c("player", "game_id", "season", "Team")) %>% 
    ungroup() %>% 
    select(player, game_id, game_date, season, Team, Opponent, is_home, 
           TOI, GA, SA, GA_, FA, xGA
           ) %>% 
    arrange(player) %>% 
    data.frame()
  
  }


################################


## ---------------------------- ##
##   Penalty Goals Calculaton   ##
## ---------------------------- ##

##################################

# Wrangle & Sort Data
fun.pen_setup <- function(data) {
  
  if (nrow(data %>% filter(event_type %in% c("PENL"), !(grepl("fighting", tolower(event_description))), !(grepl("misconduct", tolower(event_description))))) > 0) { 
    
    data <- data %>% 
      mutate(scradj = home_score - away_score, 
             home_lead = ifelse(scradj >= 3, 3, 
                                ifelse(scradj <= -3, -3, scradj)),
             home_lead_state = ifelse(home_lead < 0, 1, 
                                      ifelse(home_lead == 0, 2, 
                                             ifelse(home_lead > 0, 3, home_lead))), 
             home_lead = home_lead + 4, 
             event_length = ifelse(is.na(event_length), 0, event_length))
    
    # Determine "initial" next strength state
    pbp_part <- data %>% 
      filter(event_type %in% c("PENL", "FAC"), 
             !(grepl("misconduct", tolower(event_description))), 
             !(grepl("fighting", tolower(event_description)))
             ) %>% 
      group_by(game_id) %>% 
      mutate(next_st_state1 = ifelse(event_type == "PENL" & lead(event_type) == "FAC", lead(game_strength_state), "X"), 
             next_st_state1 = ifelse(is.na(next_st_state1), "X", next_st_state1)
             ) %>% 
      filter(event_type == "PENL") %>% 
      mutate(last_pen_time = ifelse(lag(game_seconds) != game_seconds, lag(game_seconds), 0)) %>%
      select(game_id, event_index, last_pen_time, next_st_state1) %>% 
      data.frame()
    
    pbp_part[is.na(pbp_part)] <- 0
    
    # Join next strength state
    pen_add <- left_join(data, pbp_part, by = c("game_id", "event_index"))
    
    # Identify features
    pen_source <- pen_add %>% 
      filter(event_type %in% c("PENL"), 
             !(grepl("fighting", tolower(event_description))), 
             !(grepl("misconduct", tolower(event_description)))
             ) %>% 
      mutate(# Add last penalty time for cluster penalties (probably a better way to do this)
        last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
        last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
        last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
        last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
        last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
        last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
        last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
        last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
        last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
        last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
        last_pen_time = ifelse(last_pen_time == 0, lag(last_pen_time), last_pen_time), 
        last_pen_time = ifelse(last_pen_time > 0 & lag(game_id) != game_id, 0, last_pen_time),
        
        cluster = 1 * (game_seconds == lead(game_seconds) & game_id == lead(game_id)), 
        cluster = ifelse(cluster == 0 & lag(cluster) == 1, 1, cluster), 
        
        pen_shot = 1 * grepl("PS-", event_description), 
        major =    1 * grepl("maj", tolower(event_description)),
        double =   1 * grepl("double minor", tolower(event_description)), 
        team_pen = 1 * grepl("bench", tolower(event_description)),
        
        # Add next strength state for cluster penalties (not sure of a better way to do this)
        next_st_state2  = ifelse(next_st_state1  == "X" & game_seconds == lead(game_seconds), lead(next_st_state1),  next_st_state1), 
        next_st_state3  = ifelse(next_st_state2  == "X" & game_seconds == lead(game_seconds), lead(next_st_state2),  next_st_state2), 
        next_st_state4  = ifelse(next_st_state3  == "X" & game_seconds == lead(game_seconds), lead(next_st_state3),  next_st_state3), 
        next_st_state5  = ifelse(next_st_state4  == "X" & game_seconds == lead(game_seconds), lead(next_st_state4),  next_st_state4), 
        next_st_state6  = ifelse(next_st_state5  == "X" & game_seconds == lead(game_seconds), lead(next_st_state5),  next_st_state5), 
        next_st_state7  = ifelse(next_st_state6  == "X" & game_seconds == lead(game_seconds), lead(next_st_state6),  next_st_state6), 
        next_st_state8  = ifelse(next_st_state7  == "X" & game_seconds == lead(game_seconds), lead(next_st_state7),  next_st_state7), 
        next_st_state9  = ifelse(next_st_state8  == "X" & game_seconds == lead(game_seconds), lead(next_st_state8),  next_st_state8), 
        next_st_state10 = ifelse(next_st_state9  == "X" & game_seconds == lead(game_seconds), lead(next_st_state9),  next_st_state9), 
        next_st_state11 = ifelse(next_st_state10 == "X" & game_seconds == lead(game_seconds), lead(next_st_state10), next_st_state10), 
        next_st_state12 = ifelse(next_st_state11 == "X" & game_seconds == lead(game_seconds), lead(next_st_state11), next_st_state11), 
        next_st_state13 = ifelse(next_st_state12 == "X" & game_seconds == lead(game_seconds), lead(next_st_state12), next_st_state12), 
        next_st_state14 = ifelse(next_st_state13 == "X" & game_seconds == lead(game_seconds), lead(next_st_state13), next_st_state13), 
        next_st_state15 = ifelse(next_st_state14 == "X" & game_seconds == lead(game_seconds), lead(next_st_state14), next_st_state14), 
        
        no_impact = 1 * ((game_strength_state %in% c("5v5", "4v4", "3v3") & next_st_state15 %in% c("5v5", "4v4", "3v3")) | 
                           (game_strength_state == next_st_state15 & 
                              (game_strength_state != "5v3" & game_strength_state != "3v5" & 
                                 game_strength_state != "Ev3" & game_strength_state != "3vE")) |
                           grepl("match", tolower(event_description)) | 
                           pen_shot == 1 | 
                           major == 1 | 
                           next_st_state15 == "X"), 
        
        # Correct simultaneous minors
        cluster = ifelse(cluster == 1 & 
                           # 5v5
                           (((game_strength_state == "5v5" & next_st_state15 %in% c("5v3", "3v5")) | 
                               (game_strength_state == "5vE" & next_st_state15 == "3v5") | 
                               (game_strength_state == "Ev5" & next_st_state15 == "5v3")) | 
                              # Powerplay
                              (game_strength_state == "5v4" & next_st_state15 == "3v4") | 
                              (game_strength_state == "4v5" & next_st_state15 == "4v3")), 
                         0, cluster)
        ) %>% 
      select(game_id, event_index, season, game_date, event_type, event_description, 
             event_detail, event_team, event_player_1, event_player_2, home_team, 
             away_team, home_lead, home_lead_state, game_period, game_seconds, 
             last_pen_time, game_strength_state, next_st_state15, pen_shot, 
             major, double, team_pen, no_impact, cluster
             ) %>%  
      rename(next_st_state = next_st_state15) %>% 
      data.frame()
    
    return(pen_source)
    
    } else { 
    # If no penalties, return empty data.frame with same structure
    pen_source <- data.frame(game_id = character(), 
                             event_index = numeric(), 
                             season = character(), 
                             game_date = character(), 
                             event_type = character(), 
                             event_description = character(), 
                             event_detail = character(), 
                             event_team = character(), 
                             event_player_1 = character(), 
                             event_player_2 = character(), 
                             home_team = character(), 
                             away_team = character(), 
                             home_lead = numeric(), 
                             home_lead_state = numeric(), 
                             game_period = numeric(), 
                             game_seconds = numeric(), 
                             last_pen_time = numeric(), 
                             game_strength_state = character(), 
                             next_st_state15 = character(), 
                             pen_shot = numeric(), 
                             major = numeric(), 
                             double = numeric(), 
                             team_pen = numeric(), 
                             no_impact = numeric(), 
                             cluster = numeric())
    
    }

  }

# Assignments - initial
fun.pen_assign <- function(data) {
  
  # create data.frame to check if this process needs to be completed
  x <- data %>% 
    select(cluster, no_impact) %>% 
    mutate(check = 1 * (cluster == 1 & no_impact == 0))
  
  if (sum(na.omit(x$check)) > 0) { 
    
    # Initial gather
    test1 <- data %>% 
      filter(no_impact == 0, cluster == 1) %>% 
      mutate(pen_id = round((as.numeric(game_id) * as.numeric(game_seconds)) / 10000000, 2)) %>% 
      select(game_id, event_index, pen_id, event_player_1, event_player_2)
    
    test1a <- test1 %>% 
      select(-c(game_id, event_index, event_player_2)) %>% 
      gather(Column, Value, -pen_id, na.rm = TRUE) %>%
      count(pen_id, Value)  %>% 
      rename(take = Value)
    
    test1b <- test1 %>% 
      select(-c(game_id, event_index, event_player_1)) %>% 
      gather(Column, Value, -pen_id, na.rm = TRUE) %>%
      count(pen_id, Value)  %>% 
      rename(draw = Value)
    
    test1c <- full_join(test1a, test1b, by = c("pen_id"))
    
    # Determine order of penalties taken / drawn
    test2a <- test1c %>% 
      select(pen_id, take, n.x) %>% 
      group_by(pen_id, take) %>% 
      summarise(take_n = as.numeric(sum(n.x))) %>% 
      rename(player = take) %>% 
      data.frame()
    
    test2b <- test1c %>%
      select(pen_id, draw, n.y) %>% 
      group_by(pen_id, draw) %>% 
      summarise(draw_n = as.numeric(sum(n.y))) %>% 
      rename(player = draw) %>% 
      data.frame()
    
    test2c <- test2a %>% 
      full_join(., test2b, by = c("pen_id", "player")) %>% 
      filter(!is.na(player)) %>% 
      mutate(take_n = ifelse(is.na(take_n), 0, take_n), 
             draw_n = ifelse(is.na(draw_n), 0, draw_n), 
             diff = draw_n - take_n
             ) %>% 
      arrange(pen_id, desc(diff)) %>% 
      mutate(offset1 = ifelse(pen_id == lag(pen_id) & take_n == lag(take_n) & draw_n == lag(draw_n) & diff == 0 & lag(diff) == 0, 1, 0), 
             offset1 = ifelse(is.na(offset1) & lead(offset1) == 1 & pen_id == lead(pen_id), 1, offset1), 
             offset2 = ifelse(lead(offset1) == 1 & offset1 == 0 & pen_id == lead(pen_id), 1, offset1), 
             offset3 = ifelse(is.na(offset2) & offset1 == 0, 0, offset2)
             ) %>% 
      select(-c(offset1, offset2)) %>% 
      mutate(diff_fix = ifelse(is.na(offset3) & diff == 0 & lead(diff) < diff, -lead(diff), diff), 
             offset3 = ifelse(is.na(offset3), 0, offset3)
             ) %>% 
      select(pen_id:draw_n, diff_fix, offset3) %>% 
      rename(offset = offset3) %>% 
      data.frame()
    
    # Remove further offsetting penalties
    test2d <- test2c %>% 
      filter(offset == 1) %>% 
      select(pen_id, offset) %>% 
      group_by(pen_id) %>% 
      summarise(offset = first(offset)) %>% 
      data.frame()
    
    # Assign
    test3 <- test2c %>% 
      filter(offset != 1) %>% 
      select(-c(offset))
    
    test3a <- test3 %>% 
      group_by(pen_id) %>% 
      arrange(pen_id, desc(diff_fix)) %>%
      slice(c(1, n())) %>%
      mutate(Type = c("most", "least")) %>% 
      select(-c(take_n, draw_n, diff_fix)) %>%
      spread(Type, player) %>% 
      rename(take_assign = least, 
             draw_assign = most
             ) %>% 
      data.frame()
    
    # Final determination
    test4 <- test1 %>% 
      left_join(., test3a, by = c("pen_id")) %>% 
      group_by(pen_id) %>% 
      mutate(n = n(),  
             test = ifelse(lag(event_player_1) == event_player_2 & is.na(lag(event_player_2)), 1, 0), 
             test = ifelse(is.na(test) & lead(test) == 1, 1, test), 
             take_update = ifelse((n == 2 & lead(test == 1) & test == 1) | 
                                    (n == 2 & test == 1 & test == 1), draw_assign, take_assign), 
             draw_update = ifelse((n == 2 & lead(test == 1) & test == 1) | 
                                    (n == 2 & test == 1 & test == 1), take_assign, draw_assign), 
             take_update = ifelse(is.na(take_update), take_assign, take_update), 
             draw_update = ifelse(is.na(draw_update), draw_assign, draw_update), 
             offset = ifelse(is.na(take_update) & is.na(draw_update), 1, 0)
             ) %>% 
      arrange(game_id, event_index) %>% 
      mutate(first_assign = as.numeric(row_number()), 
             first_assign = ifelse(offset == 1, 0, first_assign)
             ) %>% 
      ungroup() %>% 
      select(-c(take_assign, draw_assign, n, test)) %>% 
      rename(take_assign = take_update, 
             draw_assign = draw_update
             ) %>% 
      select(game_id, event_index, offset, first_assign, take_assign, draw_assign) %>% 
      data.frame()
    
    # Join Data
    pen_enhanced <- data %>% 
      left_join(. , test4, by = c("game_id", "event_index")) %>% 
      mutate(offset = ifelse(is.na(offset), 0, offset), 
             first_assign = ifelse(is.na(first_assign), 0, first_assign), 
             take_assign = ifelse(is.na(take_assign), 0, take_assign), 
             draw_assign = ifelse(is.na(draw_assign), 0, draw_assign), 
             no_impact2 = ifelse(no_impact == 1 | offset == 1 | first_assign > 1, 1, 0), 
             cluster = ifelse(is.na(cluster), 0, cluster), 
             last_pen_time = ifelse(is.na(last_pen_time), 0, last_pen_time)
             ) %>% 
      select(game_id:double, team_pen, no_impact2, cluster:draw_assign) %>% 
      rename(no_impact = no_impact2) %>% 
      data.frame()
    
    pen_enhanced$season <- as.numeric(pen_enhanced$season)
    
    return(pen_enhanced)
    
    } else {
    # Return data.frame in correct format with all 0s
    pen_enhanced <- data %>% 
      mutate(offset = 0, 
             first_assign = 0, 
             take_assign = 0, 
             draw_assign = 0)
    
    return(pen_enhanced)
    
    }

  }

# Calculate goal value - MAIN
fun.pen_value_main <- function(pen_data, pbp_data) {
  
  # Remove Duplicates (2007 & 2009)
  pen_calc_main <- pen_data %>% 
    filter(no_impact != 1) %>% 
    mutate(event_player_1 = ifelse(is.na(event_player_1), 0, event_player_1), 
           event_player_2 = ifelse(is.na(event_player_2), 0, event_player_2), 
           duplicate = 1 * (game_id == lag(game_id) & event_player_1 == lag(event_player_1) & 
                              event_player_2 == lag(event_player_2) & event_description == lag(event_description) & 
                              home_team == lag(home_team) & away_team == lag(away_team) & 
                              event_team == lag(event_team) & game_period == lag(game_period) & 
                              game_strength_state == lag(game_strength_state)), 
           duplicate = ifelse(is.na(duplicate), 0, duplicate)
           ) %>% 
    filter(duplicate == 0) %>% 
    select(-c(duplicate)) %>% 
    mutate(event_player_1 = ifelse(event_player_1 == 0, NA, event_player_1), 
           event_player_2 = ifelse(event_player_2 == 0, NA, event_player_2)
           ) %>% 
    
    # Calculate Goal Values for Main and Assigned penalties
    mutate(time_diff = game_seconds - last_pen_time, 
           
           # Venue Unspecific
           EV_5v5_5v4 = ifelse(game_strength_state %in% c("5v5", "Ev5", "5vE") & next_st_state %in% c("5v4", "4v5", "Ev4", "4vE"), 
                               ifelse(double == 1 & first_assign == 0, 
                                      ((penrate_GF[1,5] - penrate_GA[1,5]) * 240) / 3600, 
                                      ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600), 
                               0), 
           
           EV_4v4_4v3 = ifelse(game_strength_state %in% c("4v4", "Ev4", "4vE") & next_st_state %in% c("4v3", "3v4"),
                               ifelse(game_period == 4, 
                                      
                                      ifelse(season >= 20152016, 
                                             ((penrate_GF[3,7] - penrate_GA[3,7]) * (ifelse(3900 - game_seconds > 120, 120, 3900 - game_seconds))) / 3600, 
                                             ((penrate_GF[2,7] - penrate_GA[2,7]) * (ifelse(3900 - game_seconds > 120, 120, 3900 - game_seconds))) / 3600), 
                                      
                                      ((penrate_GF[2,7] - penrate_GA[2,7]) * (120 - (ifelse(time_diff > 120 | time_diff < 0, 60, time_diff))) / 3600) + 
                                        (((penrate_GF[1,5] - penrate_GA[1,5]) * (ifelse(time_diff > 120 | time_diff < 0, 60, time_diff))) / 3600)), 
                               0), 
           
           EV_3v3_4v3 = ifelse(game_strength_state %in% c("3v3", "Ev3", "3vE") & next_st_state %in% c("3v4", "4v3"), 
                               ((penrate_GF[3,7] - penrate_GA[3,7]) * (ifelse(3900 - game_seconds > 120, 120, (3900 - game_seconds)))) / 3600, 
                               0), 
           
           EV_5v5_5v3 = ifelse((game_strength_state %in% c("5v5", "Ev5", "5vE") & next_st_state %in% c("5v3", "3v5")), 
                               ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 
                               0), 
           
           # Identify Home/Away Powerplay (home = 1, away = 2)
           home_PP1_away_PP2 = ifelse(game_strength_state %in% c("5v4", "Ev4") & next_st_state %in% c("5v3", "Ev3"), 1, 
                                      ifelse(game_strength_state %in% c("5v3", "Ev3") & next_st_state %in% c("5v3", "Ev3"), 1, 
                                             ifelse((game_strength_state %in% c("5v4", "Ev4") & next_st_state =="4v4") | 
                                                      (game_strength_state == "5v4" & next_st_state == "4vE") | 
                                                      (game_strength_state == "5vE" & next_st_state == "4v4"), 1, 
                                                    ifelse((game_strength_state == "5v3" & next_st_state == "4v3"), 1, 
                                                           ifelse((game_strength_state == "5v4" & next_st_state == "3v4"), 1, 
                                                                  ifelse(game_strength_state %in% c("4v3") & next_st_state == "3v3", 1, 
                                                                         
                                                                         ifelse(game_strength_state %in% c("4v5", "4vE") & next_st_state %in% c("3v5", "3vE"), 2, 
                                                                                ifelse(game_strength_state %in% c("3v5", "3vE") & next_st_state %in% c("3v5", "3vE"), 2, 
                                                                                       ifelse((game_strength_state %in% c("4v5", "4vE") & next_st_state == "4v4") | 
                                                                                                (game_strength_state == "4v5" & next_st_state == "Ev4") | 
                                                                                                (game_strength_state == "Ev5" & next_st_state == "4v4"), 2, 
                                                                                              ifelse((game_strength_state == "3v5" & next_st_state == "3v4"), 2, 
                                                                                                     ifelse((game_strength_state == "4v5" & next_st_state == "4v3"), 2, 
                                                                                                            ifelse(game_strength_state %in% c("3v4") & next_st_state == "3v3", 2, 
                                                                                                                   0)))))))))))
                                      ), 
           
           # Separate to Home/Away Powerplay
           PP_5v4_5v3_h = ifelse(game_strength_state %in% c("5v4", "Ev4") & next_st_state %in% c("5v3", "Ev3"),
                                 ifelse(time_diff < 120, 
                                        ((penrate_GF[4,6] - penrate_GA[4,6]) * (120 - time_diff)) / 3600 + 
                                          ((penrate_GF[1,5] - penrate_GA[1,5]) * time_diff) / 3600, 
                                        ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600), 
                                 0), 
           PP_4v5_3v5_a = ifelse(game_strength_state %in% c("4v5", "4vE") & next_st_state %in% c("3v5", "3vE"),
                                 ifelse(time_diff < 120, 
                                        ((penrate_GF[4,6] - penrate_GA[4,6]) * (120 - time_diff)) / 3600 + 
                                          ((penrate_GF[1,5] - penrate_GA[1,5]) * time_diff) / 3600, 
                                        ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600), 
                                 0),  
           
           PP_5v3_5v3_h = ifelse(game_strength_state %in% c("5v3", "Ev3") & next_st_state %in% c("5v3", "Ev3"), 
                                 ((penrate_GF[4,6] - penrate_GA[4,6]) * 120) / 3600, 
                                 0), 
           PP_5v3_5v3_a = ifelse(game_strength_state %in% c("3v5", "3vE") & next_st_state %in% c("3v5", "3vE"), 
                                 ((penrate_GF[4,6] - penrate_GA[4,6]) * 120) / 3600, 
                                 0), 
           
           PP_5v4_4v4_h = ifelse((game_strength_state %in% c("5v4", "Ev4") & next_st_state =="4v4") | 
                                   (game_strength_state == "5v4" & next_st_state == "4vE") | 
                                   (game_strength_state == "5vE" & next_st_state == "4v4"), 
                                 ((penrate_GA[4,3] - penrate_GF[4,3]) * (120 - time_diff)) / 3600 + 
                                   ((penrate_GF[1,5] - penrate_GA[1,5]) * time_diff) / 3600, 
                                 0), 
           PP_4v5_4v4_a = ifelse((game_strength_state %in% c("4v5", "4vE") & next_st_state == "4v4") | 
                                   (game_strength_state == "4v5" & next_st_state == "Ev4") | 
                                   (game_strength_state == "Ev5" & next_st_state == "4v4"), 
                                 ((penrate_GA[4,3] - penrate_GF[4,3]) * (120 - time_diff)) / 3600 + 
                                   ((penrate_GF[1,5] - penrate_GA[1,5]) * time_diff) / 3600, 
                                 0), 
           
           PP_5v3_4v3_h = ifelse((game_strength_state == "5v3" & next_st_state == "4v3"), 
                                 ((penrate_GA[5,7] - penrate_GF[5,7]) * (120 - time_diff)) / 3600 + 
                                   ((penrate_GA[4,3] - penrate_GF[4,3]) * time_diff) / 3600, 
                                 0), 
           PP_3v5_3v4_a = ifelse((game_strength_state == "3v5" & next_st_state == "3v4"), 
                                 ((penrate_GA[5,7] - penrate_GF[5,7]) * (120 - time_diff)) / 3600 + 
                                   ((penrate_GA[4,3] - penrate_GF[4,3]) * time_diff) / 3600, 
                                 0), 
           
           PP_5v4_3v4_h = ifelse((game_strength_state == "5v4" & next_st_state == "3v4"), 
                                 ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 
                                 0), 
           PP_4v5_4v3_a = ifelse((game_strength_state == "4v5" & next_st_state == "4v3"), 
                                 ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 
                                 0), 
           
           PP_4v3_3v3_h = ifelse(game_strength_state %in% c("4v3") & next_st_state == "3v3", 
                                 ((penrate_GA[6,4] - penrate_GF[6,4]) * (120 - time_diff)) / 3600 + 
                                   ((penrate_GF[3,7] - penrate_GA[3,7]) * time_diff) / 3600, 
                                 0), 
           PP_3v4_3v3_a = ifelse(game_strength_state %in% c("3v4") & next_st_state == "3v3", 
                                 ((penrate_GA[6,4] - penrate_GF[6,4]) * (120 - time_diff)) / 3600 + 
                                   ((penrate_GF[3,7] - penrate_GA[3,7]) * time_diff) / 3600, 
                                 0), 
           
           # Sum
           sub_sum = EV_5v5_5v4 + EV_4v4_4v3 + 
             EV_3v3_4v3 + EV_5v5_5v3 + 
             
             PP_5v4_5v3_h + PP_4v5_3v5_a + 
             PP_5v3_5v3_h + PP_5v3_5v3_a + 
             PP_5v4_4v4_h + PP_4v5_4v4_a +  
             PP_5v3_4v3_h + PP_3v5_3v4_a + 
             PP_5v4_3v4_h + PP_4v5_4v3_a +
             PP_4v3_3v3_h + PP_3v4_3v3_a, 
           
           unk_pen = ifelse(sub_sum == 0, ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 0), 
           pen_value = sub_sum + unk_pen)
  
  # Verify Assigned Penalties
  pen_calc_verify <- pen_data %>% 
    mutate(event_player_1 = ifelse(is.na(event_player_1), 0, event_player_1), 
           event_player_2 = ifelse(is.na(event_player_2), 0, event_player_2), 
           duplicate = 1 * (game_id == lag(game_id) & event_player_1 == lag(event_player_1) & 
                              event_player_2 == lag(event_player_2) & event_description == lag(event_description) & 
                              home_team == lag(home_team) & away_team == lag(away_team) & 
                              event_team == lag(event_team) & game_period == lag(game_period) & 
                              game_strength_state == lag(game_strength_state)), 
           duplicate = ifelse(is.na(duplicate), 0, duplicate)
           ) %>% 
    filter(duplicate == 0, first_assign > 0) %>% 
    select(-c(duplicate)) %>% 
    mutate(event_player_1 = ifelse(event_player_1 == 0, NA, event_player_1), 
           event_player_2 = ifelse(event_player_2 == 0, NA, event_player_2), 
           verified1 = ifelse(first_assign == 1 & lead(game_seconds) == game_seconds & 
                                (lead(event_player_2, 2) == event_player_1 & 
                                   lead(event_player_1, 2) == event_player_2), 
                              1, 0), 
           verified1 = ifelse(is.na(verified1), 0, verified1), 
           verified2 = ifelse(first_assign == 1 & lead(game_seconds) == game_seconds & 
                                (lead(event_player_2, 1) == event_player_1 & 
                                   lead(event_player_1, 1) == event_player_2), 
                              1, 0), 
           verified2 = ifelse(is.na(verified2), 0, verified2), 
           verified3 = ifelse(first_assign == 1 & lead(game_seconds) == game_seconds & 
                                (lead(event_player_2, 1) == lead(event_player_1, 2) & 
                                   lead(event_player_1, 1) == lead(event_player_2, 2)), 
                              1, 0), 
           verified3 = ifelse(is.na(verified3), 0, verified3), 
           assign_verify = verified1 + verified2 + verified3
           ) %>% 
    select(game_id, event_index, assign_verify)
  
  # Join verified
  pen_calc_main <- pen_calc_main %>% 
    left_join(., pen_calc_verify, by = c("game_id", "event_index")) %>% 
    mutate(assign_verify = ifelse(is.na(assign_verify), 0, assign_verify)) %>% 
    select(game_id:draw_assign, assign_verify, time_diff:pen_value)
  
  # Correct "Assigned" penalties (remove wrongly assigned)
  pen_calc_assign <- pen_calc_main %>% 
    filter(first_assign == 1) %>% 
    select(game_id, event_index, take_assign, draw_assign)
  
  pbp_players <- pbp_data %>% 
    filter(event_type == "PENL") %>% 
    select(game_id, event_index, home_on_1:away_on_6)
  
  pbp_players_join <- pen_calc_assign %>% 
    right_join(pbp_players, ., by = c("game_id", "event_index"))
  
  pbp_players_join[is.na(pbp_players_join)] <- 0
  
  pbp_players_join <- pbp_players_join %>% 
    mutate(no_impact2 = ifelse(((take_assign == home_on_1 | take_assign == home_on_2 | take_assign == home_on_3 | 
                                   take_assign == home_on_4 | take_assign == home_on_5 | take_assign == home_on_6) & 
                                  (draw_assign == home_on_1 | draw_assign == home_on_2 | draw_assign == home_on_3 | 
                                     draw_assign == home_on_4 | draw_assign == home_on_5 | draw_assign == home_on_6)) | 
                                 
                                 ((take_assign == away_on_1 | take_assign == away_on_2 | take_assign == away_on_3 | 
                                     take_assign == away_on_4 | take_assign == away_on_5 | take_assign == away_on_6) & 
                                    (draw_assign == away_on_1 | draw_assign == away_on_2 | draw_assign == away_on_3 | 
                                       draw_assign == away_on_4 | draw_assign == away_on_5 | draw_assign == away_on_6)), 
                               1, 0)
           ) %>% 
    select(-c(home_on_1:away_on_6, take_assign, draw_assign))
  
  pen_calc_main <- pen_calc_main %>% 
    left_join(., pbp_players_join, by = c("game_id", "event_index")) %>% 
    mutate(no_impact2 = ifelse(is.na(no_impact2), 0, no_impact2),  
           no_impact3 = no_impact + no_impact2
           ) %>% 
    filter(no_impact3 != 1) %>% 
    select(-c(no_impact2, no_impact3)) %>% 
    data.frame()
  
  return(pen_calc_main)
  
  }

# Calculate goal value - Extras
fun.pen_value_xtra <- function(data) {
  
  if (sum(na.omit(data$pen_shot)) + sum(na.omit(data$major)) > 0) {  # check if there are "extra" penalties
    
    # Calculate Penalty Shot + Major Goal Values
    pen_calc_xtras <- data %>% 
      filter(pen_shot == 1 | major == 1) %>% 
      mutate(event_player_1 = ifelse(is.na(event_player_1), 0, event_player_1), 
             event_player_2 = ifelse(is.na(event_player_2), 0, event_player_2), 
             duplicate = 1 * (game_id == lag(game_id) & event_player_1 == lag(event_player_1) & 
                                event_player_2 == lag(event_player_2) & event_description == lag(event_description) & 
                                home_team == lag(home_team) & away_team == lag(away_team) & 
                                event_team == lag(event_team) & game_period == lag(game_period) & 
                                game_strength_state == lag(game_strength_state)), 
             duplicate = ifelse(is.na(duplicate), 0, duplicate), 
             
             # Re-do no impact for xtras
             no_impact = 1 * ((game_strength_state %in% c("5v5", "4v4", "3v3") & next_st_state %in% c("5v5", "4v4", "3v3")) | 
                                grepl("match", tolower(event_description)) | 
                                (game_strength_state == next_st_state & 
                                   (game_strength_state != "5v3" & game_strength_state != "3v5" & 
                                      game_strength_state != "Ev3" & game_strength_state != "3vE"))), 
             no_impact = ifelse(pen_shot == 1, 0, no_impact)
             ) %>% 
      filter(duplicate == 0) %>% 
      select(-c(duplicate)) %>% 
      mutate(event_player_1 = ifelse(event_player_1 == 0, NA, event_player_1), 
             event_player_2 = ifelse(event_player_2 == 0, NA, event_player_2)) %>%
      
      mutate(time_diff = game_seconds - last_pen_time, 
             
             # Penalty Shots
             PS_EV_take = ifelse(pen_shot == 1 & game_strength_state %in% c("5v5", "4v4", "3v3", "Ev5", "5vE"), 
                                 ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 0), 
             
             PS_PP_take = ifelse(pen_shot == 1 & game_strength_state %in% c("5v4", "4v5", "Ev4", "4vE", "5v3", "3v5", "4v3", "3v4"), 
                                 ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 0), 
             
             PS_EV_draw = ifelse(pen_shot == 1 & game_strength_state %in% c("5v5", "4v4", "3v3", "Ev5", "5vE"), 
                                 .3194, 0), 
             
             PS_PP_draw = ifelse(pen_shot == 1 & game_strength_state %in% c("5v4", "4v5", "Ev4", "4vE", "5v3", "3v5", "4v3", "3v4"), 
                                 .3194, 0), 
             
             # Majors - Even Strength
             maj_EV_take = ifelse(major == 1 & game_strength_state %in% c("5v5", "4v4", "3v3", "Ev5", "5vE") & next_st_state != 0, 
                                  ((penrate_GF[1,5] - penrate_GA[1,5]) * 300) / 3600, 0), 
             
             maj_EV_draw = ifelse(major == 1 & game_strength_state %in% c("5v5", "4v4", "3v3", "Ev5", "5vE") & next_st_state != 0, 
                                  ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 0), 
             
             # Majors - Powerplay
             maj_PP_take_h = ifelse(major == 1 & (game_strength_state %in% c("5v4", "Ev4") & next_st_state %in% c("5v3", "Ev3")),
                                    ifelse(time_diff < 120, 
                                           ((penrate_GF[4,6] - penrate_GA[4,6]) * (120 - time_diff)) / 3600 + 
                                             ((penrate_GF[1,5] - penrate_GA[1,5]) * (300 - time_diff)) / 3600, 
                                           ((penrate_GF[1,5] - penrate_GA[1,5]) * 300) / 3600), 0), 
             
             maj_PP_take_a = ifelse(major == 1 & (game_strength_state %in% c("4v5", "4vE") & next_st_state %in% c("3v5", "3vE")),
                                    ifelse(time_diff < 120, 
                                           ((penrate_GF[4,6] - penrate_GA[4,6]) * (120 - time_diff)) / 3600 + 
                                             ((penrate_GF[1,5] - penrate_GA[1,5]) * (300 - time_diff)) / 3600, 
                                           ((penrate_GF[1,5] - penrate_GA[1,5]) * 300) / 3600), 0), 
             
             home_PP1_away_PP2 = ifelse(maj_PP_take_h > 0, 1, ifelse(maj_PP_take_a > 0, 2, 0)), 
             
             maj_PP_draw_h = ifelse(maj_PP_take_h > 0, ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 0), 
             maj_PP_draw_a = ifelse(maj_PP_take_a > 0, ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, 0), 
             
             # Combine
             EV_take = PS_EV_take + maj_EV_take, 
             EV_draw = PS_EV_draw + maj_EV_draw, 
             PP_SH_take = PS_PP_take + maj_PP_take_h + maj_PP_take_a, 
             PP_SH_draw = PS_PP_draw + maj_PP_draw_h + maj_PP_draw_a, 
             
             pen_value_take = EV_take + PP_SH_take, 
             pen_value_take = ifelse(pen_value_take == 0, ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, pen_value_take), 
             pen_value_draw = EV_draw + PP_SH_draw, 
             pen_value_draw = ifelse(pen_value_draw == 0, ((penrate_GF[1,5] - penrate_GA[1,5]) * 120) / 3600, pen_value_draw)
             ) %>% 
      data.frame()
    
    return(pen_calc_xtras)
    
    } else { 
    # Return empty 1 x 1 data.frame if no "extra" penalties (checked in fun.pen_value_sum)
    pen_calc_xtras <- data.frame(none = NA)
    
    return(pen_calc_xtras)
    
    }
  
  }

# Sum and Combine: Player Game-by-Game Adj. Pen Differential
fun.pen_value_sum <- function(main_data, xtra_data) {
  
  # Sum Main
  p_main_take <- main_data %>% 
    filter(first_assign != 1, 
           team_pen != 1
           ) %>% 
    group_by(event_player_1, game_id, game_date, season) %>% 
    summarise(take = sum(pen_value), 
              adj_take = sum(ifelse(home_team == event_team, pen_value * pen_score_adj[home_lead_state, 4],  
                                    ifelse(away_team == event_team, pen_value * pen_score_adj[home_lead_state, 5], pen_value))), 
              take_count = sum(event_type == "PENL")
              ) %>% 
    filter(!is.na(event_player_1)) %>% 
    rename(player = event_player_1) %>% 
    data.frame()
  
  p_main_draw <- main_data %>% 
    filter(first_assign != 1, 
           team_pen != 1
           ) %>% 
    group_by(event_player_2, game_id, game_date, season) %>% 
    summarise(draw = sum(pen_value), 
              adj_draw = sum(ifelse(home_team == event_team, pen_value * pen_score_adj[home_lead_state, 4],  
                                    ifelse(away_team == event_team, pen_value * pen_score_adj[home_lead_state, 5], pen_value))), 
              draw_count = sum(event_type == "PENL")
              ) %>%
    filter(!is.na(event_player_2)) %>% 
    rename(player = event_player_2) %>% 
    data.frame()
  
  p_main_all <- p_main_take %>% 
    full_join(., p_main_draw, by = c("player", "game_id", "game_date", "season")) %>% 
    data.frame()
  
  p_main_all[is.na(p_main_all)] <- 0
  
  
  # Sum Extras
  if (ncol(xtra_data) > 1) {  # check if there were "extra" penalties
    
    p_xtra_take <- xtra_data %>% 
      group_by(event_player_1, game_id, game_date, season) %>% 
      summarise(take = sum(pen_value_take), 
                adj_take = sum(ifelse(home_team == event_team, pen_value_take * pen_score_adj[home_lead_state, 4],  
                                      ifelse(away_team == event_team, pen_value_take * pen_score_adj[home_lead_state, 5], pen_value_take))), 
                take_count = sum(event_type == "PENL")
                ) %>%
      filter(!is.na(event_player_1)) %>% 
      rename(player = event_player_1) %>% 
      data.frame()
    
    p_xtra_draw <- xtra_data %>% 
      group_by(event_player_2, game_id, game_date, season) %>% 
      summarise(draw = sum(pen_value_draw), 
                adj_draw = sum(ifelse(home_team == event_team, pen_value_draw * pen_score_adj[home_lead_state, 4],  
                                      ifelse(away_team == event_team, pen_value_draw * pen_score_adj[home_lead_state, 5], pen_value_draw))), 
                draw_count = sum(event_type == "PENL")
                ) %>%
      filter(!is.na(event_player_2)) %>% 
      rename(player = event_player_2) %>% 
      data.frame()
    
    # Fix Draw or Take is NA on one penalty
    if (nrow(p_xtra_take) > 0 & nrow(p_xtra_draw) > 0) { 
      p_xtra_all <- p_xtra_take %>% 
        full_join(., p_xtra_draw, by = c("player", "game_id", "game_date", "season")) %>% 
        data.frame()
      
      p_xtra_all[is.na(p_xtra_all)] <- 0
      
      } 
    else if (nrow(p_xtra_take) > 0 & nrow(p_xtra_draw) == 0) { 
     p_xtra_draw[1, ] <- NA
     p_xtra_draw[1, 1] <- "TESTING"
     
     p_xtra_all <- p_xtra_take %>% 
       full_join(., p_xtra_draw, by = c("player", "game_id", "game_date", "season")) %>% 
       filter(player != "TESTING") %>% 
       data.frame()
     
     p_xtra_all[is.na(p_xtra_all)] <- 0
     
     }
    else if (nrow(p_xtra_take) == 0 & nrow(p_xtra_draw) > 0) { 
      p_xtra_take[1, ] <- NA
      p_xtra_take[1, 1] <- "TESTING"
      
      p_xtra_all <- p_xtra_take %>% 
        full_join(., p_xtra_draw, by = c("player", "game_id", "game_date", "season")) %>% 
        filter(player != "TESTING") %>% 
        data.frame()
      
      p_xtra_all[is.na(p_xtra_all)] <- 0
      
      }
    
    
    # Return blank data.frame in correct format if no "extra" penalties
    } else { 
    p_xtra_all <- data.frame(player = character(), 
                             game_id = character(), 
                             game_date = character(),
                             season = numeric(), 
                             take = numeric(), 
                             adj_take = numeric(), 
                             take_count = numeric(), 
                             draw = numeric(), 
                             adj_draw = numeric(), 
                             draw_count = numeric())
    
    }
  
  
  # Sum Assign
  if (nrow(main_data %>% filter(first_assign == 1, assign_verify == 1, team_pen != 1)) > 0) { # check if there were "assigned" penalties
    
    p_assign_take <- main_data %>% 
      filter(first_assign == 1, 
             assign_verify == 1, 
             team_pen != 1
             ) %>% 
      group_by(take_assign, game_id, game_date, season) %>% 
      summarise(take = sum(pen_value), 
                ## Unable to determine player's team at this point
                adj_take = sum(pen_value), 
                take_count = sum(event_type == "PENL")
                ) %>% 
      filter(!is.na(take_assign)) %>% 
      rename(player = take_assign) %>% 
      data.frame()
    
    p_assign_draw <- main_data %>% 
      filter(first_assign == 1, 
             assign_verify == 1, 
             team_pen != 1
             ) %>% 
      group_by(draw_assign, game_id, game_date, season) %>% 
      summarise(draw = sum(pen_value), 
                ## Unable to determine player's team at this point
                adj_draw = sum(pen_value), 
                draw_count = sum(event_type == "PENL")
                ) %>%
      filter(!is.na(draw_assign)) %>% 
      rename(player = draw_assign) %>% 
      data.frame()
    
    p_assign_all <- p_assign_take %>% 
      full_join(., p_assign_draw, by = c("player", "game_id", "game_date", "season")) %>% 
      data.frame()
    
    p_assign_all[is.na(p_assign_all)] <- 0
    
    } else { 
      
    # Return blank data.frame in correct format if no "assigned" penalties
    p_assign_all <- data.frame(player = character(), 
                               game_id = character(), 
                               game_date = character(), 
                               season = numeric(), 
                               take = numeric(), 
                               adj_take = numeric(), 
                               take_count = numeric(), 
                               draw = numeric(), 
                               adj_draw = numeric(), 
                               draw_count = numeric())
    
    }
  
  
  # Combine All Penalty Tables
  p_combined <- p_main_all %>% 
    rbind(., p_xtra_all, p_assign_all) %>% 
    group_by(player, game_id, game_date, season) %>% 
    summarise_all(funs(sum)) %>% 
    mutate_at(vars(take:draw_count), funs(round(.,3))) %>% 
    mutate(take = -(take), 
           adj_take = -(adj_take), 
           adj_pen_diff = adj_take + adj_draw, 
           season = as.character(season)
           ) %>% 
    select(player, game_id, game_date, season, take_count, draw_count, take, draw, adj_take, adj_draw, adj_pen_diff) %>% 
    arrange(player, game_id) %>% 
    data.frame()
  
  return(p_combined)

  }

# Add On Skater/Goalie Position & Team
fun.pen_value_sum_add <- function(pen_data, skater_data, goalie_data, position_data) { 
  
  return_df <- pen_data %>% 
    left_join(., skater_data %>%                           ## Skaters 
                select(player, game_id, Team), 
              by = c("player", "game_id")
              ) %>% 
    left_join(., position_data, by = "player") %>% 
    left_join(., goalie_data %>%                           ## Goalies
                select(player, game_id, Team) %>%  
                rename(Team_goalie = Team), 
              by = c("player", "game_id")
              ) %>% 
    mutate(position = ifelse(is.na(position) & !is.na(Team_goalie), 3, position), 
           Team =     ifelse(is.na(Team) & !is.na(Team_goalie), Team_goalie, Team)
           ) %>% 
    select(player, position, game_id, game_date, season, Team, take_count:adj_pen_diff)
  
  }

# Sum Team Penalties: Game by Game
fun.pen_team_take <- function(data) {
  
  if (nrow(data %>% filter(team_pen == 1)) > 0) { 
    
    p_team_take <- data %>% 
      filter(team_pen == 1) %>% 
      group_by(event_team, game_id) %>% 
      summarise(take_count = sum(event_type == "PENL"), 
                take = sum(pen_value), 
                adj_take = sum(ifelse(home_team == event_team, pen_value * pen_score_adj[home_lead_state, 4],  
                                      ifelse(away_team == event_team, pen_value * pen_score_adj[home_lead_state, 5], pen_value)))
                ) %>% 
      filter(!is.na(event_team)) %>% 
      mutate_at(vars(take:adj_take), funs(round(., 3))) %>% 
      rename(Team = event_team) %>% 
      data.frame()
    
    return(p_team_take)
    
    } else {
    # Return empty data.frame in same format if no team penalties
    p_team_take <- data.frame(Team = character(), 
                              game_id = character(), 
                              take_count = integer(), 
                              take = numeric(), 
                              adj_take = numeric())
    
    }
  
  }


##################################


## ------------------------------ ##
##   Team Stats Games Functions   ##
## ------------------------------ ##

####################################

## All Situations
fun.team_games_all_sit <- function(data) {
  
  data <- data %>% 
    filter(game_period < 5, 
           event_length < 900
           ) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, 
                              ifelse(scradj <= -3, -3, scradj)),
           home_lead_state = ifelse(home_lead < 0, 1, 
                                    ifelse(home_lead == 0, 2, 
                                           ifelse(home_lead > 0, 3, home_lead))), 
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           ) %>% 
    rename(pred_goal = pred_XGB_7)
  
  
  team_h <- data %>% 
    group_by(home_team, season, game_id, game_date) %>% 
    summarise(Opponent = first(away_team), 
              
              TOI = sum(event_length) / 60, 
              
              GF =  sum((event_type == "GOAL" & event_team == home_team)), 
              GA =  sum((event_type == "GOAL" & event_team == away_team)), 
              G_diff = GF - GA, 
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              xG_diff = xGF - xGA, 
              
              SF =  sum((event_type %in% st.shot_events & event_team == home_team)), 
              SA =  sum((event_type %in% st.shot_events & event_team == away_team)), 
              S_diff = SF - SA, 
              
              FF =  sum((event_type %in% st.fenwick_events & event_team == home_team)), 
              FA =  sum((event_type %in% st.fenwick_events & event_team == away_team)),
              F_diff = FF - FA, 
              
              CF =  sum((event_type %in% st.corsi_events & event_team == home_team)), 
              CA =  sum((event_type %in% st.corsi_events & event_team == away_team)),
              C_diff = CF - CA, 
              
              PEND2 = sum(na.omit(1 * (event_type == "PENL" & event_team == away_team) +
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              PENT2 = sum(na.omit(1 * (event_type == "PENL" & event_team == home_team) +
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              
              PEND5 = sum(na.omit(event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_detail)))), 
              PENT5 = sum(na.omit(event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_detail))))
              ) %>% 
    mutate(is_home = 1) %>% 
    rename(Team = home_team) %>% 
    data.frame()
  
  
  team_a <- data %>% 
    group_by(away_team, season, game_id, game_date) %>% 
    summarise(Opponent = first(home_team), 
              
              TOI = sum(event_length) / 60, TOI = sum(event_length) / 60, 
              
              GF =  sum((event_type == "GOAL" & event_team == away_team)), 
              GA =  sum((event_type == "GOAL" & event_team == home_team)), 
              G_diff = GF - GA, 
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              xG_diff = xGF - xGA, 
              
              SF =  sum((event_type %in% st.shot_events & event_team == away_team)), 
              SA =  sum((event_type %in% st.shot_events & event_team == home_team)), 
              S_diff = SF - SA, 
              
              FF =  sum((event_type %in% st.fenwick_events & event_team == away_team)), 
              FA =  sum((event_type %in% st.fenwick_events & event_team == home_team)),
              F_diff = FF - FA, 
              
              CF =  sum((event_type %in% st.corsi_events & event_team == away_team)), 
              CA =  sum((event_type %in% st.corsi_events & event_team == home_team)),
              C_diff = CF - CA, 
              
              
              PEND2 = sum(na.omit(1 * (event_type == "PENL" & event_team == home_team) +
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              PENT2 = sum(na.omit(1 * (event_type == "PENL" & event_team == away_team) +
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              PEND5 = sum(na.omit(event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_detail)))), 
              PENT5 = sum(na.omit(event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_detail))))
              ) %>% 
    mutate(is_home = 0) %>% 
    rename(Team = away_team) %>% 
    data.frame()
  
  
  # Join
  mergetest <- Reduce(function(...) merge(..., all = TRUE), list(team_h, team_a))
  
  # By game
  games <- mergetest %>% 
    group_by(Team, Opponent, game_id, game_date, season, is_home) %>% 
    summarise_all(funs(sum)) %>% 
    select(Team, Opponent, game_id, game_date, season, is_home, 
           TOI, GF:C_diff, PEND2:PENT5
           ) %>% 
    arrange(Team, game_id) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
    mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
    data.frame()
  
  return(games)
  
  }

## Even-Strength
fun.team_games_EV <- function(data, strength, scr_adj_list) {
  
  # In-function filter for strength state
  if (strength == "EV") {
    strength_ <- st.even_strength
    
    } else {
      strength_ <- strength
    
      }
  
  # Filter pbp data / add required columns
  data <- data %>% 
    filter(game_strength_state %in% strength_, 
           game_period < 5
           ) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, 
                              ifelse(scradj <= -3, -3, scradj)),
           home_lead_state = ifelse(home_lead < 0, 1, 
                                    ifelse(home_lead == 0, 2, 
                                           ifelse(home_lead > 0, 3, home_lead))), 
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           ) %>% 
    rename(pred_goal = pred_XGB_7)
  
  
  team_h <- data %>% 
    group_by(home_team, season, game_id, game_date) %>% 
    summarise(Opponent = first(away_team),  
              
              TOI_5v5 = sum((game_strength_state == "5v5") * event_length) / 60, 
              TOI_4v4 = sum((game_strength_state == "4v4") * event_length) / 60, 
              TOI_3v3 = sum((game_strength_state == "3v3") * event_length) / 60, 
              TOI = TOI_5v5 + TOI_4v4 + TOI_3v3, 
              
              # Unadjusted
              GF =  sum(event_type == "GOAL" & event_team == home_team), 
              GA =  sum(event_type == "GOAL" & event_team == away_team), 
              G_diff = GF - GA, 
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              xG_diff = xGF - xGA, 
              
              SF =  sum(event_type %in% st.shot_events & event_team == home_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == away_team), 
              S_diff = SF - SA, 
              
              FF =  sum(event_type %in% st.fenwick_events & event_team == home_team), 
              FA =  sum(event_type %in% st.fenwick_events & event_team == away_team),
              F_diff = FF - FA, 
              
              CF =  sum(event_type %in% st.corsi_events & event_team == home_team), 
              CA =  sum(event_type %in% st.corsi_events & event_team == away_team),
              C_diff = CF - CA, 
              
              # Score & Venue Adjusted
              GF_adj =  sum((event_type == "GOAL" & event_team == home_team) * scr_adj_list$home_goal_adj[home_lead_state]), 
              GA_adj =  sum((event_type == "GOAL" & event_team == away_team) * scr_adj_list$away_goal_adj[home_lead_state]), 
              G_diff_adj = GF_adj - GA_adj, 
              
              xGF_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * scr_adj_list$home_xG_adj[home_lead_state])),
              xGA_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * scr_adj_list$away_xG_adj[home_lead_state])), 
              xG_diff_adj = xGF_adj - xGA_adj, 
              
              SF_adj =  sum((event_type %in% st.shot_events & event_team == home_team) * scr_adj_list$home_shots_adj[home_lead]), 
              SA_adj =  sum((event_type %in% st.shot_events & event_team == away_team) * scr_adj_list$away_shots_adj[home_lead]), 
              S_diff_adj = SF_adj - SA_adj, 
              
              FF_adj =  sum((event_type %in% st.fenwick_events & event_team == home_team) * scr_adj_list$home_fenwick_adj[home_lead]), 
              FA_adj =  sum((event_type %in% st.fenwick_events & event_team == away_team) * scr_adj_list$away_fenwick_adj[home_lead]),
              F_diff_adj = FF_adj - FA_adj, 
              
              CF_adj =  sum((event_type %in% st.corsi_events & event_team == home_team) * scr_adj_list$home_corsi_adj[home_lead]), 
              CA_adj =  sum((event_type %in% st.corsi_events & event_team == away_team) * scr_adj_list$away_corsi_adj[home_lead]),
              C_diff_adj = CF_adj - CA_adj, 
              
              PEND2 = sum(na.omit(1 * (event_type == "PENL" & event_team == away_team) +
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              PENT2 = sum(na.omit(1 * (event_type == "PENL" & event_team == home_team) +
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              
              PEND5 = sum(na.omit(event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_detail)))), 
              PENT5 = sum(na.omit(event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_detail))))
              ) %>% 
    mutate(is_home = 1) %>% 
    rename(Team = home_team) %>% 
    data.frame()
  
  
  team_a <- data %>% 
    group_by(away_team, season, game_id, game_date) %>% 
    summarise(Opponent = first(home_team), 
              
              TOI_5v5 = sum((game_strength_state == "5v5") * event_length) / 60, 
              TOI_4v4 = sum((game_strength_state == "4v4") * event_length) / 60, 
              TOI_3v3 = sum((game_strength_state == "3v3") * event_length) / 60, 
              TOI = TOI_5v5 + TOI_4v4 + TOI_3v3, 
              
              # Unadjusted
              GF =  sum(event_type == "GOAL" & event_team == away_team), 
              GA =  sum(event_type == "GOAL" & event_team == home_team), 
              G_diff = GF - GA, 
              
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              xG_diff = xGF - xGA, 
              
              SF =  sum(event_type %in% st.shot_events & event_team == away_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == home_team), 
              S_diff = SF - SA, 
              
              FF =  sum(event_type %in% st.fenwick_events & event_team == away_team), 
              FA =  sum(event_type %in% st.fenwick_events & event_team == home_team),
              F_diff = FF - FA, 
              
              CF =  sum(event_type %in% st.corsi_events & event_team == away_team), 
              CA =  sum(event_type %in% st.corsi_events & event_team == home_team),
              C_diff = CF - CA, 
              
              # Score & Venue Adjusted
              GF_adj =  sum((event_type == "GOAL" & event_team == away_team) * scr_adj_list$away_goal_adj[home_lead_state]), 
              GA_adj =  sum((event_type == "GOAL" & event_team == home_team) * scr_adj_list$home_goal_adj[home_lead_state]), 
              G_diff_adj = GF_adj - GA_adj, 
              
              xGF_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * scr_adj_list$away_xG_adj[home_lead_state])),
              xGA_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * scr_adj_list$home_xG_adj[home_lead_state])), 
              xG_diff_adj = xGF_adj - xGA_adj, 
              
              SF_adj =  sum((event_type %in% st.shot_events & event_team == away_team) * scr_adj_list$away_shots_adj[home_lead]), 
              SA_adj =  sum((event_type %in% st.shot_events & event_team == home_team) * scr_adj_list$home_shots_adj[home_lead]), 
              S_diff_adj = SF_adj - SA_adj, 
              
              FF_adj =  sum((event_type %in% st.fenwick_events & event_team == away_team) * scr_adj_list$away_fenwick_adj[home_lead]), 
              FA_adj =  sum((event_type %in% st.fenwick_events & event_team == home_team) * scr_adj_list$home_fenwick_adj[home_lead]),
              F_diff_adj = FF_adj - FA_adj, 
              
              CF_adj =  sum((event_type %in% st.corsi_events & event_team == away_team) * scr_adj_list$away_corsi_adj[home_lead]), 
              CA_adj =  sum((event_type %in% st.corsi_events & event_team == home_team) * scr_adj_list$home_corsi_adj[home_lead]),
              C_diff_adj = CF_adj - CA_adj, 
              
              
              PEND2 = sum(na.omit(1 * (event_type == "PENL" & event_team == home_team) +
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              PENT2 = sum(na.omit(1 * (event_type == "PENL" & event_team == away_team) +
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              PEND5 = sum(na.omit(event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_detail)))), 
              PENT5 = sum(na.omit(event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_detail))))
              ) %>% 
    mutate(is_home = 0) %>% 
    rename(Team = away_team) %>% 
    data.frame()
  
  
  # Join
  mergetest <- Reduce(function(...) merge(..., all = TRUE), list(team_h, team_a))
  
  # By game
  games <- mergetest %>% 
    group_by(Team, Opponent, game_id, game_date, season, is_home) %>% 
    summarise_all(funs(sum)) %>% 
    select(Team, Opponent, game_id, game_date, season, is_home, 
           TOI, TOI_5v5:TOI_3v3, GF:C_diff, GF_adj:C_diff_adj, PEND2:PENT5
           ) %>% 
    arrange(Team, game_id) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
    mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
    data.frame()
  
  
  # Remove columns not used in below strength states
  if (strength == "EV") { 
    games <- games %>% 
      select(-c(GF:C_diff)) %>% 
      rename_at(vars(GF_adj:C_diff_adj), funs(gsub("_adj", "", .)))
    
    }
  else if (strength != "EV") { 
    games <- games %>% 
      select(-c(TOI_5v5:TOI_3v3))
    
    }
  
  return(games)
  
  }

## Powerplay
fun.team_games_PP <- function(data, strength, scr_adj_list) {
  
  # In-function filter for strength state: pbp data
  if (strength == "PP") {
    strength_ <- st.pp_strength
    
    } else {
      strength_ <- c(strength, paste(rev(unlist(strsplit(strength, NULL))), collapse = ""))
    
      }
  
  # Filter pbp data / add required columns
  data <- data %>% 
    filter(game_strength_state %in% strength_, 
           game_period < 5
           ) %>% 
    select(-c(face_index:shift_length)) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, 
                              ifelse(scradj <= -3, -3, scradj)),
           home_lead_state = ifelse(home_lead < 0, 1, 
                                    ifelse(home_lead == 0, 2, 
                                           ifelse(home_lead > 0, 3, home_lead))), 
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           ) %>% 
    rename(pred_goal = pred_XGB_7)
  
  
  # In-function strength for filter: home/away data
  if (strength == "PP") { 
    strength_home <- c("5v4", "5v3", "4v3")
    strength_away <- c("4v5", "3v5", "3v4")
    
    } else { 
      strength_home <- strength
      strength_away <- paste(rev(unlist(strsplit(strength, NULL))), collapse = "")
    
      }
  
  
  # Sum data per game
  team_h <- data %>% 
    filter(game_strength_state %in% strength_home) %>% 
    group_by(home_team, season, game_id, game_date) %>% 
    summarise(Opponent = first(away_team), 
              
              TOI_5v4 = sum((game_strength_state == "5v4") * event_length) / 60, 
              TOI_5v3 = sum((game_strength_state == "5v3") * event_length) / 60, 
              TOI_4v3 = sum((game_strength_state == "4v3") * event_length) / 60, 
              TOI = TOI_5v4 + TOI_5v3 + TOI_4v3, 
              
              GF =  sum(event_type == "GOAL" & event_team == home_team), 
              GA =  sum(event_type == "GOAL" & event_team == away_team),
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              SF =  sum(event_type %in% st.shot_events & event_team == home_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == away_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == home_team), 
              FA =  sum(event_type %in% st.fenwick_events & event_team == away_team),
              CF =  sum(event_type %in% st.corsi_events & event_team == home_team), 
              CA =  sum(event_type %in% st.corsi_events & event_team == away_team),
              
              GF_adj =  sum((event_type == "GOAL" & event_team == home_team) * scr_adj_list$home_goal_adj[home_lead_state]), 
              xGF_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * scr_adj_list$home_xG_adj[home_lead_state])),
              SF_adj =  sum((event_type %in% st.shot_events & event_team == home_team) * scr_adj_list$home_shot_adj[home_lead_state]), 
              FF_adj =  sum((event_type %in% st.fenwick_events & event_team == home_team) * scr_adj_list$home_fenwick_adj[home_lead_state]), 
              CF_adj =  sum((event_type %in% st.corsi_events & event_team == home_team) * scr_adj_list$home_corsi_adj[home_lead_state]), 
              
              
              PEND2 = sum(na.omit(1 * (event_type == "PENL" & event_team == away_team) +
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              PENT2 = sum(na.omit(1 * (event_type == "PENL" & event_team == home_team) +
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              
              PEND5 = sum(na.omit(event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_detail)))), 
              PENT5 = sum(na.omit(event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_detail))))
              ) %>% 
    rename(Team = home_team) %>% 
    mutate(is_home = 1) %>% 
    data.frame()
  
  team_a <- data %>% 
    filter(game_strength_state %in% strength_away) %>% 
    group_by(away_team, season, game_id, game_date) %>% 
    summarise(Opponent = first(home_team), 
              
              TOI_5v4 = sum((game_strength_state == "4v5") * event_length) / 60, 
              TOI_5v3 = sum((game_strength_state == "3v5") * event_length) / 60, 
              TOI_4v3 = sum((game_strength_state == "3v4") * event_length) / 60, 
              TOI = TOI_5v4 + TOI_5v3 + TOI_4v3, 
              
              GF =  sum(event_type == "GOAL" & event_team == away_team), 
              GA =  sum(event_type == "GOAL" & event_team == home_team),
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)),
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              SF =  sum(event_type %in% st.shot_events & event_team == away_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == home_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == away_team), 
              FA =  sum(event_type %in% st.fenwick_events & event_team == home_team),
              CF =  sum(event_type %in% st.corsi_events & event_team == away_team), 
              CA =  sum(event_type %in% st.corsi_events & event_team == home_team),
              
              GF_adj =  sum((event_type == "GOAL" & event_team == away_team) * scr_adj_list$away_goal_adj[home_lead_state]), 
              xGF_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * scr_adj_list$away_xG_adj[home_lead_state])),
              SF_adj =  sum((event_type %in% st.shot_events & event_team == away_team) * scr_adj_list$away_shot_adj[home_lead_state]), 
              FF_adj =  sum((event_type %in% st.fenwick_events & event_team == away_team) * scr_adj_list$away_fenwick_adj[home_lead_state]), 
              CF_adj =  sum((event_type %in% st.corsi_events & event_team == away_team) * scr_adj_list$away_corsi_adj[home_lead_state]), 
              
              
              PEND2 = sum(na.omit(1 * (event_type == "PENL" & event_team == home_team) +
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              PENT2 = sum(na.omit(1 * (event_type == "PENL" & event_team == away_team) +
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              PEND5 = sum(na.omit(event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_detail)))), 
              PENT5 = sum(na.omit(event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_detail))))
              ) %>% 
    rename(Team = away_team) %>% 
    mutate(is_home = 0) %>% 
    data.frame()
  
  # Join
  mergetest <- Reduce(function(...) merge(..., all=TRUE), list(team_h, team_a))
  
  # By game
  games <- mergetest %>% 
    group_by(Team, Opponent, game_id, game_date, season, is_home) %>% 
    summarise_all(funs(sum)) %>% 
    select(Team, Opponent, game_id, game_date, season, is_home, 
           TOI, TOI_5v4:TOI_4v3, 
           GF:CA, GF_adj:CF_adj, 
           PEND2:PENT5
           ) %>% 
    arrange(Team, game_id) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
    mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
    data.frame()
  
  
  # Remove columns not used in below strength states
  if (strength == "PP") { 
    games <- games %>% 
      select(-c(GF, xGF, SF, FF, CF)) %>% 
      rename_at(vars(GF_adj, xGF_adj, SF_adj, FF_adj, CF_adj), funs(gsub("_adj", "", .))) %>% 
      select(Team, Opponent, game_id, game_date, season, is_home, 
             TOI, TOI_5v4:TOI_4v3, 
             GF, GA, xGF, xGA, SF, SA, FF, FA, CF, CA, 
             PEND2:PENT5)
    
    }
  else if (strength != "PP") { 
    games <- games %>% 
      select(-c(TOI_5v4:TOI_4v3))
    
    }
  
  return(games)
  
  }

## Shorthanded   
fun.team_games_SH <- function(data, strength, scr_adj_list) {
  
  # In-function filter for strength state: pbp data
  if (strength == "SH") {
    strength_ <- st.pp_strength
    
    } else {
      strength_ <- c(strength, paste(rev(unlist(strsplit(strength, NULL))), collapse = ""))
    
      }
  
  # Filter pbp data / add required columns
  data <- data %>% 
    filter(game_strength_state %in% strength_, 
           game_period < 5
           ) %>% 
    select(-c(face_index:shift_length)) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, 
                              ifelse(scradj <= -3, -3, scradj)),
           home_lead_state = ifelse(home_lead < 0, 1, 
                                    ifelse(home_lead == 0, 2, 
                                           ifelse(home_lead > 0, 3, home_lead))), 
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           ) %>% 
    rename(pred_goal = pred_XGB_7)
  
  
  # in-function strength for filter: home/away data
  if (strength == "SH") { 
    strength_home <- c("4v5", "3v5", "3v4")
    strength_away <- c("5v4", "5v3", "4v3")
    
    } else { 
      strength_home <- strength
      strength_away <- paste(rev(unlist(strsplit(strength, NULL))), collapse = "")
    
      }
  
  
  # Data per game
  team_h <- data %>% 
    filter(game_strength_state %in% strength_home) %>% 
    group_by(home_team, season, game_id, game_date) %>% 
    summarise(Opponent = first(away_team), 
              
              TOI_4v5 = sum((game_strength_state == "4v5") * event_length) / 60, 
              TOI_3v5 = sum((game_strength_state == "3v5") * event_length) / 60, 
              TOI_3v4 = sum((game_strength_state == "3v4") * event_length) / 60, 
              TOI = TOI_4v5 + TOI_3v5 + TOI_3v4, 
              
              GF =  sum(event_type == "GOAL" & event_team == home_team),
              GA =  sum(event_type == "GOAL" & event_team == away_team), 
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)), 
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              SF =  sum(event_type %in% st.shot_events & event_team == home_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == away_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == home_team),
              FA =  sum(event_type %in% st.fenwick_events & event_team == away_team), 
              CF =  sum(event_type %in% st.corsi_events & event_team == home_team),
              CA =  sum(event_type %in% st.corsi_events & event_team == away_team), 
              
              GA_adj =  sum((event_type == "GOAL" & event_team == away_team) * scr_adj_list$away_goal_adj[home_lead_state]), 
              xGA_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal * scr_adj_list$away_xG_adj[home_lead_state])), 
              SA_adj =  sum((event_type %in% st.shot_events & event_team == away_team) * scr_adj_list$away_shot_adj[home_lead_state]), 
              FA_adj =  sum((event_type %in% st.fenwick_events & event_team == away_team) * scr_adj_list$away_fenwick_adj[home_lead_state]), 
              CA_adj =  sum((event_type %in% st.corsi_events & event_team == away_team) * scr_adj_list$away_corsi_adj[home_lead_state]), 
              
              
              PEND2 = sum(na.omit(1 * (event_type == "PENL" & event_team == away_team) +
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              PENT2 = sum(na.omit(1 * (event_type == "PENL" & event_team == home_team) +
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              
              PEND5 = sum(na.omit(event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_detail)))), 
              PENT5 = sum(na.omit(event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_detail))))
              ) %>% 
    rename(Team = home_team) %>% 
    mutate(is_home = 1) %>% 
    data.frame()
  
  team_a <- data %>% 
    filter(game_strength_state %in% strength_away) %>% 
    group_by(away_team, season, game_id, game_date) %>% 
    summarise(Opponent = first(home_team), 
              
              TOI_4v5 = sum((game_strength_state == "5v4") * event_length) / 60, 
              TOI_3v5 = sum((game_strength_state == "5v3") * event_length) / 60, 
              TOI_3v4 = sum((game_strength_state == "4v3") * event_length) / 60, 
              TOI = TOI_4v5 + TOI_3v5 + TOI_3v4, 
              
              GF =  sum(event_type == "GOAL" & event_team == away_team),
              GA =  sum(event_type == "GOAL" & event_team == home_team), 
              xGF = sum(na.omit((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal)), 
              xGA = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal)),
              SF =  sum(event_type %in% st.shot_events & event_team == away_team), 
              SA =  sum(event_type %in% st.shot_events & event_team == home_team), 
              FF =  sum(event_type %in% st.fenwick_events & event_team == away_team),
              FA =  sum(event_type %in% st.fenwick_events & event_team == home_team), 
              CF =  sum(event_type %in% st.corsi_events & event_team == away_team),
              CA =  sum(event_type %in% st.corsi_events & event_team == home_team), 
              
              GA_adj =  sum((event_type == "GOAL" & event_team == home_team) * scr_adj_list$home_goal_adj[home_lead_state]), 
              xGA_adj = sum(na.omit((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal * scr_adj_list$home_xG_adj[home_lead_state])),
              SA_adj =  sum((event_type %in% st.shot_events & event_team == home_team) * scr_adj_list$home_shot_adj[home_lead_state]), 
              FA_adj =  sum((event_type %in% st.fenwick_events & event_team == home_team) * scr_adj_list$home_fenwick_adj[home_lead_state]), 
              CA_adj =  sum((event_type %in% st.corsi_events & event_team == home_team) * scr_adj_list$home_corsi_adj[home_lead_state]), 
              
              
              PEND2 = sum(na.omit(1 * (event_type == "PENL" & event_team == home_team) +
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == home_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              PENT2 = sum(na.omit(1 * (event_type == "PENL" & event_team == away_team) +
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail))) -
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_description))) - 
                                    1 * (event_type == "PENL" & event_team == away_team & grepl("(10 min)", tolower(event_description)))
                                  )
                          ),
              PEND5 = sum(na.omit(event_type == "PENL" & event_team == home_team & grepl("(5 min)", tolower(event_detail)))), 
              PENT5 = sum(na.omit(event_type == "PENL" & event_team == away_team & grepl("(5 min)", tolower(event_detail))))
              ) %>% 
    rename(Team = away_team) %>% 
    mutate(is_home = 0) %>% 
    data.frame()
  
  
  # Join
  mergetest <- Reduce(function(...) merge(..., all=TRUE), list(team_h, team_a))
  
  # By game
  games <- mergetest %>% 
    group_by(Team, Opponent, game_id, game_date, season, is_home) %>% 
    summarise_all(funs(sum)) %>% 
    select(Team, Opponent, game_id, game_date, season, is_home, 
           TOI, TOI_4v5:TOI_3v4, GF:CA, GA_adj:CA_adj, PEND2:PENT5
           ) %>% 
    arrange(Team, game_id) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
    mutate_if(is.logical, funs(replace(., is.na(.), 0))) %>% 
    data.frame()
  
  
  # Remove columns not used in below strength states
  if (strength == "SH") { 
    games <- games %>% 
      select(-c(GA, xGA, SA, FA, CA)) %>% 
      rename_at(vars(GA_adj, xGA_adj, SA_adj, FA_adj, CA_adj), funs(gsub("_adj", "", .))) %>% 
      select(Team, Opponent, game_id, game_date, season, is_home, 
             TOI, TOI_4v5:TOI_3v4, 
             GF, GA, xGF, xGA, SF, SA, FF, FA, CF, CA, 
             PEND2:PENT5)
    
    }
  else if (strength != "SH") { 
    games <- games %>% 
      select(-c(TOI_4v5:TOI_3v4))
    
    }
  
  return(games)
  
  }


####################################


## ----------------------------- ##
##   Team Game Labels Function   ##
## ----------------------------- ##

###################################

fun.team_game_labels <- function(pbp_data) { 
  
  return_df <- pbp_data %>% 
    group_by(home_team, away_team, season, game_id, game_date) %>% 
    summarise() %>% 
    rename(Team = home_team, 
           Opponent = away_team
           ) %>% 
    data.frame() %>%
    rbind(., pbp_data %>% 
            group_by(away_team, home_team, season, game_id, game_date) %>% 
            summarise() %>% 
            rename(Team = away_team, 
                   Opponent = home_team
                   ) %>% 
            data.frame()
          ) %>% 
    mutate(game_label = paste0(Team, " vs. ", Opponent, ", ", game_date)) %>% 
    arrange(game_id) %>% 
    data.frame()
  
  }


###################################




## ------------------------------------------------------ ##




## ------------------ ##
##   Sum Count Data   ##
## ------------------ ##

########################

# *** only per team per season available in function

# Count Calc Functions
fun.playercounts_season_all_sit <- function(data, position_data) { 
  
  games_sum <- data %>% 
    filter(TOI > 0) %>% 
    mutate(GP = 1) %>% 
    group_by(player, season, Team) %>% 
    summarise_at(vars(GP, TOI:t_TOI), funs(sum)) %>% 
    mutate(TOI_GP = TOI / GP, 
           TOI_perc = 100 * TOI / t_TOI, 
           FO_diff = FOW - FOL,
           
           Sh_perc =   ifelse(G > 0 & iSF > 0, 100 * (G / iSF), 0), 
           F_Sh_perc = ifelse(G > 0 & iFF > 0, 100 * (G / iFF), 0), 
           C_Sh_perc = ifelse(G > 0 & iCF > 0, 100 * (G / iCF), 0), 
           xFSH_perc = ifelse(ixG > 0 & iFF > 0, 100 * (ixG / iFF), 0), 
           
           OZS_perc = 100 * OZS / (OZS + NZS + DZS),
           NZS_perc = 100 * NZS / (OZS + NZS + DZS), 
           DZS_perc = 100 * DZS / (OZS + NZS + DZS), 
           ZS_rate =  100 * OZS / (OZS + DZS)
           ) %>% 
    left_join(., position_data, by = "player") %>% 
    select(player, position, season, Team, 
           GP, TOI, TOI_GP, TOI_perc, 
           G, A1, A2, Points,
           iSF, iFF, iCF, ixG,
           Sh_perc, F_Sh_perc, C_Sh_perc, xFSH_perc, 
           iBLK, GIVE, TAKE, iHF, iHA, 
           FOW, FOL, FO_diff, 
           iPENT2:iPEND5, 
           OZS, NZS, DZS, OZS_perc, DZS_perc, NZS_perc, ZS_rate, t_TOI
           ) %>% 
    mutate_if(is.numeric, funs(round(., 2))) %>% 
    data.frame()
  
  }

fun.playercounts_season_EV <- function(data, strength, per_60) {
  
  # Determine strength state
  if (strength == "EV") {
    
    GP_hold <- data %>% 
      filter(TOI > 0) %>% 
      group_by(player, season, Team) %>% 
      mutate(GP = n()) %>% 
      summarise(GP = first(GP)) %>% 
      data.frame()
    
    hold <- data %>% 
      group_by(player, season, Team) %>% 
      mutate(offTOI = t_TOI - TOI, 
             
             offGF =  t_GF - onGF, 
             offGA =  t_GA - onGA, 
             offSF =  t_SF - onSF, 
             offSA =  t_SA - onSA, 
             offFF =  t_FF - onFF, 
             offFA =  t_FA - onFA, 
             offCF =  t_CF - onCF, 
             offCA =  t_CA - onCA, 
             offxGF = t_xGF - onxGF, 
             offxGA = t_xGA - onxGA, 
             
             offGF_state =  t_GF_state - onGF_state, 
             offGA_state =  t_GA_state - onGA_state, 
             offSF_state =  t_SF_state - onSF_state, 
             offSA_state =  t_SA_state - onSA_state, 
             offFF_state =  t_FF_state - onFF_state, 
             offFA_state =  t_FA_state - onFA_state, 
             offCF_state =  t_CF_state - onCF_state, 
             offCA_state =  t_CA_state - onCA_state, 
             offxGF_state = t_xGF_state - onxGF_state, 
             offxGA_state = t_xGA_state - onxGA_state, 
             
             Tango = iCF - G
             ) %>% 
      summarise_at(vars(TOI:Tango), funs(sum)) %>% 
      left_join(., GP_hold, by = c("player", "season", "Team")) %>% 
      data.frame()
    
    
    position <- left_join(hold, player_position, by = c("player"))
    
    summed <- position %>% 
      mutate(TOI_GP =   TOI / GP, 
             TOI_perc = 100 * TOI / t_TOI, 
             FO_diff =  FOW - FOL,
             
             Sh_perc =   ifelse(G > 0 & iSF > 0, 100 * (G / iSF), 0), 
             F_Sh_perc = ifelse(G > 0 & iFF > 0, 100 * (G / iFF), 0), 
             C_Sh_perc = ifelse(G > 0 & iCF > 0, 100 * (G / iCF), 0), 
             xFSH_perc = ifelse(ixG > 0 & iFF > 0, 100 * (ixG / iFF), 0), 
             
             OZS_perc = 100 * OZS / (OZS + NZS + DZS),
             NZS_perc = 100 * NZS / (OZS + NZS + DZS), 
             DZS_perc = 100 * DZS / (OZS + NZS + DZS), 
             ZS_rate =  100 * OZS / (OZS + DZS), 
             
             GIVE =     GIVE_o + GIVE_n + GIVE_d, 
             TAKE =     TAKE_o + TAKE_n + TAKE_d, 
             iHF =      iHF_o + iHF_n + iHF_d, 
             iHA =      iHA_o + iHA_n + iHA_d, 
             
             G_diff =   onGF - onGA
             ) %>% 
      mutate_at(vars(GP, TOI_GP), funs(ifelse(is.na(.), 0, .))) %>% 
      mutate_if(is.numeric, funs(replace(., is.nan(.), 0))) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      select(player, position, season, Team, 
             GP, TOI, TOI_GP, TOI_perc, 
             G, A1, A2, Points, G_adj, A1_adj, A2_adj, Points_adj, 
             iSF, iFF, iCF, ixG, Tango, iCF_adj, ixG_adj, 
             Sh_perc, F_Sh_perc, C_Sh_perc, xFSH_perc, 
             iBLK, iBLK_adj, 
             GIVE_o:TAKE_d, GIVE, TAKE, GIVE_adj, TAKE_adj, 
             iHF_o:iHA_d, iHF, iHA, iHF_adj, iHA_adj,  
             FOW, FOL, FO_diff, 
             iPENT2:iPEND5, 
             OZS, NZS, DZS, OZS_perc, DZS_perc, NZS_perc, ZS_rate, 
             onGF:offxGA_state
             ) %>% 
      arrange(player, season) %>% 
      data.frame()
    
    if (per_60 %in% c("F", "FALSE")) { 
      return(summed)
      
      }
    
    }
  else if (strength != "EV") {
    
    GP_hold <- data %>% 
      filter(TOI > 0) %>% 
      group_by(player, season, Team) %>% 
      mutate(GP = n()) %>% 
      summarise(GP = first(GP)) %>% 
      data.frame()
    
    hold <- data %>% 
      group_by(player, season, Team) %>% 
      mutate(offTOI = t_TOI - TOI, 
             
             offGF =  t_GF - onGF, 
             offGA =  t_GA - onGA, 
             offSF =  t_SF - onSF, 
             offSA =  t_SA - onSA, 
             offFF =  t_FF - onFF, 
             offFA =  t_FA - onFA, 
             offCF =  t_CF - onCF, 
             offCA =  t_CA - onCA, 
             offxGF = t_xGF - onxGF, 
             offxGA = t_xGA - onxGA, 
             
             offGF_adj =  t_GF_adj - onGF_adj, 
             offGA_adj =  t_GA_adj - onGA_adj, 
             offSF_adj =  t_SF_adj - onSF_adj, 
             offSA_adj =  t_SA_adj - onSA_adj, 
             offFF_adj =  t_FF_adj - onFF_adj, 
             offFA_adj =  t_FA_adj - onFA_adj, 
             offCF_adj =  t_CF_adj - onCF_adj, 
             offCA_adj =  t_CA_adj - onCA_adj, 
             offxGF_adj = t_xGF_adj - onxGF_adj, 
             offxGA_adj = t_xGA_adj - onxGA_adj, 
             
             Tango = iCF - G
             ) %>% 
      summarise_at(vars(TOI:Tango), funs(sum)) %>% 
      left_join(., GP_hold, by = c("player", "season", "Team")) %>% 
      data.frame()
    
    position <- left_join(hold, player_position, by = c("player"))
    
    summed <- position %>% 
      mutate(TOI_GP =   TOI / GP, 
             TOI_perc = 100 * TOI / t_TOI, 
             FO_diff =  FOW - FOL,
             
             Sh_perc =   ifelse(G > 0 & iSF > 0, 100 * (G / iSF), 0), 
             F_Sh_perc = ifelse(G > 0 & iFF > 0, 100 * (G / iFF), 0), 
             C_Sh_perc = ifelse(G > 0 & iCF > 0, 100 * (G / iCF), 0), 
             xFSH_perc = ifelse(ixG > 0 & iFF > 0, 100 * (ixG / iFF), 0), 
             
             OZS_perc = 100 * OZS / (OZS + NZS + DZS),
             NZS_perc = 100 * NZS / (OZS + NZS + DZS), 
             DZS_perc = 100 * DZS / (OZS + NZS + DZS), 
             ZS_rate =  100 * OZS / (OZS + DZS), 
             
             GIVE =     GIVE_o + GIVE_n + GIVE_d, 
             TAKE =     TAKE_o + TAKE_n + TAKE_d, 
             iHF =      iHF_o + iHF_n + iHF_d, 
             iHA =      iHA_o + iHA_n + iHA_d, 
             
             G_diff =   onGF - onGA
             ) %>% 
      mutate_at(vars(GP, TOI_GP), funs(ifelse(is.na(.), 0, .))) %>% 
      mutate_if(is.numeric, funs(replace(., is.nan(.), 0))) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      select(player, position, season, Team, 
             GP, TOI, TOI_GP, TOI_perc, 
             G, A1, A2, Points, 
             iSF, iFF, iCF, ixG, Tango, 
             Sh_perc, F_Sh_perc, C_Sh_perc, xFSH_perc, 
             iBLK, 
             GIVE_o:TAKE_d, GIVE, TAKE, 
             iHF_o:iHA_d, iHF, iHA, 
             FOW, FOL, FO_diff, 
             iPENT2:iPEND5, 
             OZS, NZS, DZS, OZS_perc, DZS_perc, NZS_perc, ZS_rate, 
             onGF:offxGA_adj
             ) %>% 
      arrange(player, season) %>% 
      data.frame()
    
    if (per_60 %in% c("F", "FALSE")) { 
      return(summed)
      
      }
    
    }
  
  # Convert to Per 60
  if (per_60 %in% c("T", "TRUE") & strength == "EV") { 
    
    return_df <- summed %>% 
      mutate_at(vars(G:ixG_adj, iBLK:iHA_adj, iPENT2:iPEND5, onGF:onxGA_state), 
                funs((. / TOI) * 60)
                ) %>% 
      mutate_at(vars(t_GF:t_xGA_state), 
                funs((. / t_TOI) * 60)
                ) %>% 
      mutate_at(vars(offGF:offxGA_state), 
                funs((. / offTOI) * 60)
                )
    
    return(return_df)
    
    }
  
  }

fun.playercounts_season_PP <- function(data, strength, per_60) {
  
  # Determine strength state
  if (strength == "PP") {
    
    GP_hold <- data %>% 
      group_by(player, season, Team) %>% 
      mutate(GP = n()) %>% 
      summarise(GP = first(GP)) %>% 
      data.frame()
    
    hold <- data %>% 
      group_by(player, season, Team) %>% 
      mutate(offTOI = t_TOI - TOI, 
             
             offGF =  t_GF - onGF, 
             offGA =  t_GA - onGA, 
             offSF =  t_SF - onSF, 
             offSA =  t_SA - onSA, 
             offFF =  t_FF - onFF, 
             offFA =  t_FA - onFA, 
             offCF =  t_CF - onCF, 
             offCA =  t_CA - onCA, 
             offxGF = t_xGF - onxGF, 
             offxGA = t_xGA - onxGA, 
             
             offGF_state =  t_GF_state - onGF_state, 
             offGA_state =  t_GA_state - onGA_state, 
             offSF_state =  t_SF_state - onSF_state, 
             offSA_state =  t_SA_state - onSA_state, 
             offFF_state =  t_FF_state - onFF_state, 
             offFA_state =  t_FA_state - onFA_state, 
             offCF_state =  t_CF_state - onCF_state, 
             offCA_state =  t_CA_state - onCA_state, 
             offxGF_state = t_xGF_state - onxGF_state, 
             offxGA_state = t_xGA_state - onxGA_state, 
             
             Tango = iCF - G
             ) %>% 
      summarise_at(vars(TOI:Tango), funs(sum)) %>% 
      left_join(., GP_hold, by = c("player", "season", "Team")) %>% 
      data.frame()
    
    position <- left_join(hold, player_position, by = c("player"))
    
    summed <- position %>% 
      mutate(TOI_GP = TOI/GP, 
             TOI_perc = 100 * TOI / t_TOI, 
             FO_diff = FOW - FOL,
             
             Sh_perc = ifelse(G > 0 & iSF > 0, 100 * (G / iSF), 0), 
             F_Sh_perc = ifelse(G > 0 & iFF > 0, 100 * (G / iFF), 0), 
             C_Sh_perc = ifelse(G > 0 & iCF > 0, 100 * (G / iCF), 0), 
             xFSH_perc = ifelse(ixG > 0 & iFF > 0, 100 * (ixG / iFF), 0), 
             
             OZS_perc = 100 * OZS / (OZS + NZS + DZS),
             NZS_perc = 100 * NZS / (OZS + NZS + DZS), 
             DZS_perc = 100 * DZS / (OZS + NZS + DZS), 
             ZS_rate =  100 * OZS / (OZS + DZS), 
             
             GIVE =     GIVE_o + GIVE_n + GIVE_d, 
             TAKE =     TAKE_o + TAKE_n + TAKE_d, 
             iHF =      iHF_o + iHF_n + iHF_d, 
             iHA =      iHA_o + iHA_n + iHA_d, 
             
             G_diff =   onGF - onGA
             ) %>% 
      mutate_at(vars(GP, TOI_GP), funs(ifelse(is.na(.), 0, .))) %>% 
      mutate_if(is.numeric, funs(replace(., is.nan(.), 0))) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      select(player, position, season, Team, 
             GP, TOI, TOI_GP, TOI_perc, 
             G, A1, A2, Points, G_adj, A1_adj, A2_adj, Points_adj, 
             iSF, iFF, iCF, ixG, Tango, iCF_adj, ixG_adj, 
             Sh_perc, F_Sh_perc, C_Sh_perc, xFSH_perc, 
             GIVE_o:TAKE_d, GIVE, TAKE, GIVE_adj, TAKE_adj, 
             iHF_o:iHA_d, iHF, iHA, iHF_adj, iHA_adj,  
             FOW, FOL, FO_diff, 
             iPENT2:iPEND5, 
             OZS, NZS, DZS, OZS_perc, DZS_perc, NZS_perc, ZS_rate, 
             onGF:offxGA_state
             ) %>% 
      arrange(player, season) %>% 
      data.frame()
    
    if (per_60 %in% c("F", "FALSE")) { 
      return(summed)
      }
    
    }
  else if (strength != "PP") {
    
    GP_hold <- data %>% 
      group_by(player, season, Team) %>% 
      mutate(GP = n()) %>% 
      summarise(GP = first(GP)) %>% 
      data.frame()
    
    hold <- data %>% 
      group_by(player, season, Team) %>% 
      mutate(offTOI = t_TOI - TOI, 
             
             offGF =  t_GF - onGF, 
             offGA =  t_GA - onGA, 
             offSF =  t_SF - onSF, 
             offSA =  t_SA - onSA, 
             offFF =  t_FF - onFF, 
             offFA =  t_FA - onFA, 
             offCF =  t_CF - onCF, 
             offCA =  t_CA - onCA, 
             offxGF = t_xGF - onxGF, 
             offxGA = t_xGA - onxGA, 
             
             offGF_adj =  t_GF_adj - onGF_adj, 
             offSF_adj =  t_SF_adj - onSF_adj, 
             offFF_adj =  t_FF_adj - onFF_adj, 
             offCF_adj =  t_CF_adj - onCF_adj, 
             offxGF_adj = t_xGF_adj - onxGF_adj, 
             
             Tango = iCF - G
             ) %>% 
      summarise_at(vars(TOI:Tango), funs(sum)) %>% 
      left_join(., GP_hold, by = c("player", "season", "Team")) %>% 
      data.frame()
    
    position <- left_join(hold, player_position, by = c("player"))
    
    summed <- position %>% 
      mutate(TOI_GP = TOI/GP, 
             TOI_perc = 100 * TOI / t_TOI, 
             FO_diff = FOW - FOL,
             
             Sh_perc = ifelse(G > 0 & iSF > 0, 100 * (G / iSF), 0), 
             F_Sh_perc = ifelse(G > 0 & iFF > 0, 100 * (G / iFF), 0), 
             C_Sh_perc = ifelse(G > 0 & iCF > 0, 100 * (G / iCF), 0), 
             xFSH_perc = ifelse(ixG > 0 & iFF > 0, 100 * (ixG / iFF), 0), 
             
             OZS_perc = 100 * OZS / (OZS + NZS + DZS),
             NZS_perc = 100 * NZS / (OZS + NZS + DZS), 
             DZS_perc = 100 * DZS / (OZS + NZS + DZS), 
             ZS_rate =  100 * OZS / (OZS + DZS), 
             
             GIVE =     GIVE_o + GIVE_n + GIVE_d, 
             TAKE =     TAKE_o + TAKE_n + TAKE_d, 
             iHF =      iHF_o + iHF_n + iHF_d, 
             iHA =      iHA_o + iHA_n + iHA_d, 
             
             G_diff =   onGF - onGA
             ) %>% 
      mutate_at(vars(GP, TOI_GP), funs(ifelse(is.na(.), 0, .))) %>% 
      mutate_if(is.numeric, funs(replace(., is.nan(.), 0))) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      select(player, position, season, Team, 
             GP, TOI, TOI_GP, TOI_perc, 
             G, A1, A2, Points, 
             iSF, iFF, iCF, ixG, Tango, 
             Sh_perc, F_Sh_perc, C_Sh_perc, xFSH_perc, 
             GIVE_o:TAKE_d, GIVE, TAKE, 
             iHF_o:iHA_d, iHF, iHA, 
             FOW, FOL, FO_diff, 
             iPENT2:iPEND5, 
             OZS, NZS, DZS, OZS_perc, DZS_perc, NZS_perc, ZS_rate, 
             onGF:offxGF_adj
             ) %>% 
      arrange(player, season) %>% 
      data.frame()
    
    if (per_60 %in% c("F", "FALSE")) { 
      return(summed)
      }
    
    }
  
  # Convert to Per 60
  if (per_60 %in% c("T", "TRUE") & strength == "PP") { 
    
    return_df <- summed %>% 
      mutate_at(vars(G:ixG_adj, GIVE_o:iHA_adj, iPENT2:iPEND5, onGF:onxGA_state), 
                funs((. / TOI) * 60)
                ) %>% 
      mutate_at(vars(t_GF:t_xGA_state), 
                funs((. / t_TOI) * 60)
                ) %>% 
      mutate_at(vars(offGF:offxGA_state), 
                funs((. / offTOI) * 60)
                )
    
    return(return_df)
    
    }
  
  }

fun.playercounts_season_SH <- function(data, strength, per_60) {
  
  # Determine strength state
  if (strength == "SH") {
    
    GP_hold <- data %>% 
      group_by(player, season, Team) %>% 
      mutate(GP = n()) %>% 
      summarise(GP = first(GP)) %>% 
      data.frame()
    
    hold <- data %>% 
      group_by(player, season, Team) %>% 
      mutate(offTOI = t_TOI - TOI, 
             
             offGF =  t_GF - onGF, 
             offGA =  t_GA - onGA, 
             offSF =  t_SF - onSF, 
             offSA =  t_SA - onSA, 
             offFF =  t_FF - onFF, 
             offFA =  t_FA - onFA, 
             offCF =  t_CF - onCF, 
             offCA =  t_CA - onCA, 
             offxGF = t_xGF - onxGF, 
             offxGA = t_xGA - onxGA, 
             
             offGF_state =  t_GF_state - onGF_state, 
             offGA_state =  t_GA_state - onGA_state, 
             offSF_state =  t_SF_state - onSF_state, 
             offSA_state =  t_SA_state - onSA_state, 
             offFF_state =  t_FF_state - onFF_state, 
             offFA_state =  t_FA_state - onFA_state, 
             offCF_state =  t_CF_state - onCF_state, 
             offCA_state =  t_CA_state - onCA_state, 
             offxGF_state = t_xGF_state - onxGF_state, 
             offxGA_state = t_xGA_state - onxGA_state, 
             
             Tango = iCF - G
             ) %>% 
      summarise_at(vars(TOI:Tango), funs(sum)) %>% 
      left_join(., GP_hold, by = c("player", "season", "Team")) %>% 
      data.frame()
    
    position <- left_join(hold, player_position, by = c("player"))
    
    summed <- position %>% 
      mutate(TOI_GP = TOI/GP, 
             TOI_perc = 100 * TOI / t_TOI, 
             FO_diff = FOW - FOL,
             
             Sh_perc = ifelse(G > 0 & iSF > 0, 100 * (G / iSF), 0), 
             F_Sh_perc = ifelse(G > 0 & iFF > 0, 100 * (G / iFF), 0), 
             C_Sh_perc = ifelse(G > 0 & iCF > 0, 100 * (G / iCF), 0), 
             xFSH_perc = ifelse(ixG > 0 & iFF > 0, 100 * (ixG / iFF), 0), 
             
             OZS_perc = 100 * OZS / (OZS + NZS + DZS),
             NZS_perc = 100 * NZS / (OZS + NZS + DZS), 
             DZS_perc = 100 * DZS / (OZS + NZS + DZS), 
             ZS_rate =  100 * OZS / (OZS + DZS), 
             
             GIVE =     GIVE_o + GIVE_n + GIVE_d, 
             TAKE =     TAKE_o + TAKE_n + TAKE_d, 
             iHF =      iHF_o + iHF_n + iHF_d, 
             iHA =      iHA_o + iHA_n + iHA_d, 
             
             G_diff =   onGF - onGA
             ) %>% 
      mutate_at(vars(GP, TOI_GP), funs(ifelse(is.na(.), 0, .))) %>% 
      mutate_if(is.numeric, funs(replace(., is.nan(.), 0))) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      select(player, position, season, Team, 
             GP, TOI, TOI_GP, TOI_perc, 
             G, A1, A2, Points, 
             iSF, iFF, iCF, ixG, Tango, 
             Sh_perc, F_Sh_perc, C_Sh_perc, xFSH_perc, 
             iBLK, iBLK_adj, 
             GIVE_o:TAKE_d, GIVE, TAKE, GIVE_adj, TAKE_adj, 
             iHF_o:iHA_d, iHF, iHA, iHF_adj, iHA_adj,  
             FOW, FOL, FO_diff, 
             iPENT2:iPEND5, 
             OZS, NZS, DZS, OZS_perc, DZS_perc, NZS_perc, ZS_rate, 
             onGF:offxGA_state
             ) %>% 
      arrange(player, season) %>% 
      data.frame()
    
    if (per_60 %in% c("F", "FALSE")) { 
      return(summed)
      }
    
    } 
  else if (strength != "SH") {
    
    GP_hold <- data %>% 
      group_by(player, season, Team) %>% 
      mutate(GP = n()) %>% 
      summarise(GP = first(GP)) %>% 
      data.frame()
    
    hold <- data %>% 
      group_by(player, season, Team) %>% 
      mutate(offTOI = t_TOI - TOI, 
             
             offGF =  t_GF - onGF, 
             offGA =  t_GA - onGA, 
             offSF =  t_SF - onSF, 
             offSA =  t_SA - onSA, 
             offFF =  t_FF - onFF, 
             offFA =  t_FA - onFA, 
             offCF =  t_CF - onCF, 
             offCA =  t_CA - onCA, 
             offxGF = t_xGF - onxGF, 
             offxGA = t_xGA - onxGA, 
             
             offGA_adj =  t_GA_adj - onGA_adj, 
             offSA_adj =  t_SA_adj - onSA_adj, 
             offFA_adj =  t_FA_adj - onFA_adj, 
             offCA_adj =  t_CA_adj - onCA_adj, 
             offxGA_adj = t_xGA_adj - onxGA_adj, 
             
             Tango = iCF - G
             ) %>% 
      summarise_at(vars(TOI:Tango), funs(sum)) %>% 
      left_join(., GP_hold, by = c("player", "season", "Team")) %>% 
      data.frame()
    
    position <- left_join(hold, player_position, by = c("player"))
    
    summed <- position %>% 
      mutate(TOI_GP = TOI/GP, 
             TOI_perc = 100 * TOI / t_TOI, 
             FO_diff = FOW - FOL,
             
             Sh_perc = ifelse(G > 0 & iSF > 0, 100 * (G / iSF), 0), 
             F_Sh_perc = ifelse(G > 0 & iFF > 0, 100 * (G / iFF), 0), 
             C_Sh_perc = ifelse(G > 0 & iCF > 0, 100 * (G / iCF), 0), 
             xFSH_perc = ifelse(ixG > 0 & iFF > 0, 100 * (ixG / iFF), 0), 
             
             OZS_perc = 100 * OZS / (OZS + NZS + DZS),
             NZS_perc = 100 * NZS / (OZS + NZS + DZS), 
             DZS_perc = 100 * DZS / (OZS + NZS + DZS), 
             ZS_rate =  100 * OZS / (OZS + DZS), 
             
             GIVE =     GIVE_o + GIVE_n + GIVE_d, 
             TAKE =     TAKE_o + TAKE_n + TAKE_d, 
             iHF =      iHF_o + iHF_n + iHF_d, 
             iHA =      iHA_o + iHA_n + iHA_d, 
             
             G_diff =   onGF - onGA
             ) %>% 
      mutate_at(vars(GP, TOI_GP), funs(ifelse(is.na(.), 0, .))) %>% 
      mutate_if(is.numeric, funs(replace(., is.nan(.), 0))) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      select(player, position, season, Team, 
             GP, TOI, TOI_GP, TOI_perc, 
             G, A1, A2, Points, 
             iSF, iFF, iCF, ixG, Tango, 
             Sh_perc, F_Sh_perc, C_Sh_perc, xFSH_perc, 
             iBLK, 
             GIVE_o:TAKE_d, GIVE, TAKE, 
             iHF_o:iHA_d, iHF, iHA, 
             FOW, FOL, FO_diff, 
             iPENT2:iPEND5, 
             OZS, NZS, DZS, OZS_perc, DZS_perc, NZS_perc, ZS_rate, 
             onGF:offxGA_adj
             ) %>% 
      arrange(player, season) %>% 
      data.frame()
    
    if (per_60 %in% c("F", "FALSE")) { 
      return(summed)
      }
    
    }
  
  # Convert to Per 60
  if (per_60 %in% c("T", "TRUE") & strength == "SH") { 
    
    return_df <- summed %>% 
      mutate_at(vars(G:Tango, iBLK:iHA_adj, iPENT2:iPEND5, onGF:onxGA_state), 
                funs((. / TOI) * 60)
                ) %>% 
      mutate_at(vars(t_GF:t_xGA_state), 
                funs((. / t_TOI) * 60)
                ) %>% 
      mutate_at(vars(offGF:offxGA_state), 
                funs((. / offTOI) * 60)
                )
    
    return(return_df)
    
    }
  
  }


########################


## ----------------------------- ##
##   Sum Rel_TM / WOWY Metrics   ##
## ----------------------------- ##

##################################

# Function
fun.relative_teammate <- function(TM_data, games_data, position_data, strength) { 
  
  if (strength == "even") { 
    
    # Player on-ice raw and per 60 numbers
    player_metrics_EV <- games_data %>% 
      group_by(player, season, Team) %>% 
      mutate(GP = 1) %>% 
      summarise_at(vars(TOI, GP, onGF:onxGA, onGF_state:onxGA_state), 
                   funs(sum)
                   ) %>% 
      mutate(onSH.perc = ifelse(onGF > 0 & onSF > 0, 100 * (onGF / onSF), 0)) %>% # on-ice shooting % relative to teammate
      mutate_at(vars(onGF:onxGA, onGF_state:onxGA_state), # convert columns to per 60
                funs((. / TOI) * 60)
                ) %>% 
      ungroup() %>% 
      
      rename_at(vars(onGF:onxGA, onGF_state:onxGA_state), # rename to per 60
                funs(paste0(gsub("on", "",.), "60"))
                ) %>% 
      left_join(., position_data, by = "player") %>% 
      data.frame()
    
    # Teammate on-ice raw and per 60 numbers
    teammate_metrics_EV <- player_metrics_EV %>% 
      rename(teammate = player)
    
    # Calculate WOWY data
    WOWY_df <- TM_data %>% 
      rename(TOI_tog = TOI) %>% 
      group_by(player, teammate, season, Team) %>% 
      summarise(TOI_tog = sum(TOI_tog)) %>% 
      ungroup() %>% 
      left_join(., player_metrics_EV, by = c("player", "season", "Team")) %>% # join player metrics
      left_join(., teammate_metrics_EV, by = c("teammate", "season", "Team"), suffix = c("_p", "_t")) %>% # join teammate metrics
      mutate(player_TOI_perc_w = TOI_tog / TOI_p) %>% 
      mutate_at(vars(ends_with("_t")), # add "weighted raw" columns 
                .funs = funs(weighted = . * player_TOI_perc_w)
                ) %>% 
      data.frame()
    
    # Calculate relative to teammate metrics
    joined_df_EV <- WOWY_df %>% 
      group_by_at(vars(player, position_p, season, Team, TOI_p, GF60_p:xGA60_p, onSH.perc_p, GF_state60_p:xGA_state60_p)) %>% 
      summarise_at(vars(player_TOI_perc_w, GF60_t_weighted:xGA60_t_weighted, onSH.perc_t_weighted, GF_state60_t_weighted:xGA_state60_t_weighted), # sum "weighted raw" columns
                   funs(sum)
                   ) %>% 
      ungroup() %>% 
      mutate_at(vars(GF60_t_weighted:xGA60_t_weighted, onSH.perc_t_weighted, GF_state60_t_weighted:xGA_state60_t_weighted), # finalzie weighted avg of teammates
                funs(. / player_TOI_perc_w)
                ) %>% 
      rename_at(vars(GF60_p:xGA60_p, onSH.perc_p, GF_state60_p:xGA_state60_p), # rename players
                funs(gsub("_p", "",.))
                ) %>% 
      rename_at(vars(GF60_t_weighted:xGA60_t_weighted, onSH.perc_t_weighted, GF_state60_t_weighted:xGA_state60_t_weighted), # rename teammates
                funs(paste0("w_TM_", gsub("_t_weighted", "",.)))
                ) %>% 
      rename_at(vars(contains("_state")), # fix state adj names
                funs(gsub("_state60", "60_state", .))
                ) %>% 
      mutate(rel_TM_GF60 =  GF60 - w_TM_GF60, 
             rel_TM_GA60 =  GA60 - w_TM_GA60, 
             rel_TM_SF60 =  SF60 - w_TM_SF60, 
             rel_TM_SA60 =  SA60 - w_TM_SA60, 
             rel_TM_FF60 =  FF60 - w_TM_FF60, 
             rel_TM_FA60 =  FA60 - w_TM_FA60, 
             rel_TM_CF60 =  CF60 - w_TM_CF60, 
             rel_TM_CA60 =  CA60 - w_TM_CA60, 
             rel_TM_xGF60 = xGF60 - w_TM_xGF60, 
             rel_TM_xGA60 = xGA60 - w_TM_xGA60, 
             
             rel_TM_SH_perc = onSH.perc - w_TM_onSH.perc, 
             
             rel_TM_GF60_state =  GF60_state - w_TM_GF60_state, 
             rel_TM_GA60_state =  GA60_state - w_TM_GA60_state, 
             rel_TM_SF60_state =  SF60_state - w_TM_SF60_state, 
             rel_TM_SA60_state =  SA60_state - w_TM_SA60_state, 
             rel_TM_FF60_state =  FF60_state - w_TM_FF60_state, 
             rel_TM_FA60_state =  FA60_state - w_TM_FA60_state, 
             rel_TM_CF60_state =  CF60_state - w_TM_CF60_state, 
             rel_TM_CA60_state =  CA60_state - w_TM_CA60_state, 
             rel_TM_xGF60_state = xGF60_state - w_TM_xGF60_state, 
             rel_TM_xGA60_state = xGA60_state - w_TM_xGA60_state
             ) %>% 
      data.frame()
    
    # Create impact numbers and clean up
    rel_impact_EV <- joined_df_EV %>% 
      mutate_at(vars(contains("rel_TM_")), # create expanded "impact" columns
                .funs = funs(impact = round((. / 60) * TOI_p, 2))
                ) %>% 
      rename_at(vars(rel_TM_GF60_impact:rel_TM_xGA60_impact, rel_TM_GF60_state_impact:rel_TM_xGA60_state_impact), 
                funs(gsub("60", "", .))
                ) %>% 
      select(player, position_p, season, Team, TOI_p, 
             GF60:xGA60,
             GF60_state:xGA60_state,
             
             rel_TM_GF60:rel_TM_xGA60,
             rel_TM_SH_perc, 
             rel_TM_GF60_state:rel_TM_xGA60_state,
             
             rel_TM_GF_impact:rel_TM_xGA_impact, 
             rel_TM_GF_state_impact:rel_TM_xGA_state_impact
             ) %>% 
      rename(position = position_p, 
             TOI = TOI_p
             ) %>% 
      mutate_if(is.numeric, funs(round(., 3))) %>% 
      mutate(TOI = round(TOI, 2)) %>% 
      data.frame()
    
    
    return_list <- list(WOWY_data =   WOWY_df, 
                        rel_TM_data = rel_impact_EV)
    
    return(return_list)
    
    }
  
  if (strength == "powerplay") { 
  
    # Calculate relative to teammate
    player_metrics_PP <- games_data %>% 
      filter(TOI > 0) %>% 
      group_by(player, season, Team) %>% 
      mutate(GP = 1) %>% 
      summarise_at(vars(TOI, GP, onGF, onSF, onFF, onCF, onxGF, onGF_state, onSF_state, onFF_state, onCF_state, onxGF_state), 
                   funs(sum)
                   ) %>% 
      mutate(onSH.perc = ifelse(onGF > 0 & onSF > 0, 100 * (onGF / onSF), 0)) %>% # for on-ice shooting % relative to teammate
      mutate_at(vars(onGF, onSF, onFF, onCF, onxGF, onGF_state, onSF_state, onFF_state, onCF_state, onxGF_state), # convert columns to per 60
                funs((. / TOI) * 60)
                ) %>% 
      ungroup() %>% 
      rename_at(vars(onGF, onSF, onFF, onCF, onxGF, onGF_state, onSF_state, onFF_state, onCF_state, onxGF_state), # rename to per 60
                funs(paste0(gsub("on", "",.), "60"))
                ) %>% 
      left_join(., position_data, by = "player") %>% 
      data.frame()
    
    # Teammate on-ice raw and per 60 numbers
    teammate_metrics_PP <- player_metrics_PP %>% 
      #select(-c(position, GP, TOI)) %>% 
      rename(teammate = player)
    
    # Calculate relative to teammate numbers
    WOWY_df <- TM_data %>% 
      rename(TOI_tog = TOI) %>% 
      group_by(player, teammate, season, Team) %>% 
      summarise(TOI_tog = sum(TOI_tog)) %>% 
      ungroup() %>% 
      left_join(., player_metrics_PP, by = c("player", "season", "Team")) %>% # join player metrics
      left_join(., teammate_metrics_PP, by = c("teammate", "season", "Team"), suffix = c("_p", "_t")) %>% # join teammate metrics
      mutate(player_TOI_perc_w = TOI_tog / TOI_p) %>% 
      mutate_at(vars(ends_with("_t")), # add "weighted raw" columns 
                .funs = funs(weighted = . * player_TOI_perc_w)
                ) %>% 
      data.frame()
    
    joined_df_PP <- WOWY_df %>% 
      group_by_at(vars(player, position_p, season, Team, TOI_p, GF60_p:xGF60_p, GF_state60_p:xGF_state60_p, onSH.perc_p)) %>% 
      summarise_at(vars(player_TOI_perc_w, GF60_t_weighted:xGF60_t_weighted, GF_state60_t_weighted:xGF_state60_t_weighted, onSH.perc_t_weighted), 
                   funs(sum)
                   ) %>% 
      ungroup() %>% 
      mutate_at(vars(GF60_t_weighted:xGF60_t_weighted, GF_state60_t_weighted:xGF_state60_t_weighted, onSH.perc_t_weighted), # finalzie weighted avg of teammates
                funs(. / player_TOI_perc_w)
                ) %>% 
      rename_at(vars(GF60_p:xGF60_p, GF_state60_p:xGF_state60_p, onSH.perc_p), 
                funs(gsub("_p", "",.))
                ) %>% 
      rename_at(vars(GF60_t_weighted:xGF60_t_weighted, GF_state60_t_weighted:xGF_state60_t_weighted, onSH.perc_t_weighted), 
                funs(paste0("w_TM_", gsub("_t_weighted", "",.)))
                ) %>% 
      rename_at(vars(contains("_state")), # fix state adj names
                funs(gsub("_state60", "60_state", .))
                ) %>% 
      mutate(rel_TM_GF60 =  GF60 - w_TM_GF60, 
             rel_TM_SF60 =  SF60 - w_TM_SF60, 
             rel_TM_FF60 =  FF60 - w_TM_FF60, 
             rel_TM_CF60 =  CF60 - w_TM_CF60, 
             rel_TM_xGF60 = xGF60 - w_TM_xGF60, 
             rel_TM_GF60_state =  GF60_state - w_TM_GF60_state, 
             rel_TM_SF60_state =  SF60_state - w_TM_SF60_state, 
             rel_TM_FF60_state =  FF60_state - w_TM_FF60_state, 
             rel_TM_CF60_state =  CF60_state - w_TM_CF60_state, 
             rel_TM_xGF60_state = xGF60_state - w_TM_xGF60_state, 
             
             rel_TM_SH_perc = onSH.perc - w_TM_onSH.perc
             ) %>% 
      data.frame()
    
    # Create impact numbers and clean up
    rel_impact_PP <- joined_df_PP %>% 
      mutate_at(vars(contains("rel_TM_")), # create expanded "impact" columns
                .funs = funs(impact = round((. / 60) * TOI_p, 2))
                ) %>% 
      rename_at(vars(rel_TM_GF60_impact:rel_TM_xGF60_impact, rel_TM_GF60_state_impact:rel_TM_xGF60_state_impact), 
                funs(gsub("60", "", .))
                ) %>% 
      select(player, position_p, season, Team, TOI_p, 
             GF60:xGF60, 
             GF60_state:xGF60_state, 
             rel_TM_GF60:rel_TM_xGF60, rel_TM_GF60_state:rel_TM_xGF60_state, 
             rel_TM_GF_impact:rel_TM_xGF_impact, rel_TM_GF_state_impact:rel_TM_xGF_state_impact
             ) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      rename(position = position_p, 
             TOI = TOI_p
             ) %>% 
      data.frame()
    
    return_list <- list(WOWY_data =   WOWY_df, 
                        rel_TM_data = rel_impact_PP)
    
    return(return_list)
    
    }
  
  if (strength == "shorthanded") { 
  
    # SH - Relative to Teammate "New"
    player_metrics_SH <- games_data %>% 
      filter(TOI > 0) %>% 
      group_by(player, season, Team) %>% 
      mutate(GP = 1) %>% 
      summarise_at(vars(TOI, GP, onGA, onSA, onFA, onCA, onxGA, onGA_state, onSA_state, onFA_state, onCA_state, onxGA_state), 
                   funs(sum)
                   ) %>% 
      mutate_at(vars(onGA, onSA, onFA, onCA, onxGA, onGA_state, onSA_state, onFA_state, onCA_state, onxGA_state), # convert columns to per 60
                funs((. / TOI) * 60)
                ) %>% 
      ungroup() %>% 
      rename_at(vars(onGA, onSA, onFA, onCA, onxGA, onGA_state, onSA_state, onFA_state, onCA_state, onxGA_state), # rename to per 60
                funs(paste0(gsub("on", "",.), "60"))
                ) %>% 
      left_join(., position_data, by = "player") %>% 
      data.frame()
    
    # Teammate on-ice raw and per 60 numbers
    teammate_metrics_SH <- player_metrics_SH %>% 
      #select(-c(position, GP, TOI)) %>% 
      rename(teammate = player)
    
    # Calculate relative to teammate numbers
    WOWY_df <- TM_data %>% 
      rename(TOI_tog = TOI) %>% 
      group_by(player, teammate, season, Team) %>% 
      summarise(TOI_tog = sum(TOI_tog)) %>% 
      ungroup() %>% 
      left_join(., player_metrics_SH, by = c("player", "season", "Team")) %>% # join player metrics
      left_join(., teammate_metrics_SH, by = c("teammate", "season", "Team"), suffix = c("_p", "_t")) %>% # join teammate metrics
      mutate(player_TOI_perc_w = TOI_tog / TOI_p) %>% 
      mutate_at(vars(ends_with("_t")), # add "weighted raw" columns 
                .funs = funs(weighted = . * player_TOI_perc_w)
                ) %>% 
      data.frame()
    
    joined_df_SH <- WOWY_df %>% 
      group_by_at(vars(player, position_p, season, Team, TOI_p, GA60_p:xGA60_p, GA_state60_p:xGA_state60_p)) %>% 
      summarise_at(vars(player_TOI_perc_w, GA60_t_weighted:xGA60_t_weighted, GA_state60_t_weighted:xGA_state60_t_weighted), 
                   funs(sum)
                   ) %>% 
      ungroup() %>% 
      mutate_at(vars(GA60_t_weighted:xGA60_t_weighted, GA_state60_t_weighted:xGA_state60_t_weighted), # finalzie weighted avg of teammates
                funs(. / player_TOI_perc_w)
                ) %>% 
      rename_at(vars(GA60_p:xGA60_p, GA_state60_p:xGA_state60_p), 
                funs(gsub("_p", "",.))
                ) %>% 
      rename_at(vars(GA60_t_weighted:xGA60_t_weighted, GA_state60_t_weighted:xGA_state60_t_weighted), 
                funs(paste0("w_TM_", gsub("_t_weighted", "",.)))
                ) %>% 
      rename_at(vars(contains("_state")), # fix state adj names
                funs(gsub("_state60", "60_state", .))
                ) %>% 
      mutate(rel_TM_GA60 =  GA60 - w_TM_GA60, 
             rel_TM_SA60 =  SA60 - w_TM_SA60, 
             rel_TM_FA60 =  FA60 - w_TM_FA60, 
             rel_TM_CA60 =  CA60 - w_TM_CA60, 
             rel_TM_xGA60 = xGA60 - w_TM_xGA60, 
             
             rel_TM_GA60_state =  GA60_state - w_TM_GA60_state, 
             rel_TM_SA60_state =  SA60_state - w_TM_SA60_state, 
             rel_TM_FA60_state =  FA60_state - w_TM_FA60_state, 
             rel_TM_CA60_state =  CA60_state - w_TM_CA60_state, 
             rel_TM_xGA60_state = xGA60_state - w_TM_xGA60_state
             ) %>% 
      data.frame()
    
    # Create impact numbers and clean up
    rel_impact_SH <- joined_df_SH %>% 
      mutate_at(vars(contains("rel_TM_")), # create expanded "impact" columns
                .funs = funs(impact = round((. / 60) * TOI_p, 2))
                ) %>%  
      rename_at(vars(rel_TM_GA60_impact:rel_TM_xGA60_impact, rel_TM_GA60_state_impact:rel_TM_xGA60_state_impact), 
                funs(gsub("60", "", .))
                ) %>% 
      select(player, position_p, season, Team, TOI_p, 
             GA60:xGA60, 
             GA60_state:xGA60_state, 
             rel_TM_GA60:rel_TM_xGA60, rel_TM_GA60_state:rel_TM_xGA60_state, 
             rel_TM_GA_impact:rel_TM_xGA_impact, rel_TM_GA_state_impact:rel_TM_xGA_state_impact
             ) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      rename(position = position_p, 
             TOI = TOI_p
             ) %>% 
      data.frame()
    
    # Return list
    return_list <- list(WOWY_data =   WOWY_df, 
                        rel_TM_data = rel_impact_SH)
    
    return(return_list)
    
    }
  
  if (strength == "5v5") { 
    
    # Player on-ice raw and per 60 numbers
    player_metrics_EV <- games_data %>% 
      select(-c(onGF:onxGA)) %>% 
      rename_at(vars(onGF_adj:onxGA_adj), funs(gsub("_adj", "", .))) %>% 
      group_by(player, season, Team) %>% 
      mutate(GP = 1) %>% 
      summarise_at(vars(TOI, GP, onGF:onxGA), 
                   funs(sum)
                   ) %>% 
      mutate(onSH.perc = ifelse(onGF > 0 & onSF > 0, 100 * (onGF / onSF), 0)) %>% # on-ice shooting % relative to teammate
      mutate_at(vars(onGF:onxGA), # convert columns to per 60
                funs((. / TOI) * 60)
                ) %>% 
      ungroup() %>% 
      
      rename_at(vars(onGF:onxGA), # rename to per 60
                funs(paste0(gsub("on", "",.), "60"))
                ) %>% 
      left_join(., position_data, by = "player") %>% 
      data.frame()
    
    # Teammate on-ice raw and per 60 numbers
    teammate_metrics_EV <- player_metrics_EV %>% 
      rename(teammate = player)
    
    # Calculate WOWY data
    WOWY_df <- TM_data %>% 
      rename(TOI_tog = TOI) %>% 
      group_by(player, teammate, season, Team) %>% 
      summarise(TOI_tog = sum(TOI_tog)) %>% 
      ungroup() %>% 
      left_join(., player_metrics_EV, by = c("player", "season", "Team")) %>% # join player metrics
      left_join(., teammate_metrics_EV, by = c("teammate", "season", "Team"), suffix = c("_p", "_t")) %>% # join teammate metrics
      mutate(player_TOI_perc_w = TOI_tog / TOI_p) %>% 
      mutate_at(vars(ends_with("_t")), # add "weighted raw" columns 
                .funs = funs(weighted = . * player_TOI_perc_w)
                ) %>% 
      data.frame()
    
    # Calculate relative to teammate metrics
    joined_df_EV <- WOWY_df %>% 
      group_by_at(vars(player, position_p, season, Team, TOI_p, GF60_p:xGA60_p, onSH.perc_p)) %>% 
      summarise_at(vars(player_TOI_perc_w, GF60_t_weighted:xGA60_t_weighted, onSH.perc_t_weighted), # sum "weighted raw" columns
                   funs(sum)
                   ) %>% 
      ungroup() %>% 
      mutate_at(vars(GF60_t_weighted:xGA60_t_weighted, onSH.perc_t_weighted), # finalzie weighted avg of teammates
                funs(. / player_TOI_perc_w)
                ) %>% 
      rename_at(vars(GF60_p:xGA60_p, onSH.perc_p), # rename players
                funs(gsub("_p", "",.))
                ) %>% 
      rename_at(vars(GF60_t_weighted:xGA60_t_weighted, onSH.perc_t_weighted), # rename teammates
                funs(paste0("w_TM_", gsub("_t_weighted", "",.)))
                ) %>% 
      mutate(rel_TM_GF60 =  GF60 - w_TM_GF60, 
             rel_TM_GA60 =  GA60 - w_TM_GA60, 
             rel_TM_SF60 =  SF60 - w_TM_SF60, 
             rel_TM_SA60 =  SA60 - w_TM_SA60, 
             rel_TM_FF60 =  FF60 - w_TM_FF60, 
             rel_TM_FA60 =  FA60 - w_TM_FA60, 
             rel_TM_CF60 =  CF60 - w_TM_CF60, 
             rel_TM_CA60 =  CA60 - w_TM_CA60, 
             rel_TM_xGF60 = xGF60 - w_TM_xGF60, 
             rel_TM_xGA60 = xGA60 - w_TM_xGA60, 
             
             rel_TM_SH_perc = onSH.perc - w_TM_onSH.perc
             ) %>% 
      data.frame()
    
    # Create impact numbers and clean up
    rel_impact_EV <- joined_df_EV %>% 
      mutate_at(vars(contains("rel_TM_")), # create expanded "impact" columns
                .funs = funs(impact = round((. / 60) * TOI_p, 2))
                ) %>% 
      rename_at(vars(rel_TM_GF60_impact:rel_TM_xGA60_impact), 
                funs(gsub("60", "", .))
                ) %>% 
      select(player, position_p, season, Team, TOI_p, 
             GF60:xGA60,
             
             rel_TM_GF60:rel_TM_xGA60,
             rel_TM_SH_perc, 
             
             rel_TM_GF_impact:rel_TM_xGA_impact
             ) %>% 
      rename(position = position_p, 
             TOI = TOI_p
             ) %>% 
      mutate_if(is.numeric, funs(round(., 3))) %>% 
      mutate(TOI = round(TOI, 2)) %>% 
      data.frame()
    
    
    return_list <- list(WOWY_data =   WOWY_df, 
                        rel_TM_data = rel_impact_EV)
    
    return(return_list)
    
    }
  
  if (strength == "5v4") { 
    
    # Calculate relative to teammate
    player_metrics_PP <- games_data %>% 
      select(-c(onGF:onxGA)) %>% 
      rename_at(vars(onGF_adj:onxGF_adj), funs(gsub("_adj", "", .))) %>% 
      filter(TOI > 0) %>% 
      group_by(player, season, Team) %>% 
      mutate(GP = 1) %>% 
      summarise_at(vars(TOI, GP, onGF, onSF, onFF, onCF, onxGF), 
                   funs(sum)
                   ) %>% 
      mutate(onSH.perc = ifelse(onGF > 0 & onSF > 0, 100 * (onGF / onSF), 0)) %>% # for on-ice shooting % relative to teammate
      mutate_at(vars(onGF, onSF, onFF, onCF, onxGF), # convert columns to per 60
                funs((. / TOI) * 60)
                ) %>% 
      ungroup() %>% 
      rename_at(vars(onGF, onSF, onFF, onCF, onxGF), # rename to per 60
                funs(paste0(gsub("on", "",.), "60"))
                ) %>% 
      left_join(., position_data, by = "player") %>% 
      data.frame()
    
    # Teammate on-ice raw and per 60 numbers
    teammate_metrics_PP <- player_metrics_PP %>% 
      rename(teammate = player)
    
    # Calculate relative to teammate numbers
    WOWY_df <- TM_data %>% 
      rename(TOI_tog = TOI) %>% 
      group_by(player, teammate, season, Team) %>% 
      summarise(TOI_tog = sum(TOI_tog)) %>% 
      ungroup() %>% 
      left_join(., player_metrics_PP, by = c("player", "season", "Team")) %>% # join player metrics
      left_join(., teammate_metrics_PP, by = c("teammate", "season", "Team"), suffix = c("_p", "_t")) %>% # join teammate metrics
      mutate(player_TOI_perc_w = TOI_tog / TOI_p) %>% 
      mutate_at(vars(ends_with("_t")), # add "weighted raw" columns 
                .funs = funs(weighted = . * player_TOI_perc_w)
                ) %>% 
      data.frame()
    
    joined_df_PP <- WOWY_df %>% 
      group_by_at(vars(player, position_p, season, Team, TOI_p, GF60_p:xGF60_p, onSH.perc_p)) %>% 
      summarise_at(vars(player_TOI_perc_w, GF60_t_weighted:xGF60_t_weighted, onSH.perc_t_weighted), 
                   funs(sum)
                   ) %>% 
      ungroup() %>% 
      mutate_at(vars(GF60_t_weighted:xGF60_t_weighted, onSH.perc_t_weighted), # finalzie weighted avg of teammates
                funs(. / player_TOI_perc_w)
                ) %>% 
      rename_at(vars(GF60_p:xGF60_p, onSH.perc_p), 
                funs(gsub("_p", "",.))
                ) %>% 
      rename_at(vars(GF60_t_weighted:xGF60_t_weighted, onSH.perc_t_weighted), 
                funs(paste0("w_TM_", gsub("_t_weighted", "",.)))
                ) %>% 
      mutate(rel_TM_GF60 =    GF60 - w_TM_GF60, 
             rel_TM_SF60 =    SF60 - w_TM_SF60, 
             rel_TM_FF60 =    FF60 - w_TM_FF60, 
             rel_TM_CF60 =    CF60 - w_TM_CF60, 
             rel_TM_xGF60 =   xGF60 - w_TM_xGF60, 
             rel_TM_SH_perc = onSH.perc - w_TM_onSH.perc
             ) %>% 
      data.frame()
    
    # Create impact numbers and clean up
    rel_impact_PP <- joined_df_PP %>% 
      mutate_at(vars(contains("rel_TM_")), # create expanded "impact" columns
                .funs = funs(impact = round((. / 60) * TOI_p, 2))
                ) %>% 
      rename_at(vars(rel_TM_GF60_impact:rel_TM_xGF60_impact), 
                funs(gsub("60", "", .))
                ) %>% 
      select(player, position_p, season, Team, TOI_p, 
             GF60:xGF60, 
             rel_TM_GF60:rel_TM_xGF60, 
             rel_TM_GF_impact:rel_TM_xGF_impact
             ) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      rename(position = position_p, 
             TOI = TOI_p
             ) %>% 
      data.frame()
    
    return_list <- list(WOWY_data =   WOWY_df, 
                        rel_TM_data = rel_impact_PP)
    
    return(return_list)
      
    }
  
  if (strength == "4v5") { 
    
    # SH - Relative to Teammate "New"
    player_metrics_SH <- games_data %>% 
      select(-c(onGF:onxGA)) %>% 
      rename_at(vars(onGA_adj:onxGA_adj), funs(gsub("_adj", "", .))) %>% 
      filter(TOI > 0) %>% 
      group_by(player, season, Team) %>% 
      mutate(GP = 1) %>% 
      summarise_at(vars(TOI, GP, onGA, onSA, onFA, onCA, onxGA), 
                   funs(sum)
                   ) %>% 
      mutate_at(vars(onGA, onSA, onFA, onCA, onxGA), # convert columns to per 60
                funs((. / TOI) * 60)
                ) %>% 
      ungroup() %>% 
      rename_at(vars(onGA, onSA, onFA, onCA, onxGA), # rename to per 60
                funs(paste0(gsub("on", "",.), "60"))
                ) %>% 
      left_join(., position_data, by = "player") %>% 
      data.frame()
    
    # Teammate on-ice raw and per 60 numbers
    teammate_metrics_SH <- player_metrics_SH %>% 
      rename(teammate = player)
    
    # Calculate relative to teammate numbers
    WOWY_df <- TM_data %>% 
      rename(TOI_tog = TOI) %>% 
      group_by(player, teammate, season, Team) %>% 
      summarise(TOI_tog = sum(TOI_tog)) %>% 
      ungroup() %>% 
      left_join(., player_metrics_SH, by = c("player", "season", "Team")) %>% # join player metrics
      left_join(., teammate_metrics_SH, by = c("teammate", "season", "Team"), suffix = c("_p", "_t")) %>% # join teammate metrics
      mutate(player_TOI_perc_w = TOI_tog / TOI_p) %>% 
      mutate_at(vars(ends_with("_t")), # add "weighted raw" columns 
                .funs = funs(weighted = . * player_TOI_perc_w)
                ) %>% 
      data.frame()
    
    joined_df_SH <- WOWY_df %>% 
      group_by_at(vars(player, position_p, season, Team, TOI_p, GA60_p:xGA60_p)) %>% 
      summarise_at(vars(player_TOI_perc_w, GA60_t_weighted:xGA60_t_weighted), 
                   funs(sum)
                   ) %>% 
      ungroup() %>% 
      mutate_at(vars(GA60_t_weighted:xGA60_t_weighted), # finalzie weighted avg of teammates
                funs(. / player_TOI_perc_w)
                ) %>% 
      rename_at(vars(GA60_p:xGA60_p), 
                funs(gsub("_p", "",.))
                ) %>% 
      rename_at(vars(GA60_t_weighted:xGA60_t_weighted), 
                funs(paste0("w_TM_", gsub("_t_weighted", "",.)))
                ) %>% 
      rename_at(vars(contains("_state")), # fix state adj names
                funs(gsub("_state60", "60_state", .))
                ) %>% 
      mutate(rel_TM_GA60 =  GA60 - w_TM_GA60, 
             rel_TM_SA60 =  SA60 - w_TM_SA60, 
             rel_TM_FA60 =  FA60 - w_TM_FA60, 
             rel_TM_CA60 =  CA60 - w_TM_CA60, 
             rel_TM_xGA60 = xGA60 - w_TM_xGA60
             ) %>% 
      data.frame()
    
    # Create impact numbers and clean up
    rel_impact_SH <- joined_df_SH %>% 
      mutate_at(vars(contains("rel_TM_")), # create expanded "impact" columns
                .funs = funs(impact = round((. / 60) * TOI_p, 2))
                ) %>%  
      rename_at(vars(rel_TM_GA60_impact:rel_TM_xGA60_impact), 
                funs(gsub("60", "", .))
                ) %>% 
      select(player, position_p, season, Team, TOI_p, 
             GA60:xGA60, 
             rel_TM_GA60:rel_TM_xGA60, 
             rel_TM_GA_impact:rel_TM_xGA_impact
             ) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      rename(position = position_p, 
             TOI = TOI_p
             ) %>% 
      data.frame()
    
    # Return list
    return_list <- list(WOWY_data =   WOWY_df, 
                        rel_TM_data = rel_impact_SH)
    
    return(return_list)
      
    }
  
  }


##################################


## -------------------- ##
##   Sum Goalie Stats   ##
## -------------------- ##

##########################

fun.goalie_sum_all_sit <- function(data) { 
  
  return_df <- data %>% 
    group_by(player, season, Team) %>% 
    summarise_at(vars(TOI, GA, SA, GA_, FA, xGA), funs(sum)) %>% 
    ungroup() %>% 
    arrange(desc(TOI)) %>% 
    mutate(n = row_number(),  
           qual = 1 * (n <= 60), 
           SV_perc = 100 * (1 - (GA / SA)), 
           FSV_perc = 100 * (1 - (GA_ / FA)), 
           xFSV_perc = 100 * (1 - (xGA / FA)), 
           d_FSV_perc = FSV_perc - xFSV_perc, 
           GSAA = ((sum(GA) / sum(SA)) * SA) - GA, 
           GSAx = xGA - GA_
           ) %>% 
    mutate_if(is.numeric, funs(round(., 2))) %>% 
    select(player:Team, TOI, 
           qual, 
           GA, SA, 
           SV_perc, 
           GA_, FA, xGA, 
           FSV_perc, xFSV_perc, d_FSV_perc, GSAA, GSAx
           ) %>% 
    arrange(player) %>% 
    data.frame()
  
  }


##########################


## ------------------ ##
##   Sum Team Stats   ##
## ------------------ ##

########################

# All Sit Function
fun.team_sum_all_sit <- function(data) { 
  
  hold <- data %>% 
    mutate(GP = 1) %>% 
    select(-c(Opponent, game_id, game_date, is_home)) %>% 
    group_by(Team, season) %>% 
    summarise_all(funs(sum)) %>% 
    mutate(GF_perc =  100 * round(GF / (GF + GA), 4), 
           xGF_perc = 100 * round(xGF / (xGF + xGA), 4), 
           SF_perc =  100 * round(SF / (SF + SA), 4), 
           FF_perc =  100 * round(FF / (FF + FA), 4), 
           CF_perc =  100 * round(CF / (CF + CA), 4)
           ) %>% 
    mutate_if(is.numeric, funs(round(., 2))) %>% 
    select(Team:TOI, GP, GF:C_diff, GF_perc:CF_perc, PEND2:PENT5) %>% 
    mutate(PEN2_diff = PEND2 - PENT2) %>% 
    data.frame()
  
  }

# EV Function
fun.team_sum_EV <- function(data, strength) { 
  
  if (strength == "EV") { 
    
    # Calculate games played
    GP_count <- data %>% 
      group_by(Team, game_id, season) %>% 
      summarise() %>% 
      group_by(Team, season) %>% 
      mutate(GP = row_number()) %>% 
      summarise(GP = max(GP)) %>% 
      filter(!is.na(Team)) %>% 
      data.frame()
    
    # Summed for season
    sum <- data %>% 
      select(-c(Opponent, game_id, game_date, is_home)) %>% 
      group_by(Team, season) %>% 
      summarise_all(funs(sum)) %>% 
      mutate(GF_perc =  100 * round(GF / (GF + GA), 4), 
             xGF_perc = 100 * round(xGF / (xGF + xGA), 4), 
             SF_perc =  100 * round(SF / (SF + SA), 4), 
             FF_perc =  100 * round(FF / (FF + FA), 4), 
             CF_perc =  100 * round(CF / (CF + CA), 4), 
             skaters =  5 * (TOI_5v5 / TOI) + 4 * (TOI_4v4 / TOI) + 3 * (TOI_3v3 / TOI)
             ) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      right_join(GP_count, ., by = c("Team", "season")) %>% 
      select(Team:TOI_3v3, skaters, GF:C_diff, GF_perc:CF_perc, PEND2:PENT5) %>% 
      mutate(PEN2_diff = PEND2 - PENT2) %>% 
      data.frame()
    
    }
  else if (strength != "EV") { 
    
    # Calculate games played
    GP_count <- data %>% 
      group_by(Team, game_id, season) %>% 
      summarise() %>% 
      group_by(Team, season) %>% 
      mutate(GP = row_number()) %>% 
      summarise(GP = max(GP)) %>% 
      filter(!is.na(Team)) %>% 
      data.frame()
    
    # Summed for season
    sum <- data %>% 
      select(-c(Opponent, game_id, game_date, is_home)) %>% 
      group_by(Team, season) %>% 
      summarise_all(funs(sum)) %>% 
      mutate(GF_perc =  100 * round(GF / (GF + GA), 4), 
             xGF_perc = 100 * round(xGF / (xGF + xGA), 4), 
             SF_perc =  100 * round(SF / (SF + SA), 4), 
             FF_perc =  100 * round(FF / (FF + FA), 4), 
             CF_perc =  100 * round(CF / (CF + CA), 4), 
             
             GF_perc_adj =  100 * round(GF / (GF + GA), 4), 
             xGF_perc_adj = 100 * round(xGF / (xGF + xGA), 4), 
             SF_perc_adj =  100 * round(SF / (SF + SA), 4), 
             FF_perc_adj =  100 * round(FF / (FF + FA), 4), 
             CF_perc_adj =  100 * round(CF / (CF + CA), 4)
             ) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      right_join(GP_count, ., by = c("Team", "season")) %>% 
      select(Team:TOI, GF:C_diff, GF_adj:C_diff_adj, GF_perc:CF_perc, GF_perc_adj:CF_perc_adj, PEND2:PENT5) %>% 
      mutate(PEN2_diff = PEND2 - PENT2) %>% 
      data.frame()
    
    }
  
  }

# PP Function
fun.team_sum_PP <- function(data, strength) { 
  
  if (strength == "PP") { 
    
    # Calculate games played
    GP_count <- data %>% 
      group_by(Team, game_id, season) %>% 
      summarise() %>% 
      group_by(Team, season) %>% 
      mutate(GP = row_number()) %>% 
      summarise(GP = max(GP)) %>% 
      filter(!is.na(Team)) %>% 
      data.frame()
    
    # Summed for season
    sum <- data %>% 
      select(-c(Opponent, game_id, game_date, is_home)) %>% 
      group_by(Team, season) %>% 
      summarise_all(funs(sum)) %>% 
      mutate(skaters =  5 * (TOI_5v4 / TOI) + 5 * (TOI_5v3 / TOI) + 4 * (TOI_4v3 / TOI)) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      right_join(GP_count, ., by = c("Team", "season")) %>% 
      select(Team:TOI_4v3, skaters, GF:CA, PEND2:PENT5) %>% 
      mutate(PEN2_diff = PEND2 - PENT2) %>% 
      data.frame()
    
    }
  else if (strength != "PP") { 
    
    # Calculate games played
    GP_count <- data %>% 
      group_by(Team, game_id, season) %>% 
      summarise() %>% 
      group_by(Team, season) %>% 
      mutate(GP = row_number()) %>% 
      summarise(GP = max(GP)) %>% 
      filter(!is.na(Team)) %>% 
      data.frame()
    
    # Summed for season
    sum <- data %>% 
      select(-c(Opponent, game_id, game_date, is_home)) %>% 
      group_by(Team, season) %>% 
      summarise_all(funs(sum)) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      right_join(GP_count, ., by = c("Team", "season")) %>% 
      select(Team:TOI, GF:CA, GF_adj:CF_adj, PEND2:PENT5) %>% 
      mutate(PEN2_diff = PEND2 - PENT2) %>% 
      data.frame()
    
    }
  
  }

# SH Function
fun.team_sum_SH <- function(data, strength) { 
  
  if (strength == "SH") { 
    
    # Calculate games played
    GP_count <- data %>% 
      group_by(Team, game_id, season) %>% 
      summarise() %>% 
      group_by(Team, season) %>% 
      mutate(GP = row_number()) %>% 
      summarise(GP = max(GP)) %>% 
      filter(!is.na(Team)) %>% 
      data.frame()
    
    # Summed for season
    sum <- data %>% 
      select(-c(Opponent, game_id, game_date, is_home)) %>% 
      group_by(Team, season) %>% 
      summarise_all(funs(sum)) %>% 
      mutate(skaters =  4 * (TOI_4v5 / TOI) + 3 * (TOI_3v5 / TOI) + 3 * (TOI_3v4 / TOI)) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      right_join(GP_count, ., by = c("Team", "season")) %>% 
      select(Team:TOI_3v4, skaters, GF:CA, PEND2:PENT5) %>% 
      mutate(PEN2_diff = PEND2 - PENT2) %>% 
      data.frame()
    
    }
  else if (strength != "SH") { 
    
    # Calculate games played
    GP_count <- data %>% 
      group_by(Team, game_id, season) %>% 
      summarise() %>% 
      group_by(Team, season) %>% 
      mutate(GP = row_number()) %>% 
      summarise(GP = max(GP)) %>% 
      filter(!is.na(Team)) %>% 
      data.frame()
    
    # Summed for season
    sum <- data %>% 
      select(-c(Opponent, game_id, game_date, is_home)) %>% 
      group_by(Team, season) %>% 
      summarise_all(funs(sum)) %>% 
      mutate_if(is.numeric, funs(round(., 2))) %>% 
      right_join(GP_count, ., by = c("Team", "season")) %>% 
      select(Team:TOI, GF:CA, GA_adj:CA_adj, PEND2:PENT5) %>% 
      mutate(PEN2_diff = PEND2 - PENT2) %>% 
      data.frame()
    
    }
  
  }


########################


## --------------------------- ##
##   TEAM RAPM FUNCTION - EV   ## *** Updated 12/16/18 with cv.glmnet change
## --------------------------- ##

#################################

fun.team_RAPM <- function(data) { 
  
  # Objects
  st.shot_events <- c("SHOT",  "GOAL")
  st.fenwick_events <- c("SHOT", "GOAL", "MISS")
  st.corsi_events <- c("SHOT", "GOAL", "MISS", "BLOCK" )
  st.even_strength <- c("5v5", "4v4", "3v3") %>% as.factor()
  
  
  print(paste0("prepare - season: ", unique(data$season)), quote = F)
  
  # Prepare pbp (initial) - filter to EV, select columns, join back-to-back data
  fun.pbp_prepare <- function(data_) {
    
    pbp_part <- data_ %>% 
      filter(event_type %in% c("GOAL", "SHOT", "MISS", "BLOCK", "ON", "TAKE", "GIVE", "HIT", "FAC"), 
             game_strength_state %in% st.even_strength, 
             game_period < 5
             ) %>% 
      mutate(scradj = home_score - away_score, 
             home_lead = ifelse(scradj >= 3, 3, 
                                ifelse(scradj <= -3, -3, scradj))
             ) %>% 
      rename(pred_goal = pred_XGB_7) %>% 
      select(game_id, event_index, season, 
             home_team, away_team, 
             game_strength_state,
             event_length, 
             event_team, 
             event_type, 
             home_lead, 
             home_zonestart, 
             pred_goal
             ) %>% 
      left_join(., btb, by = c("game_id")) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
    
    return(pbp_part)
  
    }
  pbp_part <- fun.pbp_prepare(data_ = data)
  
  
  # Create data frame with all players, goalies, teams, and event_types and their respective IDs
  fun.names_match <- function(data_) {
    
    # Teams
    teams_str <- unique(na.omit(data_$event_team))
    teams <- data.frame(matrix(nrow = length(teams_str), ncol = 3))
    names(teams) <- c("player", "position", "ID")
    teams$player <- teams_str
    teams$position <- 4
    
    teams <- teams %>% 
      arrange(player) %>% 
      mutate(ID = row_number())
    
    
    # Event Type
    event_str <- unique(na.omit(data_$event_type))
    event <- data.frame(matrix(nrow = length(event_str), ncol = 3))
    names(event) <- c("player", "position", "ID")
    event$player <- event_str
    event$position <- 5
    
    event <- event %>% 
      arrange(player) %>%
      mutate(ID = row_number() + 100)
    
    
    # Strength State
    st.strength <- unique(na.omit(data_$game_strength_state))
    strength <- data.frame(matrix(nrow = length(st.strength), ncol = 3))
    names(strength) <- c("player", "position", "ID")
    strength$player <- st.strength
    strength$position <- 6
    
    strength <- strength %>% 
      arrange(player) %>% 
      mutate(ID = row_number() + 200)
    
    
    # Combine
    all <- teams %>% 
      rbind(., event, strength) %>% 
      filter(player != 0) %>% 
      arrange(ID)
    
    return(all)
  
    }
  names_match <- fun.names_match(data_ = pbp_part)
  
  
  # Identify and save specific event type IDs for dummy function/creation below
  st.corsi_events <-   names_match[which(names_match[, 1] %in% c("SHOT", "GOAL", "MISS", "BLOCK")), 3]
  st.fenwick_events <- names_match[which(names_match[, 1] %in% c("SHOT", "GOAL", "MISS")), 3]
  st.goal_ID <-        names_match[which(names_match[, 1] %in% c("GOAL")), 3] 
  st.fac_ID <-         names_match[which(names_match[, 1] %in% c("FAC")), 3]
  st.on_ID <-          names_match[which(names_match[, 1] %in% c("ON")), 3]
  st.5v5 <-            names_match[which(names_match[, 1] %in% c("5v5")), 3]
  st.4v4 <-            names_match[which(names_match[, 1] %in% c("4v4")), 3]
  st.3v3 <-            names_match[which(names_match[, 1] %in% c("3v3")), 3]
  
  
  # Convert prepared pbp data frame to numeric values
  fun.IDs <- function(data_, names_data) {
    
    data_$game_id <- as.numeric(data_$game_id)
    
    # Teams
    data_$event_team <- names_data$ID[match(data_$event_team, names_data$player)]
    data_$home_team <-  names_data$ID[match(data_$home_team, names_data$player)]
    data_$away_team <-  names_data$ID[match(data_$away_team, names_data$player)]
    
    # Event Type
    data_$event_type <- names_data$ID[match(data_$event_type, names_data$player)]
    data_$game_strength_state <- names_data$ID[match(data_$game_strength_state, names_data$player)]
    
    # Make Empty Slots 0s
    data_[is.na(data_)] <- 0
    
    return(data_)
  
    }
  pbp_part <- fun.IDs(data_ =      pbp_part, 
                      names_data = names_match)
  
  
  ## ------------------------ ##
  ##    Create APM Tables     ##
  ## ------------------------ ##
  
  print("make table", quote = F)
  
  # GF60, xGF60, CF60 - create APM sparse matrix  
  fun.APMsparse_teams_GF <- function(data_) {
    
    ### Create Home Matrix
    print(" - home_matrix", quote = F)
    
    test.H <- data_ %>% 
      arrange(game_id, event_index) %>% 
      mutate(shift_ID = cumsum(event_type == st.on_ID), 
             off_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 3), 
             def_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 1), 
             off_lead = home_lead
             ) %>% 
      group_by(game_id, 
               season, 
               game_strength_state, 
               shift_ID, 
               off_lead
               ) %>% 
      summarise(off_team = first(home_team), 
                def_team = first(away_team), 
                length = sum(event_length),
                GF60 =  (sum(event_type == st.goal_ID & event_team == home_team) / sum(event_length)) * 3600, 
                CF60 =  (sum(event_type %in% st.corsi_events & event_team == home_team) / sum(event_length)) * 3600, 
                xGF60 = (sum((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal) / sum(event_length)) * 3600, 
                off_zonestart = sum(off_zonestart), 
                def_zonestart = sum(def_zonestart), 
                btb = first(home_btb)
                ) %>% 
      filter(length > 0) %>% 
      ungroup() %>% 
      add_column(., n = 0, .before = 1) %>% 
      mutate(n = as.numeric(row_number()), 
             score_down_3 = 1 * (off_lead <= -3), 
             score_down_2 = 1 * (off_lead == -2), 
             score_down_1 = 1 * (off_lead == -1), 
             score_even =   1 * (off_lead ==  0), 
             score_up_1 =   1 * (off_lead ==  1), 
             score_up_2 =   1 * (off_lead ==  2), 
             score_up_3 =   1 * (off_lead >=  3), 
             off_zonestart = 1 * (off_zonestart >= 1), 
             def_zonestart = 1 * (def_zonestart >= 1), 
             state_5v5 = 1 * (game_strength_state == st.5v5),
             state_4v4 = 1 * (game_strength_state == st.4v4),
             state_3v3 = 1 * (game_strength_state == st.3v3),
             is_home = 1
             ) %>% 
      select(-c(game_id, off_lead, shift_ID)) %>% 
      data.matrix()
    
    
    
    # Column Names
    print(" -- get_names", quote = F)
    
    # Retrieve player names
    groups_d <- unique(as.vector(test.H[, grep("_team", colnames(test.H))]))
    
    # Remove any missing slots
    groups_d <- groups_d[!groups_d %in% 0]
    
    # Order Smallest to Largest
    groups_d <- sort(groups_d, decreasing = FALSE)
    groups_o <- sort(groups_d, decreasing = FALSE)
    
    
    
    ### Home Offense
    print(" -- home_offense", quote = F)
    
    # Determine Columns
    tmp <- lapply(groups_o, function(x, test.H)  which(test.H[, "off_team"] == x), test.H = test.H)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_o <- sparseMatrix(i, j, dims = c(nrow(test.H), length(groups_o)))
    
    # Rename
    colnames(dummies_o) <- groups_o
    colnames(dummies_o) <- paste(colnames(dummies_o), ".o", sep = "")
    
    
    ### Home Defense
    print(" -- home_defense", quote = F)
    
    # Determine Columns
    tmp <- lapply(groups_d, function(x, test.H)  which(test.H[, "def_team"] == x), test.H = test.H)
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_d <- sparseMatrix(i, j, dims = c(nrow(test.H), length(groups_d)))
    # Rename
    colnames(dummies_d) <- groups_d
    colnames(dummies_d) <- paste(colnames(dummies_d), ".d", sep = "")
    
    # Combine
    test_sparse.H <- cbind(test.H, dummies_o, dummies_d)
    
    rm(test.H)
    gc()
    
    
    ##----------------------##
    
    
    ### Create Away Matrix
    print(" - away_matrix", quote = F)
    
    test.A <- data_ %>% 
      arrange(game_id, event_index) %>% 
      mutate(shift_ID = cumsum(event_type == st.on_ID), 
             off_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 1), 
             def_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 3), 
             off_lead = -1 * home_lead
             ) %>% 
      group_by(game_id, 
               season, 
               game_strength_state, 
               shift_ID, 
               off_lead
               ) %>% 
      summarise(off_team = first(away_team), 
                def_team = first(home_team), 
                length = sum(event_length),
                GF60 = (sum(event_type == st.goal_ID & event_team == away_team) / sum(event_length)) * 3600, 
                CF60 =  (sum(event_type %in% st.corsi_events & event_team == away_team) / sum(event_length)) * 3600, 
                xGF60 = (sum((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal) / sum(event_length)) * 3600, 
                off_zonestart = sum(off_zonestart), 
                def_zonestart = sum(def_zonestart), 
                btb = first(away_btb)
                ) %>% 
      filter(length > 0) %>% 
      ungroup() %>% 
      add_column(., n = 0, .before = 1) %>% 
      mutate(n = as.numeric(row_number()), 
             score_down_3 = 1 * (off_lead <= -3), 
             score_down_2 = 1 * (off_lead == -2), 
             score_down_1 = 1 * (off_lead == -1), 
             score_even =   1 * (off_lead ==  0), 
             score_up_1 =   1 * (off_lead ==  1), 
             score_up_2 =   1 * (off_lead ==  2), 
             score_up_3 =   1 * (off_lead >=  3), 
             off_zonestart = 1 * (off_zonestart >= 1), 
             def_zonestart = 1 * (def_zonestart >= 1), 
             state_5v5 = 1 * (game_strength_state == st.5v5),
             state_4v4 = 1 * (game_strength_state == st.4v4),
             state_3v3 = 1 * (game_strength_state == st.3v3),
             is_home = 0
             ) %>% 
      select(-c(game_id, off_lead, shift_ID)) %>% 
      data.matrix()
    
    
    ### Away Offense
    print(" -- away_offense", quote = F)
    
    # Determine Columns
    tmp <- lapply(groups_o, function(x, test.A)  which(test.A[, "off_team"] == x), test.A = test.A)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_o <- sparseMatrix(i, j, dims = c(nrow(test.A), length(groups_o)))
    
    # Rename
    colnames(dummies_o) <- groups_o
    colnames(dummies_o) <- paste(colnames(dummies_o), ".o", sep = "")
    
    
    ### Away Defense
    print(" -- away_defense", quote = F)
    
    # Determine Columns
    tmp <- lapply(groups_d, function(x, test.A)  which(test.A[, "def_team"] == x), test.A = test.A)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_d <- sparseMatrix(i, j, dims = c(nrow(test.A), length(groups_d)))
    
    # Rename
    colnames(dummies_d) <- groups_d
    colnames(dummies_d) <- paste(colnames(dummies_d), ".d", sep = "")
    
    # Combine
    test_sparse.A <- cbind(test.A, dummies_o, dummies_d)
    
    rm(test.A)
    gc()
    
    
    #####  Big Join  #####
    print(" -- combine", quote = F)
    
    test_all <- rbind(test_sparse.H, test_sparse.A)
    
    rm(test_sparse.H, test_sparse.A)
    gc()
    
    return(test_all)
  
    }
  APM_teams <- fun.APMsparse_teams_GF(data_ = pbp_part)
  
  
  
  ## ------------------------ ##
  ##          Model           ##
  ## ------------------------ ##
  
  # Cleanup / separate target, weights, and predictors
  length_l <- list(APM_teams[, "length"])
  GF60_l <- list(APM_teams[, "GF60"])
  CF60_l <- list(APM_teams[, "CF60"])
  xGF60_l <- list(APM_teams[, "xGF60"])
  
  # Remove NaNs
  length <- unlist(rapply(length_l, f = function(x) ifelse(x == 0, 1, x), how = "replace")) # correct length of 0
  GF60 <- unlist(rapply(GF60_l, f = function(x) ifelse(is.nan(x), 0, x), how = "replace")) # correct NANs
  CF60 <- unlist(rapply(CF60_l, f = function(x) ifelse(is.nan(x), 0, x), how = "replace"))
  xGF60 <- unlist(rapply(xGF60_l, f = function(x) ifelse(is.nan(x), 0, x), how = "replace"))
  
  # Remove Infs
  length_l <- list(length)
  GF60_l <- list(GF60)
  CF60_l <- list(CF60)
  xGF60_l <- list(xGF60)
  
  length <- unlist(rapply(length_l, f = function(x) ifelse(x == 0, 1, x), how = "replace"))
  GF60 <- unlist(rapply(GF60_l, f = function(x) ifelse(is.infinite(x), 0, x), how = "replace"))
  CF60 <- unlist(rapply(CF60_l, f = function(x) ifelse(is.infinite(x), 0, x), how = "replace"))
  xGF60 <- unlist(rapply(xGF60_l, f = function(x) ifelse(is.infinite(x), 0, x), how = "replace"))
  
  
  
  ## sparse matrix column names:
  # [1]  "n"                   "season"              "game_strength_state" "off_team"            "def_team"            "length"              "GF60"               
  # [8]  "CF60"                "xGF60"               "off_zonestart"       "def_zonestart"       "btb"                 "score_down_3"        "score_down_2"       
  # [15] "score_down_1"        "score_even"          "score_up_1"          "score_up_2"          "score_up_3"          "state_5v5"           "state_4v4"          
  # [22] "state_3v3"           "is_home"
  
  
  # Construct design matrix
  APM_teams_matrix <- APM_teams[, -c(1:9, 10:11, 16, 20)]  # remove excess columns - corrected factor variables
  
  
  rm(APM_teams, pbp_part, GF60_l, CF60_l, xGF60_l, length_l)
  gc()
  
  
  # Cross Validation / Ridge Regression
  
  registerDoMC(cores = 2)
  
  
  ## ------------------ ##
  ##   CV - Goals For   ##
  ## ------------------ ##
  
  print("cross validation - GF", quote = F)
  
  CV_results_GF <- cv.glmnet(x = APM_teams_matrix, 
                             y = GF60, 
                             weights = length, 
                             alpha = 0, 
                             nfolds = 10, 
                             standardize = FALSE, 
                             parallel = TRUE)
  gc()
  
  
  ## ----------------------- ##
  ##   CV - Expected Goals   ##
  ## ----------------------- ##
  
  print("cross validation - xG", quote = F)
  
  CV_results_xG <- cv.glmnet(x = APM_teams_matrix, 
                             y = xGF60, 
                             weights = length, 
                             alpha = 0, 
                             nfolds = 10, 
                             standardize = FALSE, 
                             parallel = TRUE)
  gc()
  
  
  ## ------------------ ##
  ##   CV - Corsi For   ##
  ## ------------------ ##
  
  print("cross validation - CF", quote = F)
  
  CV_results_CF <- cv.glmnet(x = APM_teams_matrix, 
                             y = CF60, 
                             weights = length, 
                             alpha = 0, 
                             nfolds = 10, 
                             standardize = FALSE, 
                             parallel = TRUE)
  gc()
  
  
  
  print("join / combine / finalize", quote = F)
  
  
  ## ------------------------ ##
  ##   Get Coefficients - GF  ##
  ## ------------------------ ##
  
  APM_test_GF <- data.frame(as.matrix(coef(CV_results_GF, s = CV_results_GF$lambda.min))) %>% 
    rownames_to_column(var = "APM_names")

  
  # Remove .d / .o suffixes
  APM_test_GF_d <- APM_test_GF %>%
    filter(grepl(".d", APM_names), 
           !grepl("_", APM_names)
           ) %>% 
    mutate(APM_names = gsub(".d", "", APM_names)) %>% 
    rename(GA = X1)
  
  APM_test_GF_o <- APM_test_GF %>% 
    filter(grepl(".o", APM_names), 
           !grepl("_", APM_names)
           ) %>% 
    mutate(APM_names = gsub(".o", "", APM_names)) %>% 
    rename(GF = X1)
  
  # Join
  APM_all_GF <- APM_test_GF_d %>% 
    left_join(., APM_test_GF_o, by = "APM_names") %>% 
    mutate(GPM = GF - GA) %>% 
    select(APM_names, GF, GA, GPM) %>% 
    rename(player = APM_names)
  
  
  
  ## ------------------------ ##
  ##   Get Coefficients - xG  ##
  ## ------------------------ ##
  
  APM_test_xG <- data.frame(as.matrix(coef(CV_results_xG, s = CV_results_xG$lambda.min))) %>% 
    rownames_to_column(var = "APM_names")
  
  
  # Remove .d / .o suffixes
  APM_test_xG_d <- APM_test_xG %>%
    filter(grepl(".d", APM_names), 
           !grepl("_", APM_names)
           ) %>% 
    mutate(APM_names = gsub(".d", "", APM_names)) %>% 
    rename(xGA = X1)
  
  APM_test_xG_o <- APM_test_xG %>% 
    filter(grepl(".o", APM_names), 
           !grepl("_", APM_names)
           ) %>% 
    mutate(APM_names = gsub(".o", "", APM_names)) %>% 
    rename(xGF = X1)
  
  # Join
  APM_all_xG <- APM_test_xG_d %>% 
    left_join(., APM_test_xG_o, by = "APM_names") %>% 
    mutate(xGPM = xGF - xGA) %>% 
    select(APM_names, xGF, xGA, xGPM) %>% 
    rename(player = APM_names)
  
  
  
  ## ------------------------ ##
  ##   Get Coefficients - CF  ##
  ## ------------------------ ##
  
  APM_test_CF <- data.frame(as.matrix(coef(CV_results_CF, s = CV_results_CF$lambda.min))) %>% 
    rownames_to_column(var = "APM_names")
  
  
  # Remove .d / .o suffixes
  APM_test_CF_d <- APM_test_CF %>%
    filter(grepl(".d", APM_names), 
           !grepl("_", APM_names)
           ) %>% 
    mutate(APM_names = gsub(".d", "", APM_names)) %>% 
    rename(CA = X1)
  
  APM_test_CF_o <- APM_test_CF %>% 
    filter(grepl(".o", APM_names), 
           !grepl("_", APM_names)
           ) %>% 
    mutate(APM_names = gsub(".o", "", APM_names)) %>% 
    rename(CF = X1)
  
  # Join
  APM_all_CF <- APM_test_CF_d %>% 
    left_join(., APM_test_CF_o, by = "APM_names") %>% 
    mutate(CPM = CF - CA) %>% 
    select(APM_names, CF, CA, CPM) %>% 
    rename(player = APM_names)
  
  
  ## ----------------- ##
  ##   TOI / Combine   ##
  ## ----------------- ##
  
  # Match names
  APM_all_GF$player <- names_match$player[match(APM_all_GF$player, names_match$ID)]
  APM_all_xG$player <- names_match$player[match(APM_all_xG$player, names_match$ID)]
  APM_all_CF$player <- names_match$player[match(APM_all_CF$player, names_match$ID)]
  
  
  # Team TOI
  fun.team_sum <- function(data_) {
    
    team_h <- data_ %>% 
      filter(game_strength_state %in% c("5v5", "4v4", "3v3")) %>% 
      group_by(home_team, season, game_id) %>% 
      summarise(TOI_5v5 = sum((game_strength_state == "5v5") * event_length) / 60, 
                TOI_4v4 = sum((game_strength_state == "4v4") * event_length) / 60, 
                TOI_3v3 = sum((game_strength_state == "3v3") * event_length) / 60, 
                TOI = sum(event_length) / 60) %>% 
      mutate(GP = 1) %>%
      group_by(home_team, season) %>% 
      summarise_at(vars(TOI_5v5:GP), funs(sum)) %>% 
      rename(Team = home_team) %>% 
      data.frame()
    
    team_a <- data_ %>% 
      filter(game_strength_state %in% c("5v5", "4v4", "3v3")) %>% 
      group_by(away_team, season, game_id) %>% 
      summarise(TOI_5v5 = sum((game_strength_state == "5v5") * event_length) / 60, 
                TOI_4v4 = sum((game_strength_state == "4v4") * event_length) / 60, 
                TOI_3v3 = sum((game_strength_state == "3v3") * event_length) / 60, 
                TOI = sum(event_length) / 60) %>% 
      mutate(GP = 1) %>%
      group_by(away_team, season) %>% 
      summarise_at(vars(TOI_5v5:GP), funs(sum)) %>% 
      rename(Team = away_team) %>% 
      data.frame()
    
    team_sum <- team_h %>% 
      full_join(., team_a, by = c("Team", "season", "TOI_5v5", "TOI_4v4", "TOI_3v3", "TOI", "GP")) %>% 
      group_by(Team, season) %>% 
      summarise_at(vars(TOI_5v5:GP), funs(sum)) %>% 
      mutate(skaters = 5 * (TOI_5v5 / TOI) + 4 * (TOI_4v4 / TOI) + 3 * (TOI_3v3 / TOI)) %>% 
      select(Team, season, GP, TOI, TOI_5v5:TOI_3v3, skaters) %>% 
      data.frame()
    
    }
  team_TOI <- fun.team_sum(data_ = data)
  
  
  # Combine
  APM_combine <- APM_all_GF %>% 
    left_join(., APM_all_xG, by = "player") %>% 
    left_join(., APM_all_CF, by = "player") %>% 
    rename(Team = player) %>% 
    left_join(team_TOI, ., by = "Team") %>% 
    mutate(GF_impact = (GF * (TOI / 60)) - (GA * (TOI / 60)), 
           xG_impact = (xGF * (TOI / 60)) - (xGA * (TOI / 60)), 
           CF_impact = (CF * (TOI / 60)) - (CA * (TOI / 60))
           ) %>% 
    mutate_at(vars(TOI:TOI_3v3, CF:CF_impact), funs(round(., 2))) %>% 
    mutate_at(vars(skaters:xGPM), funs(round(., 3)))
  
  
  # Return list of objects
  return_list <- list(
    
    # Target variables
    target_GF = GF60, 
    target_xG = xGF60, 
    target_CF = CF60,
    
    # Weights
    weights = length, 
    
    # Design matrix
    design_matrix = APM_teams_matrix, 
    
    # cv.glmnet objects
    cv_results_GF = CV_results_GF, 
    cv_results_xG = CV_results_xG, 
    cv_results_CF = CV_results_CF,
    
    # Names object
    IDs_master = names_match, 
    
    # Final RAPM table
    team_data =  APM_combine
    
    )
  
  }


#################################


## ---------------------- ##
##   TEAM RAPM RUN - PP   ## *** Updated 12/16/18 with cv.glmnet change
## ---------------------- ##

#################################

fun.team_RAPM_PP_SH <- function(data) { 
  
  print(paste0("prepare - season: ", unique(data$season)), quote = F)
  
  # Prepare pbp (initial) - filter to PP strength + column select
  fun.pbp_prepare <- function(data_) {
    
    pbp_part <- data_ %>% 
      filter(event_type %in% c("GOAL", "SHOT", "MISS", "BLOCK", "ON", "TAKE", "GIVE", "HIT", "FAC"), 
             game_strength_state %in% st.pp_strength, 
             game_period < 5
             ) %>% 
      mutate(scradj = home_score - away_score, 
             home_lead = ifelse(scradj >= 3, 3, 
                                ifelse(scradj <= -3, -3, scradj))
             ) %>% 
      rename(pred_goal = pred_XGB_7) %>% 
      select(game_id, event_index, season, 
             home_team, away_team, 
             event_length, 
             event_team, 
             event_type, 
             game_strength_state, 
             home_lead, 
             home_zonestart, 
             pred_goal) %>% 
      left_join(., btb, by = c("game_id")) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
    
    return(pbp_part)
    
    }
  pbp_part <- fun.pbp_prepare(data_ = data)
  
  
  # Create IDs for Powerplay Names Match
  fun.names_match_PP <- function(pbp_data) {
    
    # Teams
    teams_str <- unique(na.omit(pbp_data$event_team))
    teams <- data.frame(matrix(nrow = length(teams_str), ncol = 3))
    names(teams) <- c("player", "position", "ID")
    teams$player <- teams_str
    teams$position <- 4
    
    teams <- teams %>% 
      arrange(player) %>% 
      mutate(ID = row_number())
    
    
    # Event Type
    event_str <- unique(na.omit(pbp_data$event_type))
    event <- data.frame(matrix(nrow = length(event_str), ncol = 3))
    names(event) <- c("player", "position", "ID")
    event$player <- event_str
    event$position <- 5
    
    event <- event %>% 
      arrange(player) %>% 
      mutate(ID = row_number() + 100)
    
    
    # Strength State
    st.strength <- unique(na.omit(pbp_data$game_strength_state))
    strength <- data.frame(matrix(nrow = length(st.strength), ncol = 3))
    
    names(strength) <- c("player", "position", "ID")
    
    strength$player <- st.strength
    strength$position <- 6
    
    strength <- strength %>% 
      arrange(player) %>% 
      mutate(ID = row_number() + 200)
    
    
    # Combine
    all <- teams %>% 
      rbind(., event, strength) %>% 
      filter(player != 0) %>% 
      arrange(ID)
    
    return(all)
  
    }
  names_match_PP <- fun.names_match_PP(pbp_data = data)
  
  
  # Identify and save specific event type IDs for dummy function/creation below
  st.corsi_events <-   names_match_PP[which(names_match_PP[, 1] %in% c("SHOT", "GOAL", "MISS", "BLOCK")), 3]
  st.fenwick_events <- names_match_PP[which(names_match_PP[, 1] %in% c("SHOT", "GOAL", "MISS")), 3]
  st.goal_ID <-        names_match_PP[which(names_match_PP[, 1] %in% c("GOAL")), 3] 
  st.fac_ID <-         names_match_PP[which(names_match_PP[, 1] %in% c("FAC")), 3]
  st.pp_home <-        names_match_PP[which(names_match_PP[, 1] %in% c("5v4", "5v3", "4v3")), 3]
  st.pp_away <-        names_match_PP[which(names_match_PP[, 1] %in% c("4v5", "3v5", "3v4")), 3]
  st.on_ID <-          names_match_PP[which(names_match_PP[, 1] %in% c("ON")), 3]
  st.5v4 <-            names_match_PP[which(names_match_PP[, 1] %in% c("5v4", "4v5")), 3]
  st.5v3 <-            names_match_PP[which(names_match_PP[, 1] %in% c("5v3", "3v5")), 3]
  st.4v3 <-            names_match_PP[which(names_match_PP[, 1] %in% c("4v3", "3v4")), 3]
  
  
  # Convert prepared pbp data frame to all numeric values
  fun.IDs <- function(data_, names_data) {
    
    data_$game_id <- as.numeric(data_$game_id)
    
    # Teams
    data_$event_team <- names_data$ID[match(data_$event_team, names_data$player)]
    data_$home_team <- names_data$ID[match(data_$home_team, names_data$player)]
    data_$away_team <- names_data$ID[match(data_$away_team, names_data$player)]
    
    # Events / strength states
    data_$event_type <- names_data$ID[match(data_$event_type, names_data$player)]
    data_$game_strength_state <- names_data$ID[match(data_$game_strength_state, names_data$player)]
    
    # Make Empty Slots 0s
    data_[is.na(data_)] <- 0
    
    return(data_)
  
    }
  pbp_part <- fun.IDs(data_ =      pbp_part, 
                      names_data = names_match_PP)
  
  
  ## ------------------------------ ##
  ##       Setup column names       ##
  ## ------------------------------ ##
  
  ## Make Columns Names
  ##############################################
  
  # Get all player names
  print("get_names")
  
  df <- as.matrix(pbp_part)
  hold <- unique(as.vector(df[, grep("_team", colnames(df))]))
  hold <- as.numeric(hold)
  
  rm(df)
  gc()
  
  # Remove Missing Slots
  hold <- hold[!hold %in% 0]
  
  # Qualify Powerplay Offense and remove goalies
  groups_PPO <- hold
  
  # Qualify Shorthanded Defense
  groups_SHD <- hold
  
  # Qualify Shorthanded Offense and remove goalies
  groups_SHO <- hold
  
  # Qualify Powerplay Defense
  groups_PPD <- hold
  
  # Order Smallest to Largest
  groups_PPO <- sort(groups_PPO, decreasing = FALSE)
  groups_SHD <- sort(groups_SHD, decreasing = FALSE)
  groups_SHO <- sort(groups_SHO, decreasing = FALSE)
  groups_PPD <- sort(groups_PPD, decreasing = FALSE)
  
  
  ##############################################
  
  
  
  ## ------------------------------ ##
  ##        Construct Tables        ##
  ## ------------------------------ ##
  
  ## Home Table 1 (Home Offense in Home PP Strengths)
  ##############################################
  
  print("home_df_1")
  
  test.H1 <- pbp_part %>% 
    filter(game_strength_state %in% st.pp_home) %>% 
    arrange(game_id, event_index) %>% 
    mutate(shift_ID = cumsum(event_type == st.on_ID), 
           off_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 3), 
           def_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 1), 
           off_lead = home_lead
           ) %>% 
    group_by(game_id, 
             season,
             shift_ID, 
             game_strength_state, 
             off_lead
             ) %>% 
    summarise(off_team = first(home_team), 
              def_team = first(away_team), 
              length = sum(event_length),
              GF60 = (sum(event_type == st.goal_ID & event_team == home_team) / sum(event_length)) * 3600, 
              CF60 =  (sum(event_type %in% st.corsi_events & event_team == home_team) / sum(event_length)) * 3600, 
              xGF60 = (sum((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal) / sum(event_length)) * 3600, 
              off_zonestart = sum(off_zonestart), 
              def_zonestart = sum(def_zonestart), 
              btb = first(home_btb)
              ) %>% 
    filter(length > 0) %>% 
    ungroup() %>% 
    add_column(., n = 0, .before = 1) %>% 
    mutate(n = as.numeric(row_number()), 
           score_down_3 = 1 * (off_lead <= -3), 
           score_down_2 = 1 * (off_lead == -2), 
           score_down_1 = 1 * (off_lead == -1), 
           score_even =   1 * (off_lead ==  0), 
           score_up_1 =   1 * (off_lead ==  1), 
           score_up_2 =   1 * (off_lead ==  2), 
           score_up_3 =   1 * (off_lead >=  3), 
           off_zonestart = 1 * (off_zonestart >= 1), 
           def_zonestart = 1 * (def_zonestart >= 1), 
           state_5v4 = 1 * (game_strength_state %in% st.5v4),
           state_5v3 = 1 * (game_strength_state %in% st.5v3),
           state_4v3 = 1 * (game_strength_state %in% st.4v3),
           is_home = 1
           ) %>% 
    select(-c(game_id, off_lead, shift_ID)) %>% 
    data.matrix()
  
  
  ## ----------------------- ##
  ##     Home PP Offense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_PPO, function(x, test.H1)  which(test.H1[, "off_team"] == x), test.H1 = test.H1)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_PPO <- sparseMatrix(i, j, dims = c(nrow(test.H1), length(groups_PPO)))
  
  # Rename
  colnames(dummies_PPO) <- groups_PPO
  colnames(dummies_PPO) <- paste(colnames(dummies_PPO), ".PPO", sep = "")
  
  
  ## ----------------------- ##
  ##     Home SH Defense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_SHD, function(x, test.H1)  which(test.H1[, "def_team"] == x), test.H1 = test.H1)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_SHD <- sparseMatrix(i, j, dims = c(nrow(test.H1), length(groups_SHD)))
  
  # Rename
  colnames(dummies_SHD) <- groups_SHD
  colnames(dummies_SHD) <- paste(colnames(dummies_SHD), ".SHD", sep = "")
  
  
  ## ----------------------- ##
  ##     Home SH Offense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_SHO, function(x, test.H1)  which(99999 == x), test.H1 = test.H1)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_SHO <- sparseMatrix(i, j, dims = c(nrow(test.H1), length(groups_SHO)))
  
  # Rename
  colnames(dummies_SHO) <- groups_SHO
  colnames(dummies_SHO) <- paste(colnames(dummies_SHO), ".SHO", sep = "")
  
  
  ## ----------------------- ##
  ##     Home PP Defense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_PPD, function(x, test.H1)  which(99999 == x), test.H1 = test.H1)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_PPD <- sparseMatrix(i, j, dims = c(nrow(test.H1), length(groups_PPD)))
  
  # Rename
  colnames(dummies_PPD) <- groups_PPD
  colnames(dummies_PPD) <- paste(colnames(dummies_PPD), ".PPD", sep = "")
  
  
  ## ----------------------- ##
  ##          Combine        ##
  ## ----------------------- ##
  test_sparse.H1 <- cbind(test.H1, dummies_PPO, dummies_SHD, dummies_SHO, dummies_PPD)
  
  rm(test.H1)
  gc()
  
  ##############################################
  
  
  ## Home Table 2 (Home Offense in Away PP Strengths)
  ##############################################
  
  print("home_df_2")
  
  test.H2 <- pbp_part %>% 
    filter(game_strength_state %in% st.pp_away) %>% 
    arrange(game_id, event_index) %>% 
    mutate(shift_ID = cumsum(event_type == st.on_ID), 
           off_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 3), 
           def_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 1), 
           off_lead = home_lead
           ) %>% 
    group_by(game_id, 
             season,
             shift_ID, 
             game_strength_state, 
             off_lead
             ) %>% 
    summarise(off_team = first(home_team), 
              def_team = first(away_team), 
              length = sum(event_length),
              GF60 = (sum(event_type == st.goal_ID & event_team == home_team) / sum(event_length)) * 3600, 
              CF60 =  (sum(event_type %in% st.corsi_events & event_team == home_team) / sum(event_length)) * 3600, 
              xGF60 = (sum((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal) / sum(event_length)) * 3600, 
              off_zonestart = sum(off_zonestart), 
              def_zonestart = sum(def_zonestart), 
              btb = first(home_btb)
              ) %>% 
    filter(length > 0) %>% 
    ungroup() %>% 
    add_column(., n = 0, .before = 1) %>% 
    mutate(n = as.numeric(row_number()), 
           score_down_3 = 1 * (off_lead <= -3), 
           score_down_2 = 1 * (off_lead == -2), 
           score_down_1 = 1 * (off_lead == -1), 
           score_even =   1 * (off_lead ==  0), 
           score_up_1 =   1 * (off_lead ==  1), 
           score_up_2 =   1 * (off_lead ==  2), 
           score_up_3 =   1 * (off_lead >=  3), 
           off_zonestart = 1 * (off_zonestart >= 1), 
           def_zonestart = 1 * (def_zonestart >= 1), 
           state_5v4 = 1 * (game_strength_state %in% st.5v4),
           state_5v3 = 1 * (game_strength_state %in% st.5v3),
           state_4v3 = 1 * (game_strength_state %in% st.4v3),
           is_home = 1
           ) %>% 
    select(-c(game_id, off_lead, shift_ID)) %>% 
    data.matrix()
  
  
  ## ----------------------- ##
  ##     Home PP Offense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_PPO, function(x, test.H2)  which(99999 == x), test.H2 = test.H2)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_PPO <- sparseMatrix(i, j, dims = c(nrow(test.H2), length(groups_PPO)))
  
  # Rename
  colnames(dummies_PPO) <- groups_PPO
  colnames(dummies_PPO) <- paste(colnames(dummies_PPO), ".PPO", sep = "")
  
  
  ## ----------------------- ##
  ##     Home SH Defense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_SHD, function(x, test.H2)  which(99999 == x), test.H2 = test.H2)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_SHD <- sparseMatrix(i, j, dims = c(nrow(test.H2), length(groups_SHD)))
  
  # Rename
  colnames(dummies_SHD) <- groups_SHD
  colnames(dummies_SHD) <- paste(colnames(dummies_SHD), ".SHD", sep = "")
  
  
  ## ----------------------- ##
  ##     Home SH Offense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_SHO, function(x, test.H2)  which(test.H2[, "off_team"] == x), test.H2 = test.H2)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_SHO <- sparseMatrix(i, j, dims = c(nrow(test.H2), length(groups_SHO)))
  
  # Rename
  colnames(dummies_SHO) <- groups_SHO
  colnames(dummies_SHO) <- paste(colnames(dummies_SHO), ".SHO", sep = "")
  
  
  ## ----------------------- ##
  ##     Home PP Defense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_PPD, function(x, test.H2)  which(test.H2[, "def_team"] == x), test.H2 = test.H2)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_PPD <- sparseMatrix(i, j, dims = c(nrow(test.H2), length(groups_PPD)))
  
  # Rename
  colnames(dummies_PPD) <- groups_PPD
  colnames(dummies_PPD) <- paste(colnames(dummies_PPD), ".PPD", sep = "")
  
  
  ## ----------------------- ##
  ##          Combine        ##
  ## ----------------------- ##
  test_sparse.H2 <- cbind(test.H2, dummies_PPO, dummies_SHD, dummies_SHO, dummies_PPD)
  
  rm(test.H2)
  gc()
  
  ##############################################
  
  
  ## Away Table 1 (Away Offense in Away PP Strengths)
  ##############################################
  
  print("away_df_1")
  
  test.A1 <- pbp_part %>% 
    filter(game_strength_state %in% st.pp_away) %>% 
    arrange(game_id, event_index) %>% 
    mutate(shift_ID = cumsum(event_type == st.on_ID), 
           off_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 1), 
           def_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 3), 
           off_lead = -1 * home_lead
           ) %>% 
    group_by(game_id, 
             season,
             shift_ID, 
             game_strength_state, 
             off_lead
             ) %>% 
    summarise(off_team = first(away_team), 
              def_team = first(home_team), 
              length = sum(event_length),
              GF60 = (sum(event_type == st.goal_ID & event_team == away_team) / sum(event_length)) * 3600, 
              CF60 =  (sum(event_type %in% st.corsi_events & event_team == away_team) / sum(event_length)) * 3600, 
              xGF60 = (sum((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal) / sum(event_length)) * 3600, 
              off_zonestart = sum(off_zonestart), 
              def_zonestart = sum(def_zonestart), 
              btb = first(away_btb)
              ) %>% 
    filter(length > 0) %>% 
    ungroup() %>% 
    add_column(., n = 0, .before = 1) %>% 
    mutate(n = as.numeric(row_number()), 
           score_down_3 = 1 * (off_lead <= -3), 
           score_down_2 = 1 * (off_lead == -2), 
           score_down_1 = 1 * (off_lead == -1), 
           score_even =   1 * (off_lead ==  0), 
           score_up_1 =   1 * (off_lead ==  1), 
           score_up_2 =   1 * (off_lead ==  2), 
           score_up_3 =   1 * (off_lead >=  3), 
           off_zonestart = 1 * (off_zonestart >= 1), 
           def_zonestart = 1 * (def_zonestart >= 1), 
           state_5v4 = 1 * (game_strength_state %in% st.5v4),
           state_5v3 = 1 * (game_strength_state %in% st.5v3),
           state_4v3 = 1 * (game_strength_state %in% st.4v3),
           is_home = 0
           ) %>% 
    select(-c(game_id, off_lead, shift_ID)) %>% 
    data.matrix()
  
  
  ## ----------------------- ##
  ##     Away PP Offense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_PPO, function(x, test.A1)  which(test.A1[, "off_team"] == x), test.A1 = test.A1)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_PPO <- sparseMatrix(i, j, dims = c(nrow(test.A1), length(groups_PPO)))
  
  # Rename
  colnames(dummies_PPO) <- groups_PPO
  colnames(dummies_PPO) <- paste(colnames(dummies_PPO), ".PPO", sep = "")
  
  
  ## ----------------------- ##
  ##     Away SH Defense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_SHD, function(x, test.A1)  which(test.A1[, "def_team"] == x), test.A1 = test.A1)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_SHD <- sparseMatrix(i, j, dims = c(nrow(test.A1), length(groups_SHD)))
  
  # Rename
  colnames(dummies_SHD) <- groups_SHD
  colnames(dummies_SHD) <- paste(colnames(dummies_SHD), ".SHD", sep = "")
  
  
  ## ----------------------- ##
  ##     Away SH Offense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_SHO, function(x, test.A1)  which(99999 == x), test.A1 = test.A1)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_SHO <- sparseMatrix(i, j, dims = c(nrow(test.A1), length(groups_SHO)))
  
  # Rename
  colnames(dummies_SHO) <- groups_SHO
  colnames(dummies_SHO) <- paste(colnames(dummies_SHO), ".SHO", sep = "")
  
  
  ## ----------------------- ##
  ##     Away PP Defense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_PPD, function(x, test.A1)  which(99999 == x), test.A1 = test.A1)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_PPD <- sparseMatrix(i, j, dims = c(nrow(test.A1), length(groups_PPD)))
  
  # Rename
  colnames(dummies_PPD) <- groups_PPD
  colnames(dummies_PPD) <- paste(colnames(dummies_PPD), ".PPD", sep = "")
  
  
  ## ----------------------- ##
  ##         Combine         ##
  ## ----------------------- ##
  test_sparse.A1 <- cbind(test.A1, dummies_PPO, dummies_SHD, dummies_SHO, dummies_PPD)
  
  rm(test.A1)
  gc()
  
  ##############################################
  
  
  ## Away Table 2 (Away Offense in Home PP Strengths)
  ##############################################
  
  print("away_df_2")
  
  test.A2 <- pbp_part %>% 
    filter(game_strength_state %in% st.pp_home) %>% 
    arrange(game_id, event_index) %>% 
    mutate(shift_ID = cumsum(event_type == st.on_ID), 
           off_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 1), 
           def_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 3), 
           off_lead = -1 * home_lead
           ) %>% 
    group_by(game_id, 
             season,
             shift_ID, 
             game_strength_state, 
             off_lead
             ) %>% 
    summarise(off_team = first(away_team), 
              def_team = first(home_team), 
              length = sum(event_length),
              GF60 = (sum(event_type == st.goal_ID & event_team == away_team) / sum(event_length)) * 3600, 
              CF60 =  (sum(event_type %in% st.corsi_events & event_team == away_team) / sum(event_length)) * 3600, 
              xGF60 = (sum((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal) / sum(event_length)) * 3600, 
              off_zonestart = sum(off_zonestart), 
              def_zonestart = sum(def_zonestart), 
              btb = first(away_btb)
              ) %>% 
    filter(length > 0) %>% 
    ungroup() %>% 
    add_column(., n = 0, .before = 1) %>% 
    mutate(n = as.numeric(row_number()), 
           score_down_3 = 1 * (off_lead <= -3), 
           score_down_2 = 1 * (off_lead == -2), 
           score_down_1 = 1 * (off_lead == -1), 
           score_even =   1 * (off_lead ==  0), 
           score_up_1 =   1 * (off_lead ==  1), 
           score_up_2 =   1 * (off_lead ==  2), 
           score_up_3 =   1 * (off_lead >=  3), 
           off_zonestart = 1 * (off_zonestart >= 1), 
           def_zonestart = 1 * (def_zonestart >= 1), 
           state_5v4 = 1 * (game_strength_state %in% st.5v4),
           state_5v3 = 1 * (game_strength_state %in% st.5v3),
           state_4v3 = 1 * (game_strength_state %in% st.4v3),
           is_home = 0
           ) %>% 
    select(-c(game_id, off_lead, shift_ID)) %>% 
    data.matrix()
  
  
  
  ## ----------------------- ##
  ##     Away PP Offense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_PPO, function(x, test.A2)  which(99999 == x), test.A2 = test.A2)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_PPO <- sparseMatrix(i, j, dims = c(nrow(test.A2), length(groups_PPO)))
  
  # Rename
  colnames(dummies_PPO) <- groups_PPO
  colnames(dummies_PPO) <- paste(colnames(dummies_PPO), ".PPO", sep = "")
  
  
  ## ----------------------- ##
  ##     Away SH Defense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_SHD, function(x, test.A2)  which(99999 == x), test.A2 = test.A2)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_SHD <- sparseMatrix(i, j, dims = c(nrow(test.A2), length(groups_SHD)))
  
  # Rename
  colnames(dummies_SHD) <- groups_SHD
  colnames(dummies_SHD) <- paste(colnames(dummies_SHD), ".SHD", sep = "")
  
  
  ## ----------------------- ##
  ##     Away SH Offense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_SHO, function(x, test.A2)  which(test.A2[, "off_team"] == x), test.A2 = test.A2)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_SHO <- sparseMatrix(i, j, dims = c(nrow(test.A2), length(groups_SHO)))
  
  # Rename
  colnames(dummies_SHO) <- groups_SHO
  colnames(dummies_SHO) <- paste(colnames(dummies_SHO), ".SHO", sep = "")
  
  
  ## ----------------------- ##
  ##     Away PP Defense     ##
  ## ----------------------- ##
  
  # Determine Columns
  tmp <- lapply(groups_PPD, function(x, test.A2)  which(test.A2[, "def_team"] == x), test.A2 = test.A2)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies_PPD <- sparseMatrix(i, j, dims = c(nrow(test.A2), length(groups_PPD)))
  
  # Rename
  colnames(dummies_PPD) <- groups_PPD
  colnames(dummies_PPD) <- paste(colnames(dummies_PPD), ".PPD", sep = "")
  
  
  ## ----------------------- ##
  ##          Combine        ##
  ## ----------------------- ##
  test_sparse.A2 <- cbind(test.A2, dummies_PPO, dummies_SHD, dummies_SHO, dummies_PPD)
  
  rm(test.A2)
  gc()
  
  ##############################################
  
  
  
  ## ------------------------------ ##
  ##             Big Join           ##
  ## ------------------------------ ##
  print("combine")
  
  test_sparse.All <- rbind(test_sparse.H1, test_sparse.A1, test_sparse.H2, test_sparse.A2)
  
  rm(test_sparse.H1, test_sparse.A1, test_sparse.H2, test_sparse.A2)
  gc()
  
  APM_PP_teams <- test_sparse.All
  
  
  
  ## --------------- ##
  ##   Model Setup   ##
  ## --------------- ##
  
  # Cleanup / separate target, weights, and predictors
  length_l <- list(APM_PP_teams[, "length"])
  GF60_l <-   list(APM_PP_teams[, "GF60"])
  xGF60_l <-  list(APM_PP_teams[, "xGF60"])
  CF60_l <-   list(APM_PP_teams[, "CF60"])
  
  # Remove NaNs
  length <- unlist(rapply(length_l, f = function(x) ifelse(x == 0, 1, x), how = "replace")) # correct length of 0
  GF60 <-   unlist(rapply(GF60_l, f = function(x) ifelse(is.nan(x), 0, x), how = "replace")) # correct NANs
  xGF60 <-  unlist(rapply(xGF60_l, f = function(x) ifelse(is.nan(x), 0, x), how = "replace"))
  CF60 <-   unlist(rapply(CF60_l, f = function(x) ifelse(is.nan(x), 0, x), how = "replace"))
  
  # Remove Infs
  length_l <- list(length)
  GF60_l <-   list(GF60)
  xGF60_l <-  list(xGF60)
  CF60_l <-   list(CF60)
  
  length_PP <- unlist(rapply(length_l, f = function(x) ifelse(x == 0, 1, x), how = "replace"))
  GF60_PP <-   unlist(rapply(GF60_l, f = function(x) ifelse(is.infinite(x), 0, x), how = "replace"))
  xGF60_PP <-  unlist(rapply(xGF60_l, f = function(x) ifelse(is.infinite(x), 0, x), how = "replace"))
  CF60_PP <-   unlist(rapply(CF60_l, f = function(x) ifelse(is.infinite(x), 0, x), how = "replace"))
  
  
  
  ## sparse matrix column names:
  # [1]  "n"                   "season"              "game_strength_state" "off_team"            "def_team"            "length"              "GF60"               
  # [8]  "CF60"                "xGF60"               "off_zonestart"       "def_zonestart"       "btb"                 "score_down_3"        "score_down_2"       
  # [15] "score_down_1"        "score_even"          "score_up_1"          "score_up_2"          "score_up_3"          "state_5v4"           "state_5v3"          
  # [22] "state_4v3"           "is_home"
  
  
  # Construct design matrix
  APM_PP_teams_matrix <- APM_PP_teams[, -c(1:9, 10:11, 16, 20)] # remove excess columns - correct factor variables
  
  
  rm(APM_PP_teams, GF60_l, CF60_l, xGF60_l, length_l)
  gc()
  
  
  
  # Cross Validation / Ridge Regression
  registerDoMC(cores = 2)
  
  
  ## ------------------------- ##
  ##   Cross Validation - GF   ##
  ## ------------------------- ##
  
  print("cross_validation - GF")
  
  CV_results_GF <- cv.glmnet(x = APM_PP_teams_matrix, 
                             y = GF60_PP, 
                             weights = length_PP, 
                             alpha = 0, 
                             nfolds = 10, 
                             standardize = FALSE, 
                             parallel = TRUE)
  gc()
  
  
  ## ------------------------- ##
  ##   Cross Validation - xG   ##
  ## ------------------------- ##
  
  print("cross_validation - xG")
  
  CV_results_xG <- cv.glmnet(x = APM_PP_teams_matrix, 
                             y = xGF60_PP, 
                             weights = length_PP, 
                             alpha = 0, 
                             nfolds = 10, 
                             standardize = FALSE, 
                             parallel = TRUE)
  gc()
  
  
  ## ------------------------- ##
  ##   Cross Validation - CF   ##
  ## ------------------------- ##
  
  print("cross_validation - CF")
  
  CV_results_CF <- cv.glmnet(x = APM_PP_teams_matrix, 
                             y = CF60_PP, 
                             weights = length_PP, 
                             alpha = 0, 
                             nfolds = 10, 
                             standardize = FALSE, 
                             parallel = TRUE)
  gc()
  
  
  print("finalize")
  
  
  ## ------------------------ ##
  ##   Get Coefficients - GF  ##
  ## ------------------------ ##
  
  APM_test_GF <- data.frame(as.matrix(coef(CV_results_GF, s = CV_results_GF$lambda.min))) %>% 
    rownames_to_column(var = "APM_names")
  
  
  # Remove .d / .o suffixes
  APM_test_GF_PPO <- APM_test_GF %>%
    filter(grepl(".PPO", APM_names), 
           !grepl("_", APM_names)) %>% 
    mutate(APM_names = gsub(".PPO", "", APM_names)) %>% 
    rename(PPO_GF = X1)
  
  APM_test_GF_PPD <- APM_test_GF %>%
    filter(grepl(".PPD", APM_names), 
           !grepl("_", APM_names)) %>%  
    mutate(APM_names = gsub(".PPD", "", APM_names)) %>% 
    rename(PPD_GF = X1)
  
  APM_test_GF_SHO <- APM_test_GF %>% 
    filter(grepl(".SHO", APM_names), 
           !grepl("_", APM_names)) %>% 
    mutate(APM_names = gsub(".SHO", "", APM_names)) %>% 
    rename(SHO_GF = X1)
  
  APM_test_GF_SHD <- APM_test_GF %>% 
    filter(grepl(".SHD", APM_names), 
           !grepl("_", APM_names)) %>% 
    mutate(APM_names = gsub(".SHD", "", APM_names)) %>% 
    rename(SHD_GF = X1)
  
  # Join
  APM_PP_all_GF <- APM_test_GF_PPO %>% 
    left_join(., APM_test_GF_PPD, by = "APM_names") %>% 
    left_join(., APM_test_GF_SHO, by = "APM_names") %>% 
    left_join(., APM_test_GF_SHD, by = "APM_names") %>% 
    select(player = APM_names, PPO_GF, SHD_GF, SHO_GF, PPD_GF)
  
  # Rename
  APM_PP_all_GF$player <- names_match_PP$player[match(APM_PP_all_GF$player, names_match_PP$ID)]
  
  
  
  ## ------------------------ ##
  ##   Get Coefficients - xG  ##
  ## ------------------------ ##
  
  APM_test_xG <- data.frame(as.matrix(coef(CV_results_xG, s = CV_results_xG$lambda.min))) %>% 
    rownames_to_column(var = "APM_names")
  
  
  # Remove .d / .o suffixes
  APM_test_xG_PPO <- APM_test_xG %>%
    filter(grepl(".PPO", APM_names), 
           !grepl("_", APM_names)) %>% 
    mutate(APM_names = gsub(".PPO", "", APM_names)) %>% 
    rename(PPO_xG = X1)
  
  APM_test_xG_PPD <- APM_test_xG %>%
    filter(grepl(".PPD", APM_names), 
           !grepl("_", APM_names)) %>%  
    mutate(APM_names = gsub(".PPD", "", APM_names)) %>% 
    rename(PPD_xG = X1)
  
  APM_test_xG_SHO <- APM_test_xG %>% 
    filter(grepl(".SHO", APM_names), 
           !grepl("_", APM_names)) %>% 
    mutate(APM_names = gsub(".SHO", "", APM_names)) %>% 
    rename(SHO_xG = X1)
  
  APM_test_xG_SHD <- APM_test_xG %>% 
    filter(grepl(".SHD", APM_names), 
           !grepl("_", APM_names)) %>% 
    mutate(APM_names = gsub(".SHD", "", APM_names)) %>% 
    rename(SHD_xG = X1)
  
  # Join
  APM_PP_all_xG <- APM_test_xG_PPO %>% 
    left_join(., APM_test_xG_PPD, by = "APM_names") %>% 
    left_join(., APM_test_xG_SHO, by = "APM_names") %>% 
    left_join(., APM_test_xG_SHD, by = "APM_names") %>% 
    select(player = APM_names, PPO_xG, SHD_xG, SHO_xG, PPD_xG)
  
  # Rename
  APM_PP_all_xG$player <- names_match_PP$player[match(APM_PP_all_xG$player, names_match_PP$ID)]
  
  
  
  ## ------------------------ ##
  ##   Get Coefficients - CF  ##
  ## ------------------------ ##
  
  APM_test_CF <- data.frame(as.matrix(coef(CV_results_CF, s = CV_results_CF$lambda.min))) %>% 
    rownames_to_column(var = "APM_names")
  
  
  # Remove .d / .o suffixes
  APM_test_CF_PPO <- APM_test_CF %>%
    filter(grepl(".PPO", APM_names), 
           !grepl("_", APM_names)) %>% 
    mutate(APM_names = gsub(".PPO", "", APM_names)) %>% 
    rename(PPO_CF = X1)
  
  APM_test_CF_PPD <- APM_test_CF %>%
    filter(grepl(".PPD", APM_names), 
           !grepl("_", APM_names)) %>%  
    mutate(APM_names = gsub(".PPD", "", APM_names)) %>% 
    rename(PPD_CF = X1)
  
  APM_test_CF_SHO <- APM_test_CF %>% 
    filter(grepl(".SHO", APM_names), 
           !grepl("_", APM_names)) %>% 
    mutate(APM_names = gsub(".SHO", "", APM_names)) %>% 
    rename(SHO_CF = X1)
  
  APM_test_CF_SHD <- APM_test_CF %>% 
    filter(grepl(".SHD", APM_names), 
           !grepl("_", APM_names)) %>% 
    mutate(APM_names = gsub(".SHD", "", APM_names)) %>% 
    rename(SHD_CF = X1)
  
  # Join
  APM_PP_all_CF <- APM_test_CF_PPO %>% 
    left_join(., APM_test_CF_PPD, by = "APM_names") %>% 
    left_join(., APM_test_CF_SHO, by = "APM_names") %>% 
    left_join(., APM_test_CF_SHD, by = "APM_names") %>% 
    select(APM_names, PPO_CF, SHD_CF, SHO_CF, PPD_CF) %>% 
    rename(player = APM_names)
  
  # Rename
  APM_PP_all_CF$player <- names_match_PP$player[match(APM_PP_all_CF$player, names_match_PP$ID)]
  
  
  ## ----------------- ##
  ##   TOI / Combine   ##
  ## ----------------- ##
  
  # TOI
  fun.team_sum_PP <- function(data_inner) {
    
    ## --- Powerplay --- ###
    
    team_h_PP <- data_inner %>% 
      filter(game_strength_state %in% c("5v4", "5v3", "4v3")) %>% 
      group_by(home_team, season, game_id) %>% 
      summarise(TOI_5v4 = sum((game_strength_state == "5v4") * event_length) / 60, 
                TOI_5v3 = sum((game_strength_state == "5v3") * event_length) / 60, 
                TOI_4v3 = sum((game_strength_state == "4v3") * event_length) / 60, 
                TOI_PP = sum(event_length) / 60
                ) %>% 
      mutate(GP_PP = 1) %>%
      group_by(home_team, season) %>% 
      summarise_at(vars(TOI_5v4:GP_PP), funs(sum)) %>% 
      rename(Team = home_team) %>% 
      data.frame()
    
    team_a_PP <- data_inner %>% 
      filter(game_strength_state %in% c("4v5", "3v5", "3v4")) %>% 
      group_by(away_team, season, game_id) %>% 
      summarise(TOI_5v4 = sum((game_strength_state == "4v5") * event_length) / 60, 
                TOI_5v3 = sum((game_strength_state == "3v5") * event_length) / 60, 
                TOI_4v3 = sum((game_strength_state == "3v4") * event_length) / 60, 
                TOI_PP = sum(event_length) / 60
                ) %>% 
      mutate(GP_PP = 1) %>%
      group_by(away_team, season) %>% 
      summarise_at(vars(TOI_5v4:GP_PP), funs(sum)) %>% 
      rename(Team = away_team) %>% 
      data.frame()
    
    team_sum_PP <- team_h_PP %>% 
      full_join(., team_a_PP, by = c("Team", "season", "TOI_5v4", "TOI_5v3", "TOI_4v3", "TOI_PP", "GP_PP")) %>% 
      group_by(Team, season) %>% 
      summarise_at(vars(TOI_5v4:GP_PP), funs(sum)) %>% 
      mutate(skaters_PP = round(5 * (TOI_5v4 / TOI_PP) + 5 * (TOI_5v3 / TOI_PP) + 4 * (TOI_4v3 / TOI_PP), 3)) %>% 
      select(Team, season, GP_PP, TOI_PP, TOI_5v4:TOI_4v3, skaters_PP) %>% 
      mutate_at(vars(GP_PP:TOI_4v3), funs(round(., 2))) %>% 
      data.frame()
    
    
    ### --- Shorthanded --- ###
    team_h_SH <- data_inner %>% 
      filter(game_strength_state %in% c("4v5", "3v5", "3v4")) %>% 
      group_by(home_team, season, game_id) %>% 
      summarise(TOI_4v5 = sum((game_strength_state == "4v5") * event_length) / 60, 
                TOI_3v5 = sum((game_strength_state == "3v5") * event_length) / 60, 
                TOI_3v4 = sum((game_strength_state == "3v4") * event_length) / 60, 
                TOI_SH = sum(event_length) / 60
                ) %>% 
      mutate(GP_SH = 1) %>%
      group_by(home_team, season) %>% 
      summarise_at(vars(TOI_4v5:GP_SH), funs(sum)) %>% 
      rename(Team = home_team) %>% 
      data.frame()
    
    team_a_SH <- data_inner %>% 
      filter(game_strength_state %in% c("5v4", "5v3", "4v3")) %>% 
      group_by(away_team, season, game_id) %>% 
      summarise(TOI_4v5 = sum((game_strength_state == "5v4") * event_length) / 60, 
                TOI_3v5 = sum((game_strength_state == "5v3") * event_length) / 60, 
                TOI_3v4 = sum((game_strength_state == "4v3") * event_length) / 60, 
                TOI_SH = sum(event_length) / 60
                ) %>% 
      mutate(GP_SH = 1) %>%
      group_by(away_team, season) %>% 
      summarise_at(vars(TOI_4v5:GP_SH), funs(sum)) %>% 
      rename(Team = away_team) %>% 
      data.frame()
    
    team_sum_SH <- team_h_SH %>% 
      full_join(., team_a_SH, by = c("Team", "season", "TOI_4v5", "TOI_3v5", "TOI_3v4", "TOI_SH", "GP_SH")) %>% 
      group_by(Team, season) %>% 
      summarise_at(vars(TOI_4v5:GP_SH), funs(sum)) %>% 
      mutate(skaters_SH = round(4 * (TOI_4v5 / TOI_SH) + 3 * (TOI_3v5 / TOI_SH) + 3 * (TOI_3v4 / TOI_SH), 3)) %>% 
      select(Team, season, GP_SH, TOI_SH, TOI_4v5:TOI_3v4, skaters_SH) %>% 
      mutate_at(vars(GP_SH:TOI_3v4), funs(round(., 2))) %>% 
      data.frame()
    
    
    ### --- ALL --- ###
    
    team_sum_all <- left_join(team_sum_PP, team_sum_SH, by = c("Team", "season"))
    
    
    }
  team_TOI_PP <- fun.team_sum_PP(data)
  
  
  # Combine
  APM_combine_PP <- APM_PP_all_GF %>% 
    select(player, PPO_GF, PPD_GF) %>% 
    left_join(., select(APM_PP_all_xG, player, PPO_xG, PPD_xG), by = "player") %>% 
    left_join(., select(APM_PP_all_CF, player, PPO_CF, PPD_CF), by = "player") %>% 
    rename(Team = player) %>% 
    left_join(select(team_TOI_PP, Team, season, GP_PP, TOI_PP, TOI_5v4:skaters_PP), ., by = "Team") %>% 
    mutate(GF_expand = round(PPO_GF * (TOI_PP / 60), 2),  
           xGF_expand = round(PPO_xG * (TOI_PP / 60), 2), 
           CF_expand = round(PPO_CF * (TOI_PP / 60), 2)
           ) %>% 
    mutate_if(is.numeric, funs(round(., 3)))
  
  APM_combine_SH <- APM_PP_all_GF %>% 
    select(player, SHD_GF, SHO_GF) %>% 
    left_join(., select(APM_PP_all_xG, player, SHD_xG, SHO_xG), by = "player") %>% 
    left_join(., select(APM_PP_all_CF, player, SHD_CF, SHO_CF), by = "player") %>% 
    rename(Team = player) %>% 
    left_join(select(team_TOI_PP, Team, season, GP_SH, TOI_SH, TOI_4v5:skaters_SH), ., by = "Team") %>% 
    mutate(GA_expand = round(SHD_GF * (TOI_SH / 60), 2),  
           xGA_expand = round(SHD_xG * (TOI_SH / 60), 2), 
           CA_expand = round(SHD_CF * (TOI_SH / 60), 2)
           ) %>% 
    mutate_if(is.numeric, funs(round(., 3)))
  
  
  # Return list of objects
  RAPM_return <- list(
    
    # Target variables
    target_GF = GF60_PP, 
    target_xG = xGF60_PP, 
    target_CF = CF60_PP, 
    
    # Weights
    weights = length_PP, 
    
    # Design matrix
    design_matrix = APM_PP_teams_matrix, 
    
    # cv.glmnet objects
    cv_results_GF = CV_results_GF, 
    cv_results_xG = CV_results_xG, 
    cv_results_CF = CV_results_CF,
    
    # Names object
    IDs_master = names_match_PP, 
    
    # Final RAPM tables
    team_PP =    APM_combine_PP, 
    team_SH =    APM_combine_SH
    
    )
  
  }


#################################


##  SPM/GAR/WAR Prep - No Functions


## ------------------------------------- ##
##   SPM/GAR/WAR Predictions / Combine   ##
## ------------------------------------- ##

###########################################

# Even-Strength Combine Function - F & D
fun.ALL_EV_GAA <- function() { 
  
  ## -------------- ##
  ##    Forwards    ##
  ## -------------- ##
  
  ####################
  
  ## ------------- ##
  ##     EVO_F     ##
  ## ------------- ##
  
  eval_pred_EVO_F <- pred_EVO_F_join %>% 
    select(player:GP)
  
  
  eval_pred_EVO_F$pred_1 <- predict(object = mod_list_EVO_F$lm, 
                                    pred_EVO_F_join[, features_EVO_F_small],
                                    type = "raw")
  
  eval_pred_EVO_F$pred_2 <- predict(object = mod_list_EVO_F$bagEarth, 
                                    pred_EVO_F_join[, features_EVO_F_large], 
                                    type = "raw")
  
  eval_pred_EVO_F$pred_3 <- predict(object = mod_list_EVO_F$svmLinear, 
                                    pred_EVO_F_join[, features_EVO_F_large], 
                                    type = "raw")
  
  eval_pred_EVO_F <- eval_pred_EVO_F %>% 
    mutate(ensemble = 
             pred_1 * mod_EVO_F_weights[1] + 
             pred_2 * mod_EVO_F_weights[2] + 
             pred_3 * mod_EVO_F_weights[3], 
           EVO = (ensemble / 60) * TOI)
  
  
  # Combine for full join
  EVO_F_GAA <- eval_pred_EVO_F %>% 
    group_by(player, position, season, Team) %>%  # added Team
    summarise_at(vars(TOI, GP, EVO), funs(sum)) %>% 
    mutate(EVO_60 = (EVO / TOI) * 60) %>% 
    data.frame()
  
  
  
  ## ------------- ##
  ##     EVD_F     ##
  ## ------------- ##
  
  eval_pred_EVD_F <- pred_EVD_F_join %>% 
    select(player:GP)
  
  eval_pred_EVD_F$pred_1 <- predict(object = mod_list_EVD_F$bagEarth, 
                                    pred_EVD_F_join[, features_EVD_F_large],
                                    type = "raw")
  
  eval_pred_EVD_F$pred_2 <- predict(object = mod_list_EVD_F$svmLinear, 
                                    pred_EVD_F_join[, features_EVD_F_large], 
                                    type = "raw")
  
  eval_pred_EVD_F$pred_3 <- predict(object = mod_list_EVD_F$glmnet, 
                                    pred_EVD_F_join[, features_EVD_F_large], 
                                    type = "raw")
  
  eval_pred_EVD_F <- eval_pred_EVD_F %>% 
    mutate(ensemble = 
             pred_1 * mod_EVD_F_weights[1] + 
             pred_2 * mod_EVD_F_weights[2] + 
             pred_3 * mod_EVD_F_weights[3], 
           EVD = (ensemble / 60) * TOI)
  
  
  # Combine for full join
  EVD_F_GAA <- eval_pred_EVD_F %>% 
    group_by(player, position, season, Team) %>%  # added Team
    summarise_at(vars(TOI, GP, EVD), funs(sum)) %>% 
    mutate(EVD_60 = (EVD / TOI) * 60) %>% 
    data.frame()
  
  
  ## ---------------- ##
  ##  Combine EV - F  ##
  ## ---------------- ##
  
  EV_F_overall <- EVO_F_GAA %>% 
    left_join(., EVD_F_GAA, by = c("player", "position", "season", "Team", "TOI", "GP")) %>% 
    #filter(TOI > TOI_cut_EV) %>% 
    group_by(season) %>% 
    mutate(EVO = ifelse(TOI < TOI_cut_EV, (EVO / 60) * TOI, EVO),  # Added "regressing" to average
           EVD = ifelse(TOI < TOI_cut_EV, (EVD / 60) * TOI, EVD),  # Added "regressing" to average 
           
           EVO_AA =    ((EVO / TOI) - (sum(EVO) / sum(TOI))) * TOI, 
           EVD_AA =    ((EVD / TOI) - (sum(EVD) / sum(TOI))) * TOI, 
           EVO_AA_60 = (EVO_AA / TOI) * 60, 
           EVD_AA_60 = (EVD_AA / TOI) * 60
           ) %>% 
    data.frame()
  
  
  # EV FINAL JOIN: Forwards
  EV_F_team_final <- EV_F_overall %>% 
    left_join(., team_strength_RAPM_EV %>% select(season, Team, TOI, skaters, GF, xGA), 
              by = c("season", "Team"), 
              suffix = c("_player", "_team")
              ) %>% 
    group_by(player, season) %>% 
    mutate(TOI_perc_tot =  TOI_player / TOI_team, 
           adj_off =       (1.6 * GF - (TOI_perc_tot * EVO_AA_60)) / skaters,   # new team adjustment (run 3)
           adj_def =       (1.4 * xGA - (TOI_perc_tot * EVD_AA_60)) / skaters,  # new team adjustment (run 3)
           EVO_AA_60_adj = EVO_AA_60 + adj_off, 
           EVD_AA_60_adj = EVD_AA_60 + adj_def, 
           EVO_AA_adj =    (EVO_AA_60_adj / 60) * TOI_player, 
           EVD_AA_adj =    -1 * ((EVD_AA_60_adj / 60) * TOI_player), 
           EVD_AA =        -1 * EVD_AA,
           off_diff =      EVO_AA_adj - EVO_AA, 
           def_diff =      EVD_AA_adj - EVD_AA
           ) %>% 
    rename(TOI = TOI_player) %>% 
    select(player:GP, 
           TOI_perc_EV = TOI_perc_tot, 
           SPM_EVO_60 = EVO_60, 
           SPM_EVD_60 = EVD_60, 
           SPM_EVO_AA_60 = EVO_AA_60, 
           SPM_EVD_AA_60 = EVD_AA_60, 
           team_TOI_EV = TOI_team, 
           team_skaters_EV = skaters, 
           team_EV_GF = GF, 
           team_EV_xGA = xGA, 
           adj_EVO_60 = adj_off, 
           adj_EVD_60 = adj_def, 
           SPM_EVO_AA = EVO_AA, 
           SPM_EVD_AA = EVD_AA, 
           adj_EVO_total = off_diff, 
           adj_EVD_total = def_diff, 
           EVO_AA_adj, EVD_AA_adj
           ) %>% 
    data.frame()
  
  
  ####################
  
  
  ## -------------- ##
  ##   Defensemen   ##
  ## -------------- ##
  
  ####################
  
  ## ------------- ##
  ##     EVO_D     ##
  ## ------------- ##
  
  eval_pred_EVO_D <- pred_EVO_D_join %>% 
    select(player:GP)
  
  
  eval_pred_EVO_D$pred_1 <- predict(object = mod_list_EVO_D$lm, 
                                    pred_EVO_D_join[, features_EVO_D_small],
                                    type = "raw")
  
  eval_pred_EVO_D$pred_2 <- predict(object = mod_list_EVO_D$cubist, 
                                    pred_EVO_D_join[, features_EVO_D_large], 
                                    type = "raw")
  
  eval_pred_EVO_D$pred_3 <- predict(object = mod_list_EVO_D$svmLinear, 
                                    pred_EVO_D_join[, features_EVO_D_large], 
                                    type = "raw")
  
  eval_pred_EVO_D <- eval_pred_EVO_D %>% 
    mutate(ensemble = 
             pred_1 * mod_EVO_D_weights[1] + 
             pred_2 * mod_EVO_D_weights[2] + 
             pred_3 * mod_EVO_D_weights[3], 
           EVO = (ensemble / 60) * TOI)
  
  
  # Combine for full join
  EVO_D_GAA <- eval_pred_EVO_D %>% 
    group_by(player, position, season, Team) %>% 
    summarise_at(vars(TOI, GP, EVO), funs(sum)) %>% 
    mutate(EVO_60 = (EVO / TOI) * 60) %>% 
    data.frame()
  
  
  ## ------------- ##
  ##     EVD_D     ##
  ## ------------- ##
  
  eval_pred_EVD_D <- pred_EVD_D_join %>% 
    select(player:GP)
  
  eval_pred_EVD_D$pred_1 <- predict(object = mod_list_EVD_D$lm, 
                                    pred_EVD_D_join[, features_EVD_D_small],
                                    type = "raw")
  
  eval_pred_EVD_D$pred_2 <- predict(object = mod_list_EVD_D$cubist, 
                                    pred_EVD_D_join[, features_EVD_D_large], 
                                    type = "raw")
  
  eval_pred_EVD_D$pred_3 <- predict(object = mod_list_EVD_D$glmnet, 
                                    pred_EVD_D_join[, features_EVD_D_large], 
                                    type = "raw")
  
  eval_pred_EVD_D <- eval_pred_EVD_D %>% 
    mutate(ensemble = 
             pred_1 * mod_EVD_D_weights[1] + 
             pred_2 * mod_EVD_D_weights[2] + 
             pred_3 * mod_EVD_D_weights[3], 
           EVD = (ensemble / 60) * TOI)
  
  
  EVD_D_GAA <- eval_pred_EVD_D %>% 
    group_by(player, position, season, Team) %>%  # added Team
    summarise_at(vars(TOI, GP, EVD), funs(sum)) %>% 
    mutate(EVD_60 = (EVD / TOI) * 60) %>% 
    data.frame()
  
  
  ## ---------------- ##
  ##  Combine EV - D  ##
  ## ---------------- ##
  
  EV_D_overall <- EVO_D_GAA %>% 
    left_join(., EVD_D_GAA, by = c("player", "position", "season", "Team", "TOI", "GP")) %>% 
    #filter(TOI > TOI_cut_EV) %>% 
    group_by(season) %>% 
    mutate(EVO = ifelse(TOI < TOI_cut_EV, (EVO / 60) * TOI, EVO),  # Added "regressing" to average
           EVD = ifelse(TOI < TOI_cut_EV, (EVD / 60) * TOI, EVD),  # Added "regressing" to average
           
           EVO_AA = ((EVO / TOI) - (sum(EVO) / sum(TOI))) * TOI, 
           EVD_AA = ((EVD / TOI) - (sum(EVD) / sum(TOI))) * TOI, 
           EVO_AA_60 = (EVO_AA / TOI) * 60, 
           EVD_AA_60 = (EVD_AA / TOI) * 60
           ) %>% 
    data.frame()
  
  
  # EV FINAL JOIN: Defensemen
  EV_D_team_final <- EV_D_overall %>% 
    left_join(., team_strength_RAPM_EV %>% select(season, Team, TOI, skaters, GF, xGA), 
              by = c("season", "Team"), 
              suffix = c("_player", "_team")
              ) %>% 
    group_by(player, season) %>% 
    mutate(TOI_perc_tot =  TOI_player / TOI_team, 
           adj_off =       (1.6 * GF - (TOI_perc_tot * EVO_AA_60)) / skaters,   # new team adjustment (run 3)
           adj_def =       (1.4 * xGA - (TOI_perc_tot * EVD_AA_60)) / skaters,  # new team adjustment (run 3)
           EVO_AA_60_adj = EVO_AA_60 + adj_off, 
           EVD_AA_60_adj = EVD_AA_60 + adj_def, 
           EVO_AA_adj =    (EVO_AA_60_adj / 60) * TOI_player, 
           EVD_AA_adj =    -1 * ((EVD_AA_60_adj / 60) * TOI_player), 
           EVD_AA =        -1 * EVD_AA,
           off_diff =      EVO_AA_adj - EVO_AA, 
           def_diff =      EVD_AA_adj - EVD_AA
           ) %>% 
    rename(TOI = TOI_player) %>% 
    select(player:GP, 
           TOI_perc_EV = TOI_perc_tot, 
           SPM_EVO_60 = EVO_60, 
           SPM_EVD_60 = EVD_60, 
           SPM_EVO_AA_60 = EVO_AA_60, 
           SPM_EVD_AA_60 = EVD_AA_60, 
           team_TOI_EV = TOI_team, 
           team_skaters_EV = skaters, 
           team_EV_GF = GF, 
           team_EV_xGA = xGA, 
           adj_EVO_60 = adj_off, 
           adj_EVD_60 = adj_def, 
           SPM_EVO_AA = EVO_AA, 
           SPM_EVD_AA = EVD_AA, 
           adj_EVO_total = off_diff, 
           adj_EVD_total = def_diff, 
           EVO_AA_adj, EVD_AA_adj
           ) %>% 
    data.frame()
  
  
  ####################
  
  
  # FINAL F/D JOIN
  ALL_EV <- EV_F_team_final %>% 
    rbind(., EV_D_team_final) %>% 
    rename(TOI_EV = TOI)
  
  }


# Powerplay Offense Combine Function - F & D
fun.ALL_PP_GAA <- function() { 
  
  ## -------------- ##
  ##    Forwards    ##
  ## -------------- ##
  
  ####################
  
  eval_pred_PPO_F <- pred_PPO_F_join %>% 
    select(player:TOI_GP) 
  
  # Predict
  eval_pred_PPO_F$pred_1 <- predict(object = mod_list_PPO_F$cubist, 
                                    pred_PPO_F_join[, features_PPO_F_large], 
                                    type = "raw")
  
  eval_pred_PPO_F$pred_2 <- predict(object = mod_list_PPO_F$bagEarth, 
                                    pred_PPO_F_join[, features_PPO_F_large], 
                                    type = "raw")
  
  eval_pred_PPO_F$pred_3 <- predict(object = mod_list_PPO_F$svmLinear, 
                                    pred_PPO_F_join[, features_PPO_F_large], 
                                    type = "raw")
  
  eval_pred_PPO_F <- eval_pred_PPO_F %>% 
    mutate(ensemble = 
             pred_1 * mod_PPO_F_weights[1] + 
             pred_2 * mod_PPO_F_weights[2] + 
             pred_3 * mod_PPO_F_weights[3], 
           PPO = (ensemble / 60) * TOI)
  
  
  # Combine for full join
  PPO_F_GAA <- eval_pred_PPO_F %>% 
    #filter(TOI > TOI_cut_PP) %>% 
    filter(TOI > 0) %>% 
    group_by(player, position, season, Team) %>%  # added Team
    summarise_at(vars(TOI, GP, PPO), funs(sum)) %>% 
    mutate(PPO_60 = (PPO / TOI) * 60) %>% 
    data.frame()
  
  
  PPO_F_overall <- PPO_F_GAA %>% 
    group_by(season) %>% 
    mutate(PPO = ifelse(TOI < TOI_cut_PP, (PPO / 25) * TOI, PPO),   # "regress" to average for low TOI players 
           
           PPO_AA = ((PPO / TOI) - (sum(PPO) / sum(TOI))) * TOI, 
           PPO_AA_60 = (PPO_AA / TOI) * 60
           ) %>% 
    data.frame()
  
  
  # PP RAPM Team Strength
  PPO_F_team_final <- PPO_F_overall %>% 
    left_join(., team_strength_PP_full_AA %>% select(season, Team, t_TOI_PP, skaters_PP, PPO_GF_AA_team), by = c("Team", "season")) %>% 
    group_by(player, season) %>% 
    mutate(TOI_perc_tot =  TOI / t_TOI_PP, 
           adj_off =       (1.3 * PPO_GF_AA_team - (TOI_perc_tot * PPO_AA_60)) / skaters_PP, 
           PPO_AA_60_adj = PPO_AA_60 + adj_off, 
           PPO_AA_adj =    (PPO_AA_60_adj / 60) * TOI, 
           off_diff =      PPO_AA_adj - PPO_AA
           ) %>% 
    select(player:GP, 
           TOI_perc_PP = TOI_perc_tot, 
           SPM_PPO_60 = PPO_60, 
           SPM_PPO_AA_60 = PPO_AA_60, 
           team_TOI_PP = t_TOI_PP, 
           team_skaters_PP = skaters_PP, 
           team_PPO_GF_AA = PPO_GF_AA_team,
           adj_PPO_60 = adj_off, 
           SPM_PPO_AA = PPO_AA, 
           adj_PPO_total = off_diff, 
           PPO_AA_adj
           ) %>% 
    data.frame()
  
  
  ####################
  
  
  ## -------------- ##
  ##   Defensemen   ##
  ## -------------- ##
  
  ####################
  
  eval_pred_PPO_D <- pred_PPO_D_join %>% 
    select(player:TOI_GP) 
  
  # Predict
  eval_pred_PPO_D$pred_1 <- predict(object = mod_list_PPO_D$lm, 
                                    pred_PPO_D_join[, features_PPO_D_small], 
                                    type = "raw")
  
  eval_pred_PPO_D$pred_2 <- predict(object = mod_list_PPO_D$svmLinear, 
                                    pred_PPO_D_join[, features_PPO_D_large], 
                                    type = "raw")
  
  eval_pred_PPO_D$pred_3 <- predict(object = mod_list_PPO_D$glmnet, 
                                    pred_PPO_D_join[, features_PPO_D_large], 
                                    type = "raw")
  
  eval_pred_PPO_D <- eval_pred_PPO_D %>% 
    mutate(ensemble = 
             pred_1 * mod_PPO_D_weights[1] + 
             pred_2 * mod_PPO_D_weights[2] + 
             pred_3 * mod_PPO_D_weights[3], 
           PPO = (ensemble / 60) * TOI)
  
  
  # Combine for full join
  PPO_D_GAA <- eval_pred_PPO_D %>% 
    #filter(TOI > TOI_cut_PP) %>% 
    filter(TOI > 0) %>% 
    group_by(player, position, season, Team) %>%  # added Team
    summarise_at(vars(TOI, GP, PPO), funs(sum)) %>% 
    mutate(PPO_60 = (PPO / TOI) * 60) %>% 
    data.frame()
  
  
  PPO_D_overall <- PPO_D_GAA %>% 
    group_by(season) %>% 
    mutate(PPO = ifelse(TOI < TOI_cut_PP, (PPO / 25) * TOI, PPO),   # "regress" to average for low TOI players  
           
           PPO_AA = ((PPO / TOI) - (sum(PPO) / sum(TOI))) * TOI, 
           PPO_AA_60 = (PPO_AA / TOI) * 60
           ) %>% 
    data.frame()
  
  
  
  # PP RAPM Team Strength
  PPO_D_team_final <- PPO_D_overall %>% 
    left_join(., team_strength_PP_full_AA %>% select(season, Team, t_TOI_PP, skaters_PP, PPO_GF_AA_team), by = c("Team", "season")) %>% 
    group_by(player, season) %>% 
    mutate(TOI_perc_tot =  TOI / t_TOI_PP, 
           adj_off =       (1.3 * PPO_GF_AA_team - (TOI_perc_tot * PPO_AA_60)) / skaters_PP, 
           PPO_AA_60_adj = PPO_AA_60 + adj_off, 
           PPO_AA_adj =    (PPO_AA_60_adj / 60) * TOI, 
           off_diff =      PPO_AA_adj - PPO_AA
           ) %>% 
    select(player:GP, 
           TOI_perc_PP = TOI_perc_tot, 
           SPM_PPO_60 = PPO_60, 
           SPM_PPO_AA_60 = PPO_AA_60, 
           team_TOI_PP = t_TOI_PP, 
           team_skaters_PP = skaters_PP, 
           team_PPO_GF_AA = PPO_GF_AA_team,
           adj_PPO_60 = adj_off, 
           SPM_PPO_AA = PPO_AA, 
           adj_PPO_total = off_diff, 
           PPO_AA_adj
           ) %>% 
    data.frame()
  
  
  ####################
  
  
  # FINAL F/D JOIN
  ALL_PP <- PPO_F_team_final %>%
    rbind(., PPO_D_team_final) %>% 
    rename(TOI_PP = TOI)
  
  }


# Shorthanded Defense Combine Function - F & D
fun.ALL_SH_GAA <- function() { 
  
  ## -------------- ##
  ##    Forwards    ##
  ## -------------- ##
  
  ####################
  
  eval_pred_SHD_F <- pred_SHD_F_join %>% 
    select(player:TOI_GP) 
  
  # Predict
  eval_pred_SHD_F$pred_1 <- predict(object = mod_list_SHD_F$cubist, 
                                    pred_SHD_F_join[, features_SHD_F_large], 
                                    type = "raw")
  
  eval_pred_SHD_F$pred_2 <- predict(object = mod_list_SHD_F$bagEarth, 
                                    pred_SHD_F_join[, features_SHD_F_large], 
                                    type = "raw")
  
  eval_pred_SHD_F$pred_3 <- predict(object = mod_list_SHD_F$svmLinear, 
                                    pred_SHD_F_join[, features_SHD_F_large], 
                                    type = "raw")
  
  eval_pred_SHD_F <- eval_pred_SHD_F %>% 
    mutate(ensemble = 
             pred_1 * mod_SHD_F_weights[1] + 
             pred_2 * mod_SHD_F_weights[2] + 
             pred_3 * mod_SHD_F_weights[3], 
           SHD = (ensemble / 60) * TOI)
  
  
  # Combine for full join
  SHD_F_GAA <- eval_pred_SHD_F %>% 
    #filter(TOI > TOI_cut_SH) %>% 
    filter(TOI > 0) %>% 
    group_by(player, position, season, Team) %>% 
    summarise_at(vars(TOI, GP, SHD), funs(sum)) %>% 
    mutate(SHD_60 = (SHD / TOI) * 60) %>% 
    data.frame()
  
  SHD_F_overall <- SHD_F_GAA %>% 
    group_by(season) %>% 
    mutate(SHD = ifelse(TOI < TOI_cut_SH, (SHD / 25) * TOI, SHD),   # "regress" to average for low TOI players  
           
           SHD_AA = ((SHD / TOI) - (sum(SHD) / sum(TOI))) * TOI, 
           SHD_AA_60 = (SHD_AA / TOI) * 60
           ) %>% 
    data.frame()
  
  
  # PP RAPM Team Strength
  SHD_F_team_final <- SHD_F_overall %>% 
    left_join(., team_strength_SH_full_AA %>% select(season, Team, t_TOI_SH, skaters_SH, SHD_xG_AA_team), by = c("Team", "season")) %>% 
    group_by(player, season) %>% 
    mutate(TOI_perc_tot =  TOI / t_TOI_SH, 
           adj_def =       (1.3 * SHD_xG_AA_team - (TOI_perc_tot * SHD_AA_60)) / skaters_SH, 
           SHD_AA_60_adj = SHD_AA_60 + adj_def, 
           SHD_AA_adj =    -1 * ((SHD_AA_60_adj / 60) * TOI), 
           SHD_AA =        -1 * SHD_AA,
           def_diff =      SHD_AA_adj - SHD_AA
           ) %>% 
    select(player:GP, 
           TOI_perc_SH = TOI_perc_tot, 
           SPM_SHD_60 = SHD_60, 
           SPM_SHD_AA_60 = SHD_AA_60, 
           team_TOI_SH = t_TOI_SH, 
           team_skaters_SH = skaters_SH, 
           team_SHD_xGA_AA = SHD_xG_AA_team, 
           adj_SHD_60 = adj_def,
           SPM_SHD_AA = SHD_AA, 
           adj_SHD_total = def_diff,
           SHD_AA_adj
           ) %>% 
    data.frame()
  
  
  ####################
  
  
  ## -------------- ##
  ##   Defensemen   ##
  ## -------------- ##
  
  ####################
  
  eval_pred_SHD_D <- pred_SHD_D_join %>% 
    select(player:TOI_GP) 
  
  # Predict
  eval_pred_SHD_D$pred_1 <- predict(object = mod_list_SHD_D$cubist, 
                                    pred_SHD_D_join[, features_SHD_D_large], 
                                    type = "raw")
  
  eval_pred_SHD_D$pred_2 <- predict(object = mod_list_SHD_D$bagEarth, 
                                    pred_SHD_D_join[, features_SHD_D_large], 
                                    type = "raw")
  
  eval_pred_SHD_D$pred_3 <- predict(object = mod_list_SHD_D$glmnet, 
                                    pred_SHD_D_join[, features_SHD_D_large], 
                                    type = "raw")
  
  eval_pred_SHD_D <- eval_pred_SHD_D %>% 
    mutate(ensemble = 
             pred_1 * mod_SHD_D_weights[1] + 
             pred_2 * mod_SHD_D_weights[2] + 
             pred_3 * mod_SHD_D_weights[3], 
           SHD = (ensemble / 60) * TOI)
  
  
  # Combine for full join
  SHD_D_GAA <- eval_pred_SHD_D %>% 
    #filter(TOI > TOI_cut_SH) %>% 
    filter(TOI > 0) %>% 
    group_by(player, position, season, Team) %>%
    summarise_at(vars(TOI, GP, SHD), funs(sum)) %>% 
    mutate(SHD_60 = (SHD / TOI) * 60) %>% 
    data.frame()
  
  SHD_D_overall <- SHD_D_GAA %>% 
    group_by(season) %>% 
    mutate(SHD = ifelse(TOI < TOI_cut_SH, (SHD / 25) * TOI, SHD),   # "regress" to average for low TOI players  
           
           SHD_AA = ((SHD / TOI) - (sum(SHD) / sum(TOI))) * TOI, 
           SHD_AA_60 = (SHD_AA / TOI) * 60
           ) %>% 
    data.frame()
  
  
  # PP RAPM Team Strength
  SHD_D_team_final <- SHD_D_overall %>% 
    left_join(., team_strength_SH_full_AA %>% select(season, Team, t_TOI_SH, skaters_SH, SHD_xG_AA_team), by = c("Team", "season")) %>% 
    group_by(player, season) %>% 
    mutate(TOI_perc_tot =  TOI / t_TOI_SH, 
           adj_def =       (1.3 * SHD_xG_AA_team - (TOI_perc_tot * SHD_AA_60)) / skaters_SH, 
           SHD_AA_60_adj = SHD_AA_60 + adj_def, 
           SHD_AA_adj =    -1 * ((SHD_AA_60_adj / 60) * TOI), 
           SHD_AA =        -1 * SHD_AA,
           def_diff =      SHD_AA_adj - SHD_AA
           ) %>% 
    select(player:GP, 
           TOI_perc_SH = TOI_perc_tot, 
           SPM_SHD_60 = SHD_60, 
           SPM_SHD_AA_60 = SHD_AA_60, 
           team_TOI_SH = t_TOI_SH, 
           team_skaters_SH = skaters_SH, 
           team_SHD_xGA_AA = SHD_xG_AA_team, 
           adj_SHD_60 = adj_def,
           SPM_SHD_AA = SHD_AA, 
           adj_SHD_total = def_diff,
           SHD_AA_adj
           ) %>% 
    data.frame()
  
  
  ####################
  
  
  # FINAL F/D JOIN
  ALL_SH <- SHD_F_team_final %>%
    rbind(., SHD_D_team_final) %>% 
    rename(TOI_SH = TOI)
  
  }


###########################################


## ---------------------------- ##
##   Goalie & Shooter GAR/WAR   ## *** Updated 12/16/18 with factor variable change
## ---------------------------- ##

##################################

fun.shooting_RAPM <- function(pbp_data, strength_) { 
  
  # Objects
  st.shot_events <- c("SHOT",  "GOAL")
  st.fenwick_events <- c("SHOT", "GOAL", "MISS")
  st.corsi_events <- c("SHOT", "GOAL", "MISS", "BLOCK" )
  st.strength_states <- c("3v3", "5v5", "4v4", "5v4", "4v5", "5v3", "3v5", "4v3", "3v4", "5vE", "Ev5", "4vE", "Ev4") %>% as.factor() # specific to shooting GAA
  st.even_strength <- c("5v5", "4v4", "3v3") %>% as.factor()
  st.pp_strength <- c("5v4", "4v5", "5v3", "3v5", "4v3", "3v4") %>% as.factor()
  st.empty_net <- c("5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor()
  
  OR2dProb <- function(OR, prob1) {
    
    prob2 <- (OR - 1) * (prob1 / (1 - prob1)) / (1 + (OR - 1) * (prob1 / (1 - prob1)))
    
    return(prob2)
    
    } # Manny Perry's Function from GitHub
  strReverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
  
  
  ## -------------------- ##
  ##   Data Preparation   ##
  ## -------------------- ##
  
  ##########################
  
  season_ <- unique(pbp_data$season)
  
  print(paste("season:", season_), quote = F)
  
  print("Data Prep", quote = F)
  
  # Qualify goalies
  fun.goalie_qual <- function(data, cutoff, strength) {
    
    st.even_strength <- c("5v5", "4v4", "3v3") %>% as.factor()
    st.strength_states <- c("3v3", "5v5", "4v4", "5v4", "4v5", "5v3", "3v5", "4v3", "3v4", "5vE", "Ev5", "4vE", "Ev4") %>% as.factor()
    
    # Filter to strength state
    if (strength == "All") { 
      hold <- filter(data, game_strength_state %in% st.strength_states)
    
      }
    if (strength == "EV") { 
      hold <- filter(data, game_strength_state %in% st.even_strength)
    
      }
    
    # Calculate TOI for Goalies
    pbp_goalie_H <- hold %>% 
      group_by(home_goalie, home_team, season) %>% 
      summarise(TOI = sum(event_length)) %>% 
      filter(!is.na(TOI),
             home_goalie != 50
             ) %>% 
      rename(goalie = home_goalie, 
             Team =   home_team
             ) %>% 
      data.frame()
    
    pbp_goalie_A <- hold %>% 
      group_by(away_goalie, away_team, season) %>% 
      summarise(TOI = sum(event_length)) %>% 
      filter(!is.na(TOI), 
             away_goalie != 50
             ) %>% 
      rename(goalie = away_goalie, 
             Team =   away_team
             ) %>% 
      data.frame()
    
    pbp_goalie <- pbp_goalie_A %>% 
      rbind(., pbp_goalie_H) %>% 
      group_by(goalie, Team) %>% 
      summarise(TOI = sum(TOI) / 60) %>% 
      mutate(qual = ifelse(TOI >= cutoff, 1, 0), 
             player = paste0(goalie, ".", Team)
             ) %>% 
      ungroup() %>% 
      select(player, qual) %>% 
      data.frame()
    
    }
  goalie_qual <- fun.goalie_qual(data =     pbp_data, 
                                 cutoff =   1, 
                                 strength = strength_)
  
  # Qualify skaters + add qualified goalies
  fun.qualified_GF <- function(data, goalie_data, strength, qual_cut) {
    
    st.even_strength <- c("5v5", "4v4", "3v3") %>% as.factor()
    st.strength_states <- c("3v3", "5v5", "4v4", "5v4", "4v5", "5v3", "3v5", "4v3", "3v4", "5vE", "Ev5", "4vE", "Ev4") %>% as.factor()
    
    # Filter to strength state
    if (strength == "All") { 
      hold <- filter(data, game_strength_state %in% st.strength_states)
    
      }
    if (strength == "EV") { 
      hold <- filter(data, game_strength_state %in% st.even_strength)
    
      }
    
    qualified <- hold %>% 
      filter(event_type %in% st.fenwick_events) %>% 
      group_by(event_player_1) %>% 
      summarise(iFF = sum(event_type %in% st.fenwick_events)) %>% 
      mutate(qual = 1 * (iFF > qual_cut)) %>% 
      rename(player = event_player_1) %>% 
      select(player, qual) %>% 
      data.frame()
    
    qualified_return <- qualified %>% 
      rbind(., goalie_data) %>% 
      data.frame()
    
    return(qualified_return)
  
    }
  Qualified_GF <- fun.qualified_GF(data =        pbp_data, 
                                   goalie_data = goalie_qual, 
                                   strength =    strength_, 
                                   qual_cut =    0)
  rm(goalie_qual)
  
  # Prepare design matrix
  fun.pbp_prepare <- function(data, strength) {
    
    st.shot_events <- c("SHOT",  "GOAL")
    st.fenwick_events <- c("SHOT", "GOAL", "MISS")
    st.even_strength <- c("5v5", "4v4", "3v3") %>% as.factor()
    st.strength_states <- c("3v3", "5v5", "4v4", "5v4", "4v5", "5v3", "3v5", "4v3", "3v4", "5vE", "Ev5", "4vE", "Ev4") %>% as.factor()
    
    # variables selected later in function
    if (strength == "All") { 
      
      return_pbp <- data %>% 
        rename(pred_goal = pred_XGB_7, 
               shooter = event_player_1
               ) %>% 
        filter(event_type %in% st.fenwick_events, 
               game_period < 5, 
               (game_strength_state %in% c("5v5", "4v4", "3v3", "5v4", "4v5", "5v3", "3v5", "4v3", "3v4")) | 
                 ((game_strength_state == "5vE" & event_team == away_team) | (game_strength_state == "Ev5" & event_team == home_team)) | 
                 ((game_strength_state == "4vE" & event_team == away_team) | (game_strength_state == "Ev4" & event_team == home_team)), 
               !is.na(pred_goal)
               ) %>% 
        left_join(., player_position %>% rename(shooter = player), by = "shooter") %>% 
        mutate(is_goal = 1 * (event_type == "GOAL"), 
               
               goalie = ifelse(event_team == home_team, paste0(away_goalie, ".", away_team), paste0(home_goalie, ".", home_team)), 
               
               home_lead =           home_score - away_score, 
               shooter_score_trail = 1 * ((event_team == home_team & home_lead < 0) | (event_team == away_team & home_lead > 0)), 
               shooter_score_even =  1 * (home_lead == 0), 
               shooter_score_lead =  1 * ((event_team == home_team & home_lead > 0) | (event_team == away_team & home_lead < 0)), 
               
               state_5v5 = 1 * (game_strength_state == "5v5"),
               state_4v4 = 1 * (game_strength_state == "4v4"),
               state_3v3 = 1 * (game_strength_state == "3v3"),
               state_5v4 = 1 * (game_strength_state %in% c("5v4", "4v5")),
               state_5v3 = 1 * (game_strength_state %in% c("5v3", "3v5")),
               state_4v3 = 1 * (game_strength_state %in% c("4v3", "3v4")),
               state_5vE = 1 * ((game_strength_state == "5vE" & event_team == away_team) | (game_strength_state == "Ev5" & event_team == home_team)),
               state_4vE = 1 * ((game_strength_state == "4vE" & event_team == away_team) | (game_strength_state == "Ev4" & event_team == home_team)),
               
               shooter_is_home = 1 * (event_team == home_team), 
               shooter_is_F =    1 * (position == 1), 
               shooter_is_D =    1 * (position == 2) 
               ) %>% 
        left_join(., btb, by = c("game_id")) %>% 
        select(is_goal, pred_goal,
               shooter, 
               goalie, 
               state_5v5, state_4v4, state_3v3, 
               state_5v4, state_5v3, state_4v3, state_5vE, state_4vE, 
               shooter_score_trail, shooter_score_even, shooter_score_lead, 
               shooter_is_home, 
               shooter_is_F, shooter_is_D, 
               home_btb, away_btb
               ) %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
        data.frame()
      
      } 
    
    return(return_pbp)
  
    }
  pbp_part <- fun.pbp_prepare(data =     pbp_data, 
                              strength = strength_)
  
  
  # Create data frame with all players, goalies, teams, and event_types and their respective IDs
  fun.names_match <- function(sub_data2, qual_data, player_data) {
    
    # Skaters
    skaters <- mutate(player_data, ID = row_number() + 10000)
    
    # Goalies - updated to work with data above
    fun.goalie_find <- function(data) {
      
      goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data$goalie, data$goalie))))))
      
      goalie_return$player <- as.character(goalie_return$player)
      
      return(goalie_return)
    
      }
    goalies <- fun.goalie_find(sub_data2)
    
    goalies <- goalies %>% 
      mutate(position = NA, 
             ID = NA)
    
    goalies$position <- 3
    
    goalies <- goalies %>% 
      arrange(player) %>% 
      mutate(ID = row_number() + 20000)
    
    # Combine
    all <- skaters %>% 
      rbind(., goalies) %>% 
      filter(player != 0) %>% 
      left_join(., qual_data, by = "player") %>% 
      mutate(qual = ifelse(is.na(qual), 0, qual)) %>% 
      arrange(ID)
    
    return(all)
  
    }
  names_match <- fun.names_match(sub_data2 =   pbp_part, 
                                 qual_data =   Qualified_GF, 
                                 player_data = player_position)
  
  # Determine non-qualified players
  exclude <- names_match %>% 
    filter(ID > 10000, qual == 0) %>% 
    select(ID)
  
  exclude <- as.vector(exclude[, 1])
  
  # Convert prepared pbp data frame to numeric values
  fun.IDs <- function(data) {
    
    # Shooter / Goalie
    data$shooter <- names_match$ID[match(data$shooter, names_match$player)]
    data$goalie <- names_match$ID[match(data$goalie, names_match$player)]
    
    # Make Empty Slots 0s
    data[is.na(data)] <- 0
    
    return(data)
  
    }
  pbp_matrix <- fun.IDs(pbp_part)
  
  
  ##########################
  
  
  ## ------------------------ ##
  ##   Make Dummy Variables   ##
  ## ------------------------ ##
  
  ##########################
  
  print("Dummy Variables", quote = F)
  
  # Convert to matrix
  pbp_matrix <- data.matrix(pbp_matrix)
  
  # Get player names
  groups <- sort(unique(c(pbp_matrix[, "shooter"], pbp_matrix[, "goalie"])), decreasing = F)
  
  # Determine columns
  tmp <- lapply(groups, function(x, pbp_matrix)  which(pbp_matrix[, "shooter"] == x | pbp_matrix[, "goalie"] == x), pbp_matrix = pbp_matrix)
  
  # Make Dummy Variables
  j = rep(seq_along(tmp), lengths(tmp))
  i = unlist(tmp)
  dummies <- sparseMatrix(i, j, dims = c(nrow(pbp_matrix), length(groups)))
  
  # Add column names
  colnames(dummies) <- groups
  
  # Combine
  pbp_sparse <- cbind(pbp_matrix, dummies)
  
  
  ##########################
  
  
  ## ------------------------- ##
  ##   Model Setup / Combine   ##
  ## ------------------------- ##
  
  ##########################
  
  ## sparse matrix column names:
  # [1]  "is_goal"             "pred_goal"           "shooter"             "goalie"              "state_5v5"           "state_4v4"           "state_3v3"          
  # [8]  "state_5v4"           "state_5v3"           "state_4v3"           "state_5vE"           "state_4vE"           "shooter_score_trail" "shooter_score_even" 
  # [15] "shooter_score_lead"  "shooter_is_home"     "shooter_is_F"        "shooter_is_D"        "home_btb"            "away_btb"
  
  
  is_goal <- pbp_sparse[, "is_goal"]
  shooting_design <- pbp_sparse[, -c(1, 3:4, 5, 14, 17)] # remove excess columns, corrected factor variables, added 12/12/18
  
  
  print("Cross Validation", quote = F)
  
  registerDoMC(cores = 2)
  
  CV_results <- cv.glmnet(x = shooting_design, 
                          y = is_goal, 
                          family = "binomial", 
                          alpha = 0, 
                          nfolds = 10, 
                          #standardize = FALSE, # not using standardization for single-season model
                          parallel = TRUE)
  gc()
  
  print(paste("lambdas tested:", length(CV_results$lambda)), quote = F)
  print(paste("lambda_min:", round(CV_results$lambda.min, 6)), quote = F)
  
  # Retrieve coefficients
  shooting_coefs_raw <- data.frame(variable = c("Intercept", colnames(shooting_design)),
                                   coefficient = matrix(exp(coef(CV_results, s = "lambda.min")))
                                   )
  shooting_coefs <- shooting_coefs_raw
  shooting_coefs$variable <- names_match$player[match(shooting_coefs$variable, names_match$ID)]
  
  
  # Create Data Frames
  GAA_Shooters <- pbp_part %>% 
    rename(player = shooter) %>% 
    mutate(n = 1) %>% 
    group_by(player) %>% 
    summarise(iFF = sum(n)) %>% 
    left_join(., shooting_coefs %>% rename(player = variable), by = "player") %>% 
    mutate(prob =   OR2dProb(coefficient, sum(is_goal) / nrow(pbp_part)), 
           GAA =    OR2dProb(coefficient, sum(is_goal) / nrow(pbp_part)) * iFF, # baseline fSh% determined from data used to construct design matrix
           season = season_
           ) %>% 
    left_join(., player_position, by = "player") %>% 
    select(player, position, season, iFF, coefficient, prob, GAA) %>% 
    data.frame()
  
  GAA_Goalies <- pbp_part %>% 
    rename(player = goalie) %>% 
    mutate(n = 1) %>% 
    group_by(player) %>% 
    summarise(FA = sum(n)) %>% 
    left_join(., shooting_coefs %>% rename(player = variable), by = "player") %>% 
    mutate(prob =     OR2dProb(coefficient, sum(is_goal) / nrow(pbp_part)),  
           GAA =      -1 * OR2dProb(coefficient, sum(is_goal) / nrow(pbp_part)) * FA,  # baseline fSh% determined from data used to construct design matrix
           Team =     substr(player, nchar(player) - 2, nchar(player)), 
           player =   substr(player, 1, nchar(player) - 4), 
           position = 3, 
           season =   season_
           ) %>% 
    select(player, position, season, Team, FA, coefficient, prob, GAA) %>% 
    data.frame()
  
  
  ##########################
  
  gc()
  
  # Return
  return_list <- list(shooter_df =    GAA_Shooters, 
                      goalie_df =     GAA_Goalies, 
                      names_match =   names_match, 
                      CV_object =     CV_results, 
                      coefs_raw =     shooting_coefs_raw, 
                      goal_vec =      is_goal, 
                      design_matrix = shooting_design, 
                      full_matrix =   pbp_sparse
                      )
  
  return(return_list)
  
  }


##################################


## -------------------- ##
##   Skater RAPM - EV   ## *** Updated 12/16/18 with cv.glmnet change
## -------------------- ##

##########################

fun.RAPM_EV_all <- function(pbp_data, games_data) { 
  
  ## ----------------------------- ##
  ##   Qualify Skaters / Goalies   ##
  ## ----------------------------- ##
  
  print("Qualify Players", quote = F)
  
  # No TOI Cutoff
  F_TOI_cut <- 1
  D_TOI_cut <- 1
  G_TOI_cut <- 1
  
  # Skaters
  qual_skater <- games_data %>% 
    group_by(player, Team) %>% 
    summarise(TOI = sum(TOI)) %>% 
    left_join(., player_position, by = "player") %>% 
    group_by(player, position) %>% 
    mutate(Team = paste0(Team, collapse = "/")) %>% 
    group_by(player, Team, position) %>% 
    summarise(TOI = sum(TOI)) %>% 
    mutate(qual = 1 * (position == 1 & TOI >= F_TOI_cut | position == 2 & TOI >= D_TOI_cut)) %>% 
    filter(qual == 1) %>% 
    data.frame()
  
  # Goalies
  pbp_goalie_H <- pbp_data %>% 
    filter(game_strength_state %in% st.even_strength) %>% 
    group_by(home_goalie, season) %>% 
    summarise(TOI = sum(event_length)) %>% 
    filter(!is.na(TOI) & home_goalie != 50) %>% 
    rename(player = home_goalie) %>% 
    data.frame()
  
  pbp_goalie_A <- pbp_data %>% 
    filter(game_strength_state %in% st.even_strength) %>% 
    group_by(away_goalie, season) %>% 
    summarise(TOI = sum(event_length)) %>% 
    filter(!is.na(TOI) & away_goalie != 50) %>% 
    rename(player = away_goalie) %>% 
    data.frame()
  
  pbp_goalie <- pbp_goalie_A %>% 
    rbind(., pbp_goalie_H) %>% 
    group_by(player) %>% 
    summarise(TOI = sum(TOI / 60)) %>% 
    mutate(qual = 1 * (TOI >= G_TOI_cut)) %>% 
    filter(qual == 1) %>% 
    rename(TOI_goalie = TOI) %>% 
    data.frame()
  
  # TOI cutoffs
  F_TOI_cut <- min(filter(qual_skater, position == 1)$TOI)
  D_TOI_cut <- min(filter(qual_skater, position == 2)$TOI)
  G_TOI_cut <- as.numeric(min(pbp_goalie[, 2]))
  
  print(paste0("Forwards: ", nrow(filter(qual_skater, position == 1))))
  print(paste0("Defensemen: ", nrow(filter(qual_skater, position == 2))))
  print(paste0("Goalies: ", nrow(pbp_goalie)))
  print(paste0("D/F ratio: ", round(nrow(filter(qual_skater, position == 2)) / nrow(filter(qual_skater, position == 1)), 4)))
  
  rm(pbp_goalie_A, pbp_goalie_H)
  
  
  ## ---------------------------- ##
  ##   RAPM Design Matrix Setup   ##
  ## ---------------------------- ##
  
  print("Design Matrix Setup", quote = F)
  
  # Qualify goalies
  fun.goalie_qual <- function(data, cutoff) {
    
    hold <- filter(data, game_strength_state %in% st.even_strength)
    
    pbp_goalie_H <- hold %>% 
      group_by(home_goalie, season) %>% 
      summarise(TOI = sum(event_length)) %>% 
      filter(!is.na(TOI) & home_goalie != 50) %>% 
      rename(player = home_goalie) %>% 
      data.frame()
    
    pbp_goalie_A <- hold %>% 
      group_by(away_goalie, season) %>% 
      summarise(TOI = sum(event_length)) %>% 
      filter(!is.na(TOI) & away_goalie != 50) %>% 
      rename(player = away_goalie) %>% 
      data.frame()
    
    pbp_goalie <- pbp_goalie_A %>% 
      rbind(., pbp_goalie_H) %>% 
      group_by(player) %>% 
      summarise(TOI = sum(TOI) / 60) %>% 
      mutate(qual = ifelse(TOI >= cutoff, 1, 0)) %>% 
      select(player, qual) %>% 
      data.frame()
    
    }
  goalie_qual <- fun.goalie_qual(data =   pbp_data, 
                                 cutoff = G_TOI_cut)
  
  
  # Qualify skaters + add qualified goalies
  fun.qualified_GF <- function(data, goalie_data, f_cut, d_cut) {
    
    qualified <- data %>% 
      group_by(player) %>% 
      summarise(TOI = round(sum(TOI), 2)) %>% 
      left_join(., player_position, by = "player") %>% 
      mutate(qual = ifelse(TOI >= f_cut & position == 1, 1, 
                           ifelse(TOI >= d_cut & position == 2, 1, 0))
             ) %>% 
      select(player, qual)
    
    qualified_return <- qualified %>% 
      rbind(., goalie_data) %>% 
      data.frame()
    
    return(qualified_return)
  
    }
  Qualified_GF <- fun.qualified_GF(data = games_data, 
                                   goalie_data = goalie_qual, 
                                   f_cut = F_TOI_cut, 
                                   d_cut = D_TOI_cut)
  rm(goalie_qual)
  
  
  # Prepare pbp (initial) - filter to EV, select columns, join back-to-back data
  fun.pbp_prepare <- function(data) {
    
    pbp_part <- data %>% 
      filter(event_type %in% c("GOAL", "SHOT", "MISS", "BLOCK", "ON", "TAKE", "GIVE", "HIT", "FAC"), 
             game_strength_state %in% st.even_strength, 
             game_period < 5
             ) %>% 
      mutate(scradj = home_score - away_score, 
             home_lead = ifelse(scradj >= 3, 3, 
                                ifelse(scradj <= -3, -3, scradj)), 
             event_length = ifelse(is.na(event_length), 0, event_length), 
             event_team = ifelse(is.na(event_team), 0, event_team), 
             home_zonestart = ifelse(is.na(home_zonestart), 0, home_zonestart)
             ) %>% 
      rename(pred_goal = pred_XGB_7) %>% 
      select(game_id, event_index, 
             home_on_1:away_on_6, 
             home_goalie, away_goalie, 
             home_team, away_team, 
             game_strength_state,
             event_length, 
             event_team, 
             event_type, 
             home_lead, 
             home_zonestart, 
             pred_goal
             ) %>% 
      left_join(., btb, by = c("game_id")) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
    
    return(pbp_part)
  
    }
  pbp_part <- fun.pbp_prepare(pbp_data)
  
  
  # Create data frame with all players, goalies, teams, and event_types and their respective IDs
  fun.names_match <- function(sub_data2, qual_data, player_data) {
    
    # Skaters
    skaters <- mutate(player_data, ID = row_number() + 10000)
    
    # Goalies
    fun.goalie_find <- function(data) {
      
      goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data$home_goalie, data$away_goalie))))))
      
      goalie_return$player <- as.character(goalie_return$player)
      
      return(goalie_return)
    
      }
    goalies <- fun.goalie_find(sub_data2)
    
    goalies <- goalies %>% 
      mutate(position = NA, 
             ID = NA)
    
    goalies$position <- 3
    
    goalies <- goalies %>% 
      arrange(player) %>% 
      mutate(ID = row_number() + 20000)
    
    # Teams
    teams_str <- unique(na.omit(sub_data2$event_team))
    teams <- data.frame(matrix(nrow = length(teams_str), ncol = 3))
    names(teams) <- c("player", "position", "ID")
    teams$player <- teams_str
    teams$position <- 4
    
    teams <- teams %>% 
      arrange(player) %>% 
      mutate(ID = row_number())
    
    # Event Type
    event_str <- unique(na.omit(sub_data2$event_type))
    event <- data.frame(matrix(nrow = length(event_str), ncol = 3))
    names(event) <- c("player", "position", "ID")
    event$player <- event_str
    event$position <- 5
    
    event <- event %>% 
      arrange(player) %>%
      mutate(ID = row_number() + 100)
    
    # Strength State
    st.strength <- unique(na.omit(sub_data2$game_strength_state))
    strength <- data.frame(matrix(nrow = length(st.strength), ncol = 3))
    names(strength) <- c("player", "position", "ID")
    strength$player <- st.strength
    strength$position <- 6
    
    strength <- strength %>% 
      arrange(player) %>% 
      mutate(ID = row_number() + 200)
    
    # Combine
    all <- teams %>% 
      rbind(., skaters, goalies, event, strength) %>% 
      filter(player != 0) %>% 
      left_join(., qual_data, by = "player") %>% 
      mutate(qual = ifelse(is.na(qual), 0, qual)) %>% 
      arrange(ID)
    
    return(all)
  
    }
  names_match <- fun.names_match(sub_data2 =   pbp_data, 
                                 qual_data =   Qualified_GF, 
                                 player_data = player_position %>% filter(position != 3))
  
  
  # Determine non-qualified players
  exclude <- names_match %>% 
    filter(ID > 10000, qual == 0) %>% 
    select(ID)
  
  exclude <- as.vector(exclude[, 1])
  
  
  # Identify and save specific event type IDs for dummy function/creation below
  st.corsi_events <-   names_match[which(names_match[, 1] %in% c("SHOT", "GOAL", "MISS", "BLOCK")), 3]
  st.fenwick_events <- names_match[which(names_match[, 1] %in% c("SHOT", "GOAL", "MISS")), 3]
  st.goal_ID <-        names_match[which(names_match[, 1] %in% c("GOAL")), 3] 
  st.fac_ID <-         names_match[which(names_match[, 1] %in% c("FAC")), 3]
  st.on_ID <-          names_match[which(names_match[, 1] %in% c("ON")), 3]
  st.5v5 <-            names_match[which(names_match[, 1] %in% c("5v5")), 3]
  st.4v4 <-            names_match[which(names_match[, 1] %in% c("4v4")), 3]
  st.3v3 <-            names_match[which(names_match[, 1] %in% c("3v3")), 3]
  
  
  # Convert prepared pbp data frame to numeric values
  fun.IDs <- function(data) {
    
    data$game_id <- as.numeric(data$game_id)
    
    # Home Players
    data$home_on_1 <- names_match$ID[match(data$home_on_1, names_match$player)]
    data$home_on_2 <- names_match$ID[match(data$home_on_2, names_match$player)]
    data$home_on_3 <- names_match$ID[match(data$home_on_3, names_match$player)]
    data$home_on_4 <- names_match$ID[match(data$home_on_4, names_match$player)]
    data$home_on_5 <- names_match$ID[match(data$home_on_5, names_match$player)]
    data$home_on_6 <- names_match$ID[match(data$home_on_6, names_match$player)]
    
    # Away Players
    data$away_on_1 <- names_match$ID[match(data$away_on_1, names_match$player)]
    data$away_on_2 <- names_match$ID[match(data$away_on_2, names_match$player)]
    data$away_on_3 <- names_match$ID[match(data$away_on_3, names_match$player)]
    data$away_on_4 <- names_match$ID[match(data$away_on_4, names_match$player)]
    data$away_on_5 <- names_match$ID[match(data$away_on_5, names_match$player)]
    data$away_on_6 <- names_match$ID[match(data$away_on_6, names_match$player)]
    
    # Goalies
    data$home_goalie <- names_match$ID[match(data$home_goalie, names_match$player)]
    data$away_goalie <- names_match$ID[match(data$away_goalie, names_match$player)]
    
    # Teams
    data$event_team <- names_match$ID[match(data$event_team, names_match$player)]
    data$home_team <-  names_match$ID[match(data$home_team, names_match$player)]
    data$away_team <-  names_match$ID[match(data$away_team, names_match$player)]
    
    # Event Type
    data$event_type <- names_match$ID[match(data$event_type, names_match$player)]
    
    # Game Strength
    data$game_strength_state <- names_match$ID[match(data$game_strength_state, names_match$player)]
    
    # Make Empty Slots 0s
    data[is.na(data)] <- 0
    
    return(data)
  }
  pbp_part <- fun.IDs(pbp_part)
  
  
  
  ## -------------------------------- ##
  ##    Create RAPM Design Matrix     ##
  ## -------------------------------- ##
  
  print("Create RAPM Design Matrix", quote = F)
  
  # GF60 - create APM sparse matrix  
  fun.APMsparse_GF <- function(data) {
    
    ### Create Home Matrix
    print(" -- home_matrix")
    
    test.H <- data %>% 
      arrange(game_id, event_index) %>% 
      mutate(shift_ID = cumsum(event_type == st.on_ID), 
             off_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 3), 
             def_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 1), 
             off_lead = home_lead
             ) %>% 
      group_by(game_id, 
               shift_ID, 
               game_strength_state, 
               home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6,
               away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, 
               off_lead
               ) %>% 
      summarise(home_goalie = first(home_goalie), 
                away_goalie = first(away_goalie), 
                off_team = first(home_team), 
                def_team = first(away_team), 
                length = sum(event_length),
                GF60 = (sum(event_type == st.goal_ID & event_team == home_team) / sum(event_length)) * 3600, 
                CF60 =  (sum(event_type %in% st.corsi_events & event_team == home_team) / sum(event_length)) * 3600, 
                xGF60 = (sum((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal) / sum(event_length)) * 3600, 
                off_zonestart = sum(off_zonestart), 
                def_zonestart = sum(def_zonestart), 
                btb = first(home_btb)
                ) %>% 
      filter(length > 0) %>% 
      ungroup() %>% 
      add_column(., n = 0, .before = 1) %>% 
      mutate(n = as.numeric(row_number()), 
             score_down_3 = 1 * (off_lead <= -3), 
             score_down_2 = 1 * (off_lead == -2), 
             score_down_1 = 1 * (off_lead == -1), 
             score_up_1 =   1 * (off_lead ==  1), 
             score_up_2 =   1 * (off_lead ==  2), 
             score_up_3 =   1 * (off_lead >=  3), 
             score_even =   1 * (off_lead ==  0), 
             score_trail =  1 * (off_lead < 0), 
             score_lead =   1 * (off_lead > 0), 
             off_zonestart = 1 * (off_zonestart >= 1),
             def_zonestart = 1 * (def_zonestart >= 1),
             state_5v5 = 1 * (game_strength_state == st.5v5),
             state_4v4 = 1 * (game_strength_state == st.4v4),
             state_3v3 = 1 * (game_strength_state == st.3v3),
             is_home = 1
             ) %>% 
      select(-c(game_id, off_lead, shift_ID)) %>% 
      data.matrix()
    
    
    # Column Names
    print(" -- get_names")
    
    # Retrieve player names
    groups_d <- unique(as.vector(test.H[, grep("_on_", colnames(test.H))]))
    
    # Remove Missing Slots & Remove Non-Qualified Players
    groups_d <- groups_d[!groups_d %in% 0]
    groups_d <- groups_d[!groups_d %in% exclude]
    
    # Remove Goalies for Offense groups
    groups_o <- groups_d[!groups_d %in% c(20000:30000)]
    
    # Order Smallest to Largest
    groups_d <- sort(groups_d, decreasing = FALSE)
    groups_o <- sort(groups_o, decreasing = FALSE)
    
    
    
    ### Home Offense
    print(" -- home_offense")
    
    # Determine Columns
    tmp <- lapply(groups_o, function(x, test.H)  which(test.H[, "home_on_1"] == x | 
                                                       test.H[, "home_on_2"] == x |
                                                       test.H[, "home_on_3"] == x |
                                                       test.H[, "home_on_4"] == x |
                                                       test.H[, "home_on_5"] == x |
                                                       test.H[, "home_on_6"] == x), test.H = test.H)
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_o <- sparseMatrix(i, j, dims = c(nrow(test.H), length(groups_o)))
    # Rename
    colnames(dummies_o) <- groups_o
    colnames(dummies_o) <- paste(colnames(dummies_o), ".o", sep = "")
    
    
    ### Home Defense
    print(" -- home_defense")
    
    # Determine Columns
    tmp <- lapply(groups_d, function(x, test.H)  which((test.H[, "away_on_1"] == x | 
                                                        test.H[, "away_on_2"] == x |
                                                        test.H[, "away_on_3"] == x |
                                                        test.H[, "away_on_4"] == x |
                                                        test.H[, "away_on_5"] == x |
                                                        test.H[, "away_on_6"] == x) & test.H[, "home_goalie"] != x), test.H = test.H)
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_d <- sparseMatrix(i, j, dims = c(nrow(test.H), length(groups_d)))
    # Rename
    colnames(dummies_d) <- groups_d
    colnames(dummies_d) <- paste(colnames(dummies_d), ".d", sep = "")
    
    # Combine
    test_sparse.H <- cbind(test.H, dummies_o, dummies_d)
    
    rm(test.H)
    gc()
    
    
    ##----------------------##
    
    
    ### Create Away Matrix
    print(" -- away_matrix")
    
    test.A <- data %>% 
      arrange(game_id, event_index) %>% 
      mutate(shift_ID = cumsum(event_type == st.on_ID), 
             off_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 1), 
             def_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 3), 
             off_lead = -1 * home_lead
             ) %>% 
      group_by(game_id, 
               shift_ID, 
               game_strength_state, 
               home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6,
               away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, 
               off_lead
               ) %>% 
      summarise(home_goalie = first(home_goalie), 
                away_goalie = first(away_goalie), 
                off_team = first(away_team), 
                def_team = first(home_team), 
                length = sum(event_length),
                GF60 = (sum(event_type == st.goal_ID & event_team == away_team) / sum(event_length)) * 3600, 
                CF60 =  (sum(event_type %in% st.corsi_events & event_team == away_team) / sum(event_length)) * 3600, 
                xGF60 = (sum((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal) / sum(event_length)) * 3600, 
                off_zonestart = sum(off_zonestart), 
                def_zonestart = sum(def_zonestart), 
                btb = first(away_btb)
                ) %>% 
      filter(length > 0) %>% 
      ungroup() %>% 
      add_column(., n = 0, .before = 1) %>% 
      mutate(n = as.numeric(row_number()), 
             score_down_3 = 1 * (off_lead <= -3), 
             score_down_2 = 1 * (off_lead == -2), 
             score_down_1 = 1 * (off_lead == -1), 
             score_up_1 =   1 * (off_lead ==  1), 
             score_up_2 =   1 * (off_lead ==  2), 
             score_up_3 =   1 * (off_lead >=  3), 
             score_even =   1 * (off_lead ==  0), 
             score_trail =  1 * (off_lead < 0), 
             score_lead =   1 * (off_lead > 0), 
             off_zonestart = 1 * (off_zonestart >= 1),
             def_zonestart = 1 * (def_zonestart >= 1),
             state_5v5 = 1 * (game_strength_state == st.5v5),
             state_4v4 = 1 * (game_strength_state == st.4v4),
             state_3v3 = 1 * (game_strength_state == st.3v3),
             is_home = 0
             ) %>% 
      select(-c(game_id, off_lead, shift_ID)) %>% 
      data.matrix()
    
    
    ### Away Offense
    print(" -- away_offense")
    
    # Determine Columns
    tmp <- lapply(groups_o, function(x, test.A)  which(test.A[, "away_on_1"] == x | 
                                                       test.A[, "away_on_2"] == x |
                                                       test.A[, "away_on_3"] == x |
                                                       test.A[, "away_on_4"] == x |
                                                       test.A[, "away_on_5"] == x |
                                                       test.A[, "away_on_6"] == x), test.A = test.A)
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_o <- sparseMatrix(i, j, dims = c(nrow(test.A), length(groups_o)))
    # Rename
    colnames(dummies_o) <- groups_o
    colnames(dummies_o) <- paste(colnames(dummies_o), ".o", sep = "")
    
    ### Away Defense
    print(" -- away_defense")
    # Determine Columns
    tmp <- lapply(groups_d, function(x, test.A)  which((test.A[, "home_on_1"] == x | 
                                                        test.A[, "home_on_2"] == x |
                                                        test.A[, "home_on_3"] == x |
                                                        test.A[, "home_on_4"] == x |
                                                        test.A[, "home_on_5"] == x |
                                                        test.A[, "home_on_6"] == x) & test.A[, "away_goalie"] != x), test.A = test.A)
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_d <- sparseMatrix(i, j, dims = c(nrow(test.A), length(groups_d)))
    # Rename
    colnames(dummies_d) <- groups_d
    colnames(dummies_d) <- paste(colnames(dummies_d), ".d", sep = "")
    
    # Combine
    test_sparse.A <- cbind(test.A, dummies_o, dummies_d)
    
    rm(test.A)
    gc()
    
    
    #####  Big Join  #####
    print(" -- combine")
    
    test_all <- rbind(test_sparse.H, test_sparse.A)
    
    rm(test_sparse.H, test_sparse.A)
    gc()
    
    return(test_all)
  
    }
  APM <- fun.APMsparse_GF(pbp_part)
  
  rm(pbp_part)
  gc()
  
  
  # All players TOI and position in RAPM design matrix
  player_base_ <- pbp_goalie %>% 
    rename(TOI = TOI_goalie) %>% 
    mutate(position = 3) %>% 
    select(player, TOI, position) %>% 
    rbind(select(qual_skater, player, TOI, position)) %>% 
    left_join(., select(names_match, player, ID), by = "player") %>% 
    arrange(player) %>% 
    data.frame()
  
  
  ## ------------------------ ##
  ##          Model           ##
  ## ------------------------ ##
  
  print("Cleanup / Cross Validation", quote = F)
  
  # Cleanup / separate target, weights, and predictors
  length_l <- list(APM[, "length"])
  GF60_l <-   list(APM[, "GF60"])
  CF60_l <-   list(APM[, "CF60"])
  xGF60_l <-  list(APM[, "xGF60"])
  
  # Remove NaNs
  length <- unlist(rapply(length_l, f = function(x) ifelse(x == 0, 1, x), how = "replace")) # correct length of 0
  GF60 <-   unlist(rapply(GF60_l, f = function(x) ifelse(is.nan(x), 0, x), how = "replace")) # correct NANs
  CF60 <-   unlist(rapply(CF60_l, f = function(x) ifelse(is.nan(x), 0, x), how = "replace"))
  xGF60 <-  unlist(rapply(xGF60_l, f = function(x) ifelse(is.nan(x), 0, x), how = "replace"))
  
  # Remove Infs
  length_l <- list(length)
  GF60_l <-   list(GF60)
  CF60_l <-   list(CF60)
  xGF60_l <-  list(xGF60)
  
  length <- unlist(rapply(length_l, f = function(x) ifelse(x == 0, 1, x), how = "replace"))
  GF60 <-   unlist(rapply(GF60_l, f = function(x) ifelse(is.infinite(x), 0, x), how = "replace"))
  CF60 <-   unlist(rapply(CF60_l, f = function(x) ifelse(is.infinite(x), 0, x), how = "replace"))
  xGF60 <-  unlist(rapply(xGF60_l, f = function(x) ifelse(is.infinite(x), 0, x), how = "replace"))
  
  rm(length_l, GF60_l, CF60_l, xGF60_l)
  
  
  ## sparse matrix column names:
  # [1]  "n"                   "game_strength_state" "home_on_1"           "home_on_2"           "home_on_3"           "home_on_4"           "home_on_5"          
  # [8]  "home_on_6"           "away_on_1"           "away_on_2"           "away_on_3"           "away_on_4"           "away_on_5"           "away_on_6"          
  # [15] "home_goalie"         "away_goalie"         "off_team"            "def_team"            "length"              "GF60"                "CF60"               
  # [22] "xGF60"               "off_zonestart"       "def_zonestart"       "btb"                 "score_down_3"        "score_down_2"        "score_down_1"       
  # [29] "score_up_1"          "score_up_2"          "score_up_3"          "score_even"          "score_trail"         "score_lead"          "state_5v5"          
  # [36] "state_4v4"           "state_3v3"           "is_home"
  
  
  # Base design matrices - GF / xG
  APM_g <- APM[, -c(1:22, 26:32, 35)] # *** to be implemented... 3-level score state, corrected dummies
  
  # Base design matrix - CF
  APM_c <- APM[, -c(1:22, 32:34, 35)] # *** to be implemented... 7-level score state, corrected dummies
  
  
  # Find goalie names
  goalie_names <- names_match %>% 
    filter(position == 3) %>% 
    mutate(ID = paste0(ID, ".d")) %>% 
    select(ID)
  goalie_names <- as.vector(goalie_names$ID)
  
  # Filter out goalies - xG
  no_goalie_names <- as.vector(colnames(APM_g))
  no_goalie_names <- no_goalie_names[!no_goalie_names %in% goalie_names]
  APM_s <- APM_g[, no_goalie_names]
  
  # Filter out goalies - CF
  no_goalie_names <- as.vector(colnames(APM_c))
  no_goalie_names <- no_goalie_names[!no_goalie_names %in% goalie_names]
  APM_c <- APM_c[, no_goalie_names]
  
  rm(APM)
  gc()
  
  
  ## ---------------------------- ##
  ##   Cross Validation - Goals   ##
  ## ---------------------------- ##
  
  print(" -- CV, Goals", quote = F)
  
  registerDoMC(cores = 2)
  
  CV_results_GF <- cv.glmnet(x = APM_g, 
                             y = GF60, 
                             weights = length, 
                             alpha = 0, 
                             nfolds = 10, 
                             standardize = FALSE, 
                             parallel = TRUE)
  gc()
  
  
  ## ---------------------------- ##
  ##   Cross Validation -   xG    ##
  ## ---------------------------- ##
  
  print(" -- CV, Expected Goals", quote = F)
  
  CV_results_xG <- cv.glmnet(x = APM_s, 
                             y = xGF60, 
                             weights = length, 
                             alpha = 0, 
                             nfolds = 10, 
                             standardize = FALSE, 
                             parallel = TRUE)
  gc()
  
  
  ## ---------------------------- ##
  ##   Cross Validation - Corsi   ##
  ## ---------------------------- ##
  
  print(" -- CV, Corsi", quote = F)
  
  CV_results_CF <- cv.glmnet(x = APM_c, 
                             y = CF60, 
                             weights = length, 
                             alpha = 0, 
                             nfolds = 10, 
                             standardize = FALSE, 
                             parallel = TRUE)
  gc()
  
  
  
  ## ---------------------------- ##
  ##            OUTPUTS           ##
  ## ---------------------------- ##
  
  print("Output Objects", quote = F)
  
  
  ## ------------------ ##
  ##     Goals Join     ##
  ## ------------------ ##
  
  fun.APM_bind <- function(model_data, names_data) {
    
    # Retrieve Coefficients
    APM_test <- data.frame(as.matrix(coef(model_data, s = model_data$lambda.min))) %>% 
      rownames_to_column(var = "APM_names")
    
    
    # Remove .d / .o suffixes
    APM_test_d <- APM_test %>% 
      filter(grepl(".d", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".d", "", APM_names)) %>% 
      rename(Def_GF = X1)
    
    APM_test_o <- APM_test %>% 
      filter(grepl(".o", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".o", "", APM_names)) %>% 
      rename(Off_GF = X1)
    
    # Join
    APM_all <- APM_test_d %>% 
      left_join(., APM_test_o, by = "APM_names") %>% 
      mutate(GPM = Off_GF - Def_GF) %>% 
      select(APM_names, Off_GF, Def_GF, GPM)
    
    APM_all$APM_names <- names_data$player[match(APM_all$APM_names, names_data$ID)]
    
    APM_list <- list(APM_all =  APM_all, 
                     APM_coef = APM_test)
    
    return(APM_list)
  
    }
  APM_list_GF <- fun.APM_bind(model_data =   CV_results_GF, 
                              names_data =   names_match)
  
  APM_initial_GF <-  APM_list_GF$APM_all
  APM_raw_coef_GF <- APM_list_GF$APM_coef
  
  
  # Add TOI and combine
  APM_initial_GF <- APM_initial_GF %>% 
    rename(player = APM_names) %>% 
    left_join(., names_match, by = c("player"))
  
  APM_join_GF <- games_data %>% 
    filter(TOI > 0) %>% 
    group_by(player) %>% 
    mutate(n = n()) %>% 
    summarise(TOI = sum(TOI), 
              Games = first(n)
              ) %>% 
    mutate(TOI.GP = round(TOI / Games, 2)) %>% 
    left_join(APM_initial_GF, ., by = "player") %>% 
    left_join(., pbp_goalie, by = c("player", "qual")) %>% 
    data.frame()
  
  
  # GF RAPM table
  APM_GF <- APM_join_GF %>% 
    filter(!is.na(player)) %>% 
    mutate(TOI = ifelse(position == 3, TOI_goalie, TOI), 
           O_impact_GF = Off_GF * (TOI / 60), 
           D_impact_GF = ifelse(position != 3, Def_GF * (TOI / 60), Def_GF * (TOI_goalie / 60)), 
           GF_impact = O_impact_GF - D_impact_GF, 
           Off_GF = ifelse(Off_GF != 0 & position == 3, 0, Off_GF), 
           GPM = ifelse(player == "MARC-ANDRE.FLEURY", Off_GF - Def_GF, GPM)
           ) %>% 
    select(player, position, TOI, TOI.GP, 
           Off_GF:GPM, O_impact_GF:GF_impact
           ) %>% 
    mutate_at(vars(Off_GF:GPM), funs(round(., 3))) %>% 
    mutate_at(vars(TOI, O_impact_GF:GF_impact), funs(round(., 2))) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
  
  
  
  ## ------------------- ##
  ##       xG Join       ##
  ## ------------------- ##
  
  # Pull out coefficients and match names
  fun.APM_bind <- function(model_data, names_data) {
    
    # Retrieve Coefficients
    APM_test <- data.frame(as.matrix(coef(model_data, s = model_data$lambda.min))) %>% 
      rownames_to_column(var = "APM_names")
    
    
    # Remove .d / .o suffixes
    APM_test_d <- APM_test %>% 
      filter(grepl(".d", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".d", "", APM_names)) %>% 
      rename(Def_xG = X1)
    
    APM_test_o <- APM_test %>% 
      filter(grepl(".o", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".o", "", APM_names)) %>% 
      rename(Off_xG = X1)
    
    # Join
    APM_all <- APM_test_d %>% 
      left_join(., APM_test_o, by = "APM_names") %>% 
      mutate(xGPM = Off_xG - Def_xG) %>% 
      select(APM_names, Off_xG, Def_xG, xGPM)
    
    APM_all$APM_names <- names_data$player[match(APM_all$APM_names, names_data$ID)]
    
    APM_list <- list(APM_all = APM_all, 
                     APM_coef = APM_test)
    
    return(APM_list)
  
    }
  APM_list_xG <- fun.APM_bind(model_data =   CV_results_xG, 
                              names_data =   names_match)
  
  APM_initial_xG <-  APM_list_xG$APM_all
  APM_raw_coef_xG <- APM_list_xG$APM_coef
  
  
  # Add TOI and clean up
  APM_initial_xG <- APM_initial_xG %>% 
    rename(player = APM_names) %>% 
    left_join(., names_match, by = "player")
  
  APM_join_xG <- games_data %>% 
    filter(TOI > 0) %>% 
    group_by(player) %>% 
    mutate(n = n()) %>% 
    summarise(TOI = sum(TOI), 
              Games = first(n)
              ) %>% 
    mutate(TOI.GP = round(TOI / Games, 2)) %>% 
    left_join(APM_initial_xG, ., by = "player") %>% 
    data.frame()
  
  
  # xG RAPM table
  APM_xG <- APM_join_xG %>% 
    filter(!is.na(player)) %>% 
    mutate(O_impact_xG =  Off_xG * (TOI / 60), 
           D_impact_xG =  Def_xG * (TOI / 60), 
           xG_impact = O_impact_xG - D_impact_xG
           ) %>% 
    select(player, position, TOI, TOI.GP, 
           Off_xG:xGPM, O_impact_xG:xG_impact
           ) %>% 
    mutate_at(vars(Off_xG:xGPM), funs(round(., 3))) %>% 
    mutate_at(vars(TOI, O_impact_xG:xG_impact), funs(round(., 2)))
  
  
  
  ## -------------------- ##
  ##      Corsi Join      ##
  ## -------------------- ##
  
  # Pull out coefficients and match names
  fun.APM_bind <- function(model_data, names_data) {
    
    # Retrieve Coefficients
    APM_test <- data.frame(as.matrix(coef(model_data, s = model_data$lambda.min))) %>% 
      rownames_to_column(var = "APM_names")
    
    
    # Remove .d / .o suffixes
    APM_test_d <- APM_test %>% 
      filter(grepl(".d", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".d", "", APM_names)) %>% 
      rename(Def_CF = X1)
    
    APM_test_o <- APM_test %>% 
      filter(grepl(".o", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".o", "", APM_names)) %>% 
      rename(Off_CF = X1)
    
    # Join
    APM_all <- APM_test_d %>% 
      left_join(., APM_test_o, by = "APM_names") %>% 
      mutate(CPM = Off_CF - Def_CF) %>% 
      select(APM_names, Off_CF, Def_CF, CPM)
    
    APM_all$APM_names <- names_data$player[match(APM_all$APM_names, names_data$ID)]
    
    APM_list <- list(APM_all = APM_all, 
                     APM_coef = APM_test)
    
    return(APM_list)
  
    }
  APM_list_CF <- fun.APM_bind(model_data =   CV_results_CF, 
                              names_data =   names_match)
  
  APM_initial_CF <-  APM_list_CF$APM_all
  APM_raw_coef_CF <- APM_list_CF$APM_coef
  
  
  # Add TOI and clean up
  APM_initial_CF <- APM_initial_CF %>% 
    rename(player = APM_names) %>% 
    left_join(., names_match, by = "player")
  
  APM_join_CF <- games_data %>% 
    filter(TOI > 0) %>% 
    group_by(player) %>% 
    mutate(n = n()) %>% 
    summarise(TOI = sum(TOI), 
              Games = first(n)
              ) %>% 
    mutate(TOI.GP = round(TOI / Games, 2)) %>% 
    left_join(APM_initial_CF, ., by = "player") %>% 
    data.frame()
  
  
  # CF RAPM table
  APM_CF <- APM_join_CF %>% 
    filter(!is.na(player)) %>% 
    mutate(O_impact_CF =  Off_CF * (TOI / 60), 
           D_impact_CF =  Def_CF * (TOI / 60), 
           CF_impact = O_impact_CF - D_impact_CF
           ) %>% 
    select(player, position, TOI, TOI.GP, 
           Off_CF:CPM, O_impact_CF:CF_impact
           ) %>% 
    mutate_at(vars(Off_CF:CPM), funs(round(., 3))) %>% 
    mutate_at(vars(TOI, O_impact_CF:CF_impact), funs(round(., 2)))
  
  
  
  ## ------------------------------------ ##
  ##   Join GF, xGF, and CF RAPM Tables   ##
  ## ------------------------------------ ##
  
  print("Combine / Join", quote = F)
  
  teams <- games_data %>% 
    group_by(player, Team) %>% 
    summarise(TOI = sum(TOI)) %>% 
    mutate(Team = paste0(Team, collapse = "/")) %>% 
    group_by(player, Team) %>% 
    summarise(TOI = sum(TOI)) %>% 
    select(player, Team) %>% 
    data.frame()
  
  goalie_teams <- rbind(
    pbp_data %>% 
      group_by(home_goalie, home_team) %>% 
      summarise() %>% 
      filter(!is.na(home_goalie)) %>% 
      rename(player = home_goalie, 
             Team = home_team) %>% 
      data.frame(), 
    pbp_data %>% 
      group_by(away_goalie, away_team) %>% 
      summarise() %>% 
      filter(!is.na(away_goalie)) %>% 
      rename(player = away_goalie, 
             Team = away_team) %>% 
      data.frame()
    ) %>% 
    group_by(player, Team) %>% 
    summarise() %>% 
    mutate(Team = paste0(Team, collapse = "/")) %>% 
    group_by(player, Team) %>% 
    summarise() %>% 
    data.frame()
  
  
  APM_EV_join <- APM_GF %>% 
    filter(position != 3) %>% 
    left_join(., teams, by = "player") %>% 
    left_join(., APM_xG, by = c("player", "position", "TOI", "TOI.GP")) %>% 
    left_join(., APM_CF, by = c("player", "position", "TOI", "TOI.GP")) %>% 
    mutate(season = unique(games_data$season)) %>% 
    select(player, position, season, Team, TOI,  
           Off_GF:GPM, Off_xG:xGPM, Off_CF:CPM, 
           O_impact_GF, D_impact_GF, GF_impact, 
           O_impact_xG, D_impact_xG, xG_impact, 
           O_impact_CF, D_impact_CF, CF_impact)
  
  APM_EV_join_rates <- APM_EV_join %>% 
    select(player:TOI, Off_GF:GPM, Off_xG:xGPM, Off_CF:CPM)
  
  APM_EV_join_impact <- APM_EV_join %>% 
    select(player:TOI, O_impact_GF, D_impact_GF, GF_impact, 
           O_impact_xG, D_impact_xG, xG_impact, 
           O_impact_CF, D_impact_CF, CF_impact)
  
  APM_EV_goalies <- APM_GF %>% 
    filter(position == 3) %>% 
    left_join(., goalie_teams, by = "player") %>% 
    mutate(season = unique(games_data$season)) %>% 
    select(player, position, season, Team, TOI, Def_GF, D_impact_GF)
  
  
  ## ----------------------------- ##
  ##   List of Objects to Return   ##
  ## ----------------------------- ##
  
  return_list <- list(
    
    # Target variables
    target_GF = GF60,  
    target_xG = xGF60, 
    target_CF = CF60, 
    
    # Design matrices
    design_GF = APM_g, 
    design_xG = APM_s, 
    design_CF = APM_c, 
    
    # Weights
    weights = length, 
    
    # cv.glmnet objects
    cv_results_GF = CV_results_GF, 
    cv_results_xG = CV_results_xG, 
    cv_results_CF = CV_results_CF,
    
    # Raw coefficients from cv.glmnet objects
    RAPM_coef_GF = APM_raw_coef_GF, 
    RAPM_coef_xG = APM_raw_coef_xG, 
    RAPM_coef_CF = APM_raw_coef_CF, 
    
    # Player information
    player_base = player_base_, 
    IDs_master =  names_match, 
    
    # Final tables
    RAPM_EV_join =    APM_EV_join, 
    RAPM_EV_rates =   APM_EV_join_rates, 
    RAPM_EV_impact =  APM_EV_join_impact, 
    RAPM_EV_goalies = APM_EV_goalies
    
    )
  
  return(return_list)
  
  }


##########################


## ----------------------- ##
##   Skater RAPM - PP/SH   ## *** Updated 12/16/18 with cv.glmnet change
## ----------------------- ##

#############################

# Single Season Function
fun.RAPM_PP_SH_all <- function(pbp_data, games_data_PP, games_data_SH) { 
  
  ## ------------------- ##
  ##   Qualify Players   ##
  ## ------------------- ##
  
  # TOI Cutoffs (imposed 5 minute cutoff w/o standardization)
  
  F_TOI_cut_PP <- 5
  D_TOI_cut_PP <- 5
  F_TOI_cut_SH <- 5
  D_TOI_cut_SH <- 5
  G_TOI_cut <- 5
  
  
  # Skaters
  qual_skater_PP <- games_data_PP %>% 
    group_by(player, Team) %>% 
    summarise(TOI = sum(TOI)) %>% 
    left_join(., player_position, by = "player") %>% 
    group_by(player, position) %>% 
    mutate(Team = paste0(Team, collapse = "/")) %>% 
    group_by(player, Team, position) %>% 
    summarise(TOI = sum(TOI)) %>% 
    mutate(qual = 1 * ((position == 1 & TOI >= F_TOI_cut_PP) | (position == 2 & TOI >= D_TOI_cut_PP))) %>% 
    filter(qual == 1) %>% 
    data.frame()
  
  qual_skater_SH <- games_data_SH %>% 
    group_by(player, Team) %>% 
    summarise(TOI = sum(TOI)) %>% 
    left_join(., player_position, by = "player") %>% 
    group_by(player, position) %>% 
    mutate(Team = paste0(Team, collapse = "/")) %>% 
    group_by(player, Team, position) %>% 
    summarise(TOI = sum(TOI)) %>% 
    mutate(qual = 1 * ((position == 1 & TOI >= F_TOI_cut_SH) | (position == 2 & TOI >= D_TOI_cut_SH))) %>% 
    filter(qual == 1) %>% 
    data.frame()
  
  # Goalies
  pbp_goalie_H <- pbp_data %>% 
    filter(game_strength_state %in% st.pp_strength) %>% 
    group_by(home_goalie, season) %>% 
    summarise(TOI = sum(event_length)) %>% 
    filter(!is.na(TOI) & home_goalie != 50) %>% 
    rename(player = home_goalie) %>% 
    data.frame()
  
  pbp_goalie_A <- pbp_data %>% 
    filter(game_strength_state %in% st.pp_strength) %>% 
    group_by(away_goalie, season) %>% 
    summarise(TOI = sum(event_length)) %>% 
    filter(!is.na(TOI) & away_goalie != 50) %>% 
    rename(player = away_goalie) %>% 
    data.frame()
  
  pbp_goalie <- pbp_goalie_A %>% 
    rbind(., pbp_goalie_H) %>% 
    group_by(player) %>% 
    summarise(TOI = sum(TOI / 60)) %>% 
    mutate(qual = 1 * (TOI >= G_TOI_cut)) %>% 
    filter(qual == 1) %>% 
    data.frame()
  
  # TOI cutoffs
  F_TOI_cut_PP <- min(filter(qual_skater_PP, position == 1)$TOI)
  D_TOI_cut_PP <- min(filter(qual_skater_PP, position == 2)$TOI)
  
  F_TOI_cut_SH <- min(filter(qual_skater_SH, position == 1)$TOI)
  D_TOI_cut_SH <- min(filter(qual_skater_SH, position == 2)$TOI)
  
  G_TOI_cut <- as.numeric(min(pbp_goalie[, "TOI"]))
  
  rm(pbp_goalie_A, pbp_goalie_H)
  
  
  ## ---------------------------- ##
  ##   RAPM Design Matrix Setup   ##
  ## ---------------------------- ##
  
  # Goalie Qualifying
  fun.goalie_qual <- function(data, cutoff) {
    
    data <- filter(data, game_strength_state %in% st.pp_strength)
    
    pbp_goalie_H <- data %>% 
      group_by(home_goalie, season) %>% 
      summarise(TOI = sum(event_length)) %>% 
      filter(!is.na(TOI) & home_goalie != 50) %>% 
      rename(player = home_goalie) %>% 
      data.frame()
    
    pbp_goalie_A <- data %>% 
      group_by(away_goalie, season) %>% 
      summarise(TOI = sum(event_length)) %>% 
      filter(!is.na(TOI) & away_goalie != 50) %>% 
      rename(player = away_goalie) %>% 
      data.frame()
    
    pbp_goalie <- pbp_goalie_A %>% 
      rbind(., pbp_goalie_H) %>% 
      group_by(player) %>% 
      summarise(TOI = sum(TOI) / 60) %>% 
      mutate(qual = ifelse(TOI >= cutoff, 1, 0)) %>% 
      #select(player, qual) %>% 
      data.frame()
    
    }
  goalie_qual <- fun.goalie_qual(data =   pbp_data, 
                                 cutoff = G_TOI_cut)
  
  # Find qualified players for Powerplay - USE FOR GOALS REGRESSION
  fun.qualified_GF_PP <- function(data, f_cut, d_cut) {
    
    Qualified <- data %>% 
      group_by(player) %>% 
      summarise(TOI = sum(TOI)) %>% 
      left_join(., player_position, by = "player") %>% 
      mutate(qual = ifelse(TOI >= f_cut & position == 1, 1, 
                           ifelse(TOI >= d_cut & position == 2, 1, 0))
             ) %>% 
      select(player, TOI, qual)
    
    return(Qualified)
  
    }
  Qualified_GF_PP <- fun.qualified_GF_PP(data =  games_data_PP, 
                                         f_cut = F_TOI_cut_PP, 
                                         d_cut = D_TOI_cut_PP)
  
  # Find qualified players for Shorthanded; goalies in here - USE FOR GOALS REGRESSION
  fun.qualified_GF_SH <- function(data, f_cut, d_cut) {
    
    Qualified <- data %>% 
      group_by(player) %>% 
      summarise(TOI = sum(TOI)) %>% 
      left_join(., player_position, by = "player") %>% 
      mutate(qual = ifelse(TOI >= f_cut & position == 1, 1, 
                           ifelse(TOI >= d_cut & position == 2, 1, 0))
             ) %>% 
      select(player, TOI, qual)
    
    Qualified <- rbind(Qualified, goalie_qual)
    
    return(Qualified)
  
    }
  Qualified_GF_SH <- fun.qualified_GF_SH(data =  games_data_SH, 
                                         f_cut = F_TOI_cut_SH, 
                                         d_cut = D_TOI_cut_SH)
  
  
  # Prepare pbp (initial) - filter to PP strength + column select
  fun.pbp_prepare <- function(data) {
    
    pbp_part <- data %>% 
      filter(event_type %in% c("GOAL", "SHOT", "MISS", "BLOCK", "ON", "TAKE", "GIVE", "HIT", "FAC"), 
             game_strength_state %in% st.pp_strength, 
             game_period < 5
             ) %>% 
      mutate(scradj = home_score - away_score, 
             home_lead = ifelse(scradj >= 3, 3, 
                                ifelse(scradj <= -3, -3, scradj)), 
             event_length = ifelse(is.na(event_length), 0, event_length), 
             event_team = ifelse(is.na(event_team), 0, event_team), 
             home_zonestart = ifelse(is.na(home_zonestart), 0, home_zonestart)
             ) %>% 
      rename(pred_goal = pred_XGB_7) %>% 
      select(game_id, event_index, 
             home_on_1:away_on_6, 
             home_goalie, away_goalie, 
             home_team, away_team, 
             event_length, 
             event_team, 
             event_type, 
             game_strength_state, 
             home_lead, 
             home_zonestart, 
             pred_goal) %>% 
      left_join(., btb, by = c("game_id")) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
    
    return(pbp_part)
    
    }
  pbp_part <- fun.pbp_prepare(data = pbp_data)
  
  
  # Create IDs for Powerplay Names Match
  fun.names_match_PP <- function(sub_data2, qual_data, player_data) {
    
    # Skaters
    skaters <- mutate(player_data, ID = row_number() + 10000)
    
    
    # Goalies
    fun.goalie_find <- function(data) {
      
      goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data$home_goalie, data$away_goalie))))))
      
      goalie_return$player <- as.character(goalie_return$player)
      
      return(goalie_return)
    
      }
    goalies <- fun.goalie_find(sub_data2)
    
    goalies <- goalies %>% 
      mutate(position = NA, 
             ID = NA)
    
    goalies$position <- 3
    
    goalies <- goalies %>% 
      arrange(player) %>% 
      mutate(ID = row_number() + 20000)
    
    
    # Teams
    teams_str <- unique(na.omit(sub_data2$event_team))
    teams <- data.frame(matrix(nrow = length(teams_str), ncol = 3))
    names(teams) <- c("player", "position", "ID")
    teams$player <- teams_str
    teams$position <- 4
    
    teams <- teams %>% 
      arrange(player) %>% 
      mutate(ID = row_number())
    
    
    # Event Type
    event_str <- unique(na.omit(sub_data2$event_type))
    event <- data.frame(matrix(nrow = length(event_str), ncol = 3))
    names(event) <- c("player", "position", "ID")
    event$player <- event_str
    event$position <- 5
    
    event <- event %>% 
      arrange(player) %>% 
      mutate(ID = row_number() + 100)
    
    
    # Strength State
    st.strength <- unique(na.omit(sub_data2$game_strength_state))
    strength <- data.frame(matrix(nrow = length(st.strength), ncol = 3))
    names(strength) <- c("player", "position", "ID")
    strength$player <- st.strength
    strength$position <- 6
    
    strength <- strength %>% 
      arrange(player) %>% 
      mutate(ID = row_number() + 200)
    
    
    # Combine
    all <- teams %>% 
      rbind(., skaters, goalies, event, strength) %>% 
      filter(player != 0) %>% 
      left_join(., select(qual_data, player, qual), by = "player") %>% 
      mutate(qual = ifelse(is.na(qual), 0, qual)) %>% 
      arrange(ID)
    
    return(all)
  
    }
  names_match_PP <- fun.names_match_PP(sub_data2 =   pbp_part, 
                                       qual_data =   Qualified_GF_PP,  
                                       player_data = player_position %>% filter(position != 3))
  
  # Create IDs for Shorthanded Names Match
  fun.names_match_SH <- function(sub_data2, qual_data, player_data) {
    
    # Skaters
    skaters <- mutate(player_data, ID = row_number() + 10000)
    
    
    # Goalies
    fun.goalie_find <- function(data) {
      
      goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data$home_goalie, data$away_goalie))))))
      
      goalie_return$player <- as.character(goalie_return$player)
      
      return(goalie_return)
    
      }
    goalies <- fun.goalie_find(sub_data2)
    
    goalies <- goalies %>% 
      mutate(position = NA, 
             ID = NA)
    
    goalies$position <- 3
    
    goalies <- goalies %>% 
      arrange(player) %>% 
      mutate(ID = row_number() + 20000)
    
    
    # Teams
    teams_str <- unique(na.omit(sub_data2$event_team))
    teams <- data.frame(matrix(nrow = length(teams_str), ncol = 3))
    names(teams) <- c("player", "position", "ID")
    teams$player <- teams_str
    teams$position <- 4
    
    teams <- teams %>% 
      arrange(player) %>% 
      mutate(ID = row_number())
    
    
    # Event Type
    event_str <- unique(na.omit(sub_data2$event_type))
    event <- data.frame(matrix(nrow = length(event_str), ncol = 3))
    names(event) <- c("player", "position", "ID")
    event$player <- event_str
    event$position <- 5
    
    event <- event %>% 
      arrange(player) %>% 
      mutate(ID = row_number() + 100)
    
    
    # Strength State
    st.strength <- unique(na.omit(sub_data2$game_strength_state))
    strength <- data.frame(matrix(nrow = length(st.strength), ncol = 3))
    names(strength) <- c("player", "position", "ID")
    strength$player <- st.strength
    strength$position <- 6
    
    strength <- strength %>% 
      arrange(player) %>% 
      mutate(ID = row_number() + 200)
    
    
    # Combine
    all <- teams %>% 
      rbind(., skaters, goalies, event, strength) %>% 
      filter(player != 0) %>% 
      left_join(., select(qual_data, player, qual), by = "player") %>% 
      mutate(qual = ifelse(is.na(qual), 0, qual)) %>% 
      arrange(ID)
    
    return(all)
  
    }
  names_match_SH <- fun.names_match_SH(sub_data2 =   pbp_part, 
                                       qual_data =   Qualified_GF_SH, 
                                       player_data = player_position %>% filter(position != 3))
  
  
  # Determine non-qualified players for Powerplay
  exclude_PP <- names_match_PP %>% 
    filter(ID > 10000, qual == 0) %>% 
    select(ID)
  
  exclude_PP <- as.vector(exclude_PP[, 1])
  
  # Determine non-qualified players for Shorthanded
  exclude_SH <- names_match_SH %>% 
    filter(ID > 10000, qual == 0) %>% 
    select(ID)
  
  exclude_SH <- as.vector(exclude_SH[, 1])
  
  # Identify goalies to add to PPD columns (PP/SH RAPM specific)
  qual_goalies <- names_match_SH %>%
    filter(position == 3, qual == 1) %>% 
    select(ID)
  
  qual_goalies <- as.vector(qual_goalies[, 1])
  
  
  # Identify and save specific event type IDs for dummy function/creation below
  st.corsi_events <-   names_match_PP[which(names_match_PP[, 1] %in% c("SHOT", "GOAL", "MISS", "BLOCK")), 3]
  st.fenwick_events <- names_match_PP[which(names_match_PP[, 1] %in% c("SHOT", "GOAL", "MISS")), 3]
  st.goal_ID <-        names_match_PP[which(names_match_PP[, 1] %in% c("GOAL")), 3] 
  st.fac_ID <-         names_match_PP[which(names_match_PP[, 1] %in% c("FAC")), 3]
  st.pp_home <-        names_match_PP[which(names_match_PP[, 1] %in% c("5v4", "5v3", "4v3")), 3]
  st.pp_away <-        names_match_PP[which(names_match_PP[, 1] %in% c("4v5", "3v5", "3v4")), 3]
  st.on_ID <-          names_match_PP[which(names_match_PP[, 1] %in% c("ON")), 3]
  st.5v4 <-            names_match_PP[which(names_match_PP[, 1] %in% c("5v4", "4v5")), 3]
  st.5v3 <-            names_match_PP[which(names_match_PP[, 1] %in% c("5v3", "3v5")), 3]
  st.4v3 <-            names_match_PP[which(names_match_PP[, 1] %in% c("4v3", "3v4")), 3]
  
  
  # Convert prepared pbp data frame to all numeric values
  fun.IDs <- function(data, names_data) {
    
    data$game_id <- as.numeric(data$game_id)
    
    # Home skaters
    data$home_on_1 <- names_data$ID[match(data$home_on_1, names_data$player)]
    data$home_on_2 <- names_data$ID[match(data$home_on_2, names_data$player)]
    data$home_on_3 <- names_data$ID[match(data$home_on_3, names_data$player)]
    data$home_on_4 <- names_data$ID[match(data$home_on_4, names_data$player)]
    data$home_on_5 <- names_data$ID[match(data$home_on_5, names_data$player)]
    data$home_on_6 <- names_data$ID[match(data$home_on_6, names_data$player)]
    
    # Away Skaters
    data$away_on_1 <- names_data$ID[match(data$away_on_1, names_data$player)]
    data$away_on_2 <- names_data$ID[match(data$away_on_2, names_data$player)]
    data$away_on_3 <- names_data$ID[match(data$away_on_3, names_data$player)]
    data$away_on_4 <- names_data$ID[match(data$away_on_4, names_data$player)]
    data$away_on_5 <- names_data$ID[match(data$away_on_5, names_data$player)]
    data$away_on_6 <- names_data$ID[match(data$away_on_6, names_data$player)]
    
    # Goalies
    data$home_goalie <- names_data$ID[match(data$home_goalie, names_data$player)]
    data$away_goalie <- names_data$ID[match(data$away_goalie, names_data$player)]
    
    # Teams
    data$event_team <- names_data$ID[match(data$event_team, names_data$player)]
    data$home_team <- names_data$ID[match(data$home_team, names_data$player)]
    data$away_team <- names_data$ID[match(data$away_team, names_data$player)]
    
    # Events / strength states
    data$event_type <- names_data$ID[match(data$event_type, names_data$player)]
    data$game_strength_state <- names_data$ID[match(data$game_strength_state, names_data$player)]
    
    # Make Empty Slots 0s
    data[is.na(data)] <- 0
    
    return(data)
  }
  pbp_part <- fun.IDs(data =       pbp_part, 
                      names_data = names_match_PP)
  
  gc()
  
  
  ## -------------------------------- ##
  ##    Create RAPM Design Matrix     ##
  ## -------------------------------- ##
  
  # GF60: PP/SH Vectorized
  fun.APM_PP_sparse_GF <- function(data) {
    
    ## ------------------------------ ##
    ##       Setup column names       ##
    ## ------------------------------ ##
    
    ## Make Columns Names
    ##############################################
    
    # Get all player names
    print("get_names")
    
    df <- as.matrix(data)
    hold <- unique(as.vector(df[, grep("_on_", colnames(df))]))
    hold <- as.numeric(hold)
    
    rm(df)
    gc()
    
    # Remove Missing Slots
    hold <- hold[!hold %in% 0]
    
    # Qualify Powerplay Offense and remove goalies
    groups_PPO <- hold[!hold %in% exclude_PP]
    groups_PPO <- groups_PPO[!groups_PPO %in% c(20000:30000)]
    
    # Qualify Shorthanded Defense
    groups_SHD <- hold[!hold %in% exclude_SH]
    
    # Qualify Shorthanded Offense and remove goalies
    groups_SHO <- hold[!hold %in% exclude_SH]
    groups_SHO <- groups_SHO[!groups_SHO %in% c(20000:29999)]
    
    # Qualify Powerplay Defense
    #groups_PPD <- hold[!hold %in% exclude_PP]
    groups_PPD <- c(hold[!hold %in% exclude_PP], qual_goalies) # add in qualified goalies
    
    # Order Smallest to Largest
    groups_PPO <- sort(groups_PPO, decreasing = FALSE)
    groups_SHD <- sort(groups_SHD, decreasing = FALSE)
    groups_SHO <- sort(groups_SHO, decreasing = FALSE)
    groups_PPD <- sort(groups_PPD, decreasing = FALSE)
    
    
    ##############################################
    
    
    
    ## ------------------------------ ##
    ##        Construct Tables        ##
    ## ------------------------------ ##
    
    ## Home Table 1 (Home Offense in Home PP Strengths)
    ##############################################
    
    print("home_df_1")
    
    test.H1 <- data %>% 
      filter(game_strength_state %in% st.pp_home) %>% 
      arrange(game_id, event_index) %>% 
      mutate(shift_ID = cumsum(event_type == st.on_ID), 
             off_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 3), 
             def_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 1), 
             off_lead = home_lead
             ) %>% 
      group_by(game_id, 
               shift_ID, 
               game_strength_state, 
               home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6,
               away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, 
               off_lead
               ) %>% 
      summarise(home_goalie = first(home_goalie), 
                away_goalie = first(away_goalie), 
                off_team = first(home_team), 
                def_team = first(away_team), 
                length = sum(event_length),
                GF60 = (sum(event_type == st.goal_ID & event_team == home_team) / sum(event_length)) * 3600, 
                CF60 =  (sum(event_type %in% st.corsi_events & event_team == home_team) / sum(event_length)) * 3600, 
                xGF60 = (sum((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal) / sum(event_length)) * 3600, 
                off_zonestart = sum(off_zonestart), 
                def_zonestart = sum(def_zonestart), 
                btb = first(home_btb)
                ) %>% 
      filter(length > 0) %>% 
      ungroup() %>% 
      add_column(., n = 0, .before = 1) %>% 
      mutate(n = as.numeric(row_number()), 
             score_down_3 = 1 * (off_lead <= -3), 
             score_down_2 = 1 * (off_lead == -2), 
             score_down_1 = 1 * (off_lead == -1), 
             score_up_1 =   1 * (off_lead ==  1), 
             score_up_2 =   1 * (off_lead ==  2), 
             score_up_3 =   1 * (off_lead >=  3), 
             score_even =   1 * (off_lead ==  0), 
             score_trail =  1 * (off_lead < 0), 
             score_lead =   1 * (off_lead > 0), 
             off_zonestart = 1 * (off_zonestart >= 1), 
             def_zonestart = 1 * (def_zonestart >= 1), 
             state_5v4 = 1 * (game_strength_state %in% st.5v4),
             state_5v3 = 1 * (game_strength_state %in% st.5v3),
             state_4v3 = 1 * (game_strength_state %in% st.4v3), 
             is_home = 1
             ) %>% 
      select(-c(game_id, off_lead, shift_ID)) %>% 
      data.matrix()
    
    
    ## ----------------------- ##
    ##     Home PP Offense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_PPO, function(x, test.H1)  which(test.H1[, "home_on_1"] == x | 
                                                          test.H1[, "home_on_2"] == x |
                                                          test.H1[, "home_on_3"] == x |
                                                          test.H1[, "home_on_4"] == x |
                                                          test.H1[, "home_on_5"] == x |
                                                          test.H1[, "home_on_6"] == x), test.H1 = test.H1)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_PPO <- sparseMatrix(i, j, dims = c(nrow(test.H1), length(groups_PPO)))
    
    # Rename
    colnames(dummies_PPO) <- groups_PPO
    colnames(dummies_PPO) <- paste(colnames(dummies_PPO), ".PPO", sep = "")
    
    
    ## ----------------------- ##
    ##     Home SH Defense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_SHD, function(x, test.H1)  which((test.H1[, "away_on_1"] == x | 
                                                           test.H1[, "away_on_2"] == x |
                                                           test.H1[, "away_on_3"] == x |
                                                           test.H1[, "away_on_4"] == x |
                                                           test.H1[, "away_on_5"] == x |
                                                           test.H1[, "away_on_6"] == x) & test.H1[, "home_goalie"] != x), test.H1 = test.H1)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_SHD <- sparseMatrix(i, j, dims = c(nrow(test.H1), length(groups_SHD)))
    
    # Rename
    colnames(dummies_SHD) <- groups_SHD
    colnames(dummies_SHD) <- paste(colnames(dummies_SHD), ".SHD", sep = "")
    
    
    ## ----------------------- ##
    ##     Home SH Offense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_SHO, function(x, test.H1)  which(99999 == x | 99999 == x | 99999 == x | 99999 == x | 99999 == x | 99999 == x), test.H1 = test.H1)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_SHO <- sparseMatrix(i, j, dims = c(nrow(test.H1), length(groups_SHO)))
    
    # Rename
    colnames(dummies_SHO) <- groups_SHO
    colnames(dummies_SHO) <- paste(colnames(dummies_SHO), ".SHO", sep = "")
    
    
    ## ----------------------- ##
    ##     Home PP Defense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_PPD, function(x, test.H1)  which(99999 == x |  99999 == x | 99999 == x | 99999 == x | 99999 == x | 99999 == x), test.H1 = test.H1)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_PPD <- sparseMatrix(i, j, dims = c(nrow(test.H1), length(groups_PPD)))
    
    # Rename
    colnames(dummies_PPD) <- groups_PPD
    colnames(dummies_PPD) <- paste(colnames(dummies_PPD), ".PPD", sep = "")
    
    
    ## ----------------------- ##
    ##          Combine        ##
    ## ----------------------- ##
    test_sparse.H1 <- cbind(test.H1, dummies_PPO, dummies_SHD, dummies_SHO, dummies_PPD)
    
    rm(test.H1)
    gc()
    
    ##############################################
    
    
    ## Home Table 2 (Home Offense in Away PP Strengths)
    ##############################################
    
    print("home_df_2")
    
    test.H2 <- data %>% 
      filter(game_strength_state %in% st.pp_away) %>% 
      arrange(game_id, event_index) %>% 
      mutate(shift_ID = cumsum(event_type == st.on_ID), 
             off_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 3), 
             def_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 1), 
             off_lead = home_lead
             ) %>% 
      group_by(game_id, 
               shift_ID, 
               game_strength_state, 
               home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6,
               away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, 
               off_lead
               ) %>% 
      summarise(home_goalie = first(home_goalie), 
                away_goalie = first(away_goalie), 
                off_team = first(home_team), 
                def_team = first(away_team), 
                length = sum(event_length),
                GF60 = (sum(event_type == st.goal_ID & event_team == home_team) / sum(event_length)) * 3600, 
                CF60 =  (sum(event_type %in% st.corsi_events & event_team == home_team) / sum(event_length)) * 3600, 
                xGF60 = (sum((event_type %in% st.fenwick_events & event_team == home_team) * pred_goal) / sum(event_length)) * 3600, 
                off_zonestart = sum(off_zonestart), 
                def_zonestart = sum(def_zonestart), 
                btb = first(home_btb)
                ) %>% 
      filter(length > 0) %>% 
      ungroup() %>% 
      add_column(., n = 0, .before = 1) %>% 
      mutate(n = as.numeric(row_number()), 
             score_down_3 = 1 * (off_lead <= -3), 
             score_down_2 = 1 * (off_lead == -2), 
             score_down_1 = 1 * (off_lead == -1), 
             score_up_1 =   1 * (off_lead ==  1), 
             score_up_2 =   1 * (off_lead ==  2), 
             score_up_3 =   1 * (off_lead >=  3), 
             score_even =   1 * (off_lead ==  0), 
             score_trail =  1 * (off_lead < 0), 
             score_lead =   1 * (off_lead > 0), 
             off_zonestart = 1 * (off_zonestart >= 1), 
             def_zonestart = 1 * (def_zonestart >= 1), 
             state_5v4 = 1 * (game_strength_state %in% st.5v4),
             state_5v3 = 1 * (game_strength_state %in% st.5v3),
             state_4v3 = 1 * (game_strength_state %in% st.4v3),
             is_home = 1
             ) %>% 
      select(-c(game_id, off_lead, shift_ID)) %>% 
      data.matrix()
    
    
    ## ----------------------- ##
    ##     Home PP Offense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_PPO, function(x, test.H2)  which(99999 == x | 99999 == x |99999 == x | 99999 == x | 99999 == x | 99999 == x), test.H2 = test.H2)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_PPO <- sparseMatrix(i, j, dims = c(nrow(test.H2), length(groups_PPO)))
    
    # Rename
    colnames(dummies_PPO) <- groups_PPO
    colnames(dummies_PPO) <- paste(colnames(dummies_PPO), ".PPO", sep = "")
    
    
    ## ----------------------- ##
    ##     Home SH Defense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_SHD, function(x, test.H2)  which(99999 == x | 99999 == x | 99999 == x | 99999 == x | 99999 == x | 99999 == x), test.H2 = test.H2)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_SHD <- sparseMatrix(i, j, dims = c(nrow(test.H2), length(groups_SHD)))
    
    # Rename
    colnames(dummies_SHD) <- groups_SHD
    colnames(dummies_SHD) <- paste(colnames(dummies_SHD), ".SHD", sep = "")
    
    
    ## ----------------------- ##
    ##     Home SH Offense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_SHO, function(x, test.H2)  which(test.H2[, "home_on_1"] == x | 
                                                          test.H2[, "home_on_2"] == x |
                                                          test.H2[, "home_on_3"] == x |
                                                          test.H2[, "home_on_4"] == x |
                                                          test.H2[, "home_on_5"] == x |
                                                          test.H2[, "home_on_6"] == x), test.H2 = test.H2)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_SHO <- sparseMatrix(i, j, dims = c(nrow(test.H2), length(groups_SHO)))
    
    # Rename
    colnames(dummies_SHO) <- groups_SHO
    colnames(dummies_SHO) <- paste(colnames(dummies_SHO), ".SHO", sep = "")
    
    
    ## ----------------------- ##
    ##     Home PP Defense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_PPD, function(x, test.H2)  which((test.H2[, "away_on_1"] == x | 
                                                           test.H2[, "away_on_2"] == x |
                                                           test.H2[, "away_on_3"] == x |
                                                           test.H2[, "away_on_4"] == x |
                                                           test.H2[, "away_on_5"] == x |
                                                           test.H2[, "away_on_6"] == x) & test.H2[, "home_goalie"] != x), test.H2 = test.H2)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_PPD <- sparseMatrix(i, j, dims = c(nrow(test.H2), length(groups_PPD)))
    
    # Rename
    colnames(dummies_PPD) <- groups_PPD
    colnames(dummies_PPD) <- paste(colnames(dummies_PPD), ".PPD", sep = "")
    
    
    ## ----------------------- ##
    ##          Combine        ##
    ## ----------------------- ##
    test_sparse.H2 <- cbind(test.H2, dummies_PPO, dummies_SHD, dummies_SHO, dummies_PPD)
    
    rm(test.H2)
    gc()
    
    ##############################################
    
    
    ## Away Table 1 (Away Offense in Away PP Strengths)
    ##############################################
    
    print("away_df_1")
    
    test.A1 <- data %>% 
      filter(game_strength_state %in% st.pp_away) %>% 
      arrange(game_id, event_index) %>% 
      mutate(shift_ID = cumsum(event_type == st.on_ID), 
             off_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 1), 
             def_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 3), 
             off_lead = -1 * home_lead
             ) %>% 
      group_by(game_id, 
               shift_ID, 
               game_strength_state, 
               home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6,
               away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, 
               off_lead
               ) %>% 
      summarise(home_goalie = first(home_goalie), 
                away_goalie = first(away_goalie), 
                off_team = first(away_team), 
                def_team = first(home_team), 
                length = sum(event_length),
                GF60 = (sum(event_type == st.goal_ID & event_team == away_team) / sum(event_length)) * 3600, 
                CF60 =  (sum(event_type %in% st.corsi_events & event_team == away_team) / sum(event_length)) * 3600, 
                xGF60 = (sum((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal) / sum(event_length)) * 3600, 
                off_zonestart = sum(off_zonestart), 
                def_zonestart = sum(def_zonestart), 
                btb = first(away_btb)
                ) %>% 
      filter(length > 0) %>% 
      ungroup() %>% 
      add_column(., n = 0, .before = 1) %>% 
      mutate(n = as.numeric(row_number()), 
             score_down_3 = 1 * (off_lead <= -3), 
             score_down_2 = 1 * (off_lead == -2), 
             score_down_1 = 1 * (off_lead == -1), 
             score_up_1 =   1 * (off_lead ==  1), 
             score_up_2 =   1 * (off_lead ==  2), 
             score_up_3 =   1 * (off_lead >=  3), 
             score_even =   1 * (off_lead ==  0), 
             score_trail =  1 * (off_lead < 0), 
             score_lead =   1 * (off_lead > 0), 
             off_zonestart = 1 * (off_zonestart >= 1), 
             def_zonestart = 1 * (def_zonestart >= 1), 
             state_5v4 = 1 * (game_strength_state %in% st.5v4),
             state_5v3 = 1 * (game_strength_state %in% st.5v3),
             state_4v3 = 1 * (game_strength_state %in% st.4v3),
             is_home = 0
             ) %>% 
      select(-c(game_id, off_lead, shift_ID)) %>% 
      data.matrix()
    
    
    ## ----------------------- ##
    ##     Away PP Offense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_PPO, function(x, test.A1)  which(test.A1[, "away_on_1"] == x | 
                                                          test.A1[, "away_on_2"] == x |
                                                          test.A1[, "away_on_3"] == x |
                                                          test.A1[, "away_on_4"] == x |
                                                          test.A1[, "away_on_5"] == x |
                                                          test.A1[, "away_on_6"] == x), test.A1 = test.A1)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_PPO <- sparseMatrix(i, j, dims = c(nrow(test.A1), length(groups_PPO)))
    
    # Rename
    colnames(dummies_PPO) <- groups_PPO
    colnames(dummies_PPO) <- paste(colnames(dummies_PPO), ".PPO", sep = "")
    
    
    ## ----------------------- ##
    ##     Away SH Defense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_SHD, function(x, test.A1)  which((test.A1[, "home_on_1"] == x | 
                                                           test.A1[, "home_on_2"] == x |
                                                           test.A1[, "home_on_3"] == x |
                                                           test.A1[, "home_on_4"] == x |
                                                           test.A1[, "home_on_5"] == x |
                                                           test.A1[, "home_on_6"] == x) & test.A1[, "away_goalie"] != x), test.A1 = test.A1)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_SHD <- sparseMatrix(i, j, dims = c(nrow(test.A1), length(groups_SHD)))
    
    # Rename
    colnames(dummies_SHD) <- groups_SHD
    colnames(dummies_SHD) <- paste(colnames(dummies_SHD), ".SHD", sep = "")
    
    
    ## ----------------------- ##
    ##     Away SH Offense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_SHO, function(x, test.A1)  which(99999 == x | 99999 == x | 99999 == x | 99999 == x | 99999 == x | 99999 == x), test.A1 = test.A1)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_SHO <- sparseMatrix(i, j, dims = c(nrow(test.A1), length(groups_SHO)))
    
    # Rename
    colnames(dummies_SHO) <- groups_SHO
    colnames(dummies_SHO) <- paste(colnames(dummies_SHO), ".SHO", sep = "")
    
    
    ## ----------------------- ##
    ##     Away PP Defense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_PPD, function(x, test.A1)  which(99999 == x | 99999 == x | 99999 == x | 99999 == x | 99999 == x | 99999 == x), test.A1 = test.A1)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_PPD <- sparseMatrix(i, j, dims = c(nrow(test.A1), length(groups_PPD)))
    
    # Rename
    colnames(dummies_PPD) <- groups_PPD
    colnames(dummies_PPD) <- paste(colnames(dummies_PPD), ".PPD", sep = "")
    
    
    ## ----------------------- ##
    ##         Combine         ##
    ## ----------------------- ##
    test_sparse.A1 <- cbind(test.A1, dummies_PPO, dummies_SHD, dummies_SHO, dummies_PPD)
    
    rm(test.A1)
    gc()
    
    ##############################################
    
    
    ## Away Table 2 (Away Offense in Home PP Strengths)
    ##############################################
    
    print("away_df_2")
    
    test.A2 <- data %>% 
      filter(game_strength_state %in% st.pp_home) %>% 
      arrange(game_id, event_index) %>% 
      mutate(shift_ID = cumsum(event_type == st.on_ID), 
             off_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 1), 
             def_zonestart = 1 * (event_type == st.fac_ID & home_zonestart == 3), 
             off_lead = -1 * home_lead
             ) %>% 
      group_by(game_id, 
               shift_ID, 
               game_strength_state, 
               home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6,
               away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, 
               off_lead
               ) %>% 
      summarise(home_goalie = first(home_goalie), 
                away_goalie = first(away_goalie), 
                off_team = first(away_team), 
                def_team = first(home_team), 
                length = sum(event_length),
                GF60 = (sum(event_type == st.goal_ID & event_team == away_team) / sum(event_length)) * 3600, 
                CF60 =  (sum(event_type %in% st.corsi_events & event_team == away_team) / sum(event_length)) * 3600, 
                xGF60 = (sum((event_type %in% st.fenwick_events & event_team == away_team) * pred_goal) / sum(event_length)) * 3600, 
                off_zonestart = sum(off_zonestart), 
                def_zonestart = sum(def_zonestart), 
                btb = first(away_btb)
                ) %>% 
      filter(length > 0) %>% 
      ungroup() %>% 
      add_column(., n = 0, .before = 1) %>% 
      mutate(n = as.numeric(row_number()), 
             score_down_3 = 1 * (off_lead <= -3), 
             score_down_2 = 1 * (off_lead == -2), 
             score_down_1 = 1 * (off_lead == -1), 
             score_up_1 =   1 * (off_lead ==  1), 
             score_up_2 =   1 * (off_lead ==  2), 
             score_up_3 =   1 * (off_lead >=  3), 
             score_even =   1 * (off_lead ==  0), 
             score_trail =  1 * (off_lead < 0), 
             score_lead =   1 * (off_lead > 0), 
             off_zonestart = 1 * (off_zonestart >= 1), 
             def_zonestart = 1 * (def_zonestart >= 1), 
             state_5v4 = 1 * (game_strength_state %in% st.5v4),
             state_5v3 = 1 * (game_strength_state %in% st.5v3),
             state_4v3 = 1 * (game_strength_state %in% st.4v3),
             is_home = 0
             ) %>% 
      select(-c(game_id, off_lead, shift_ID)) %>% 
      data.matrix()
    
    
    ## ----------------------- ##
    ##     Away PP Offense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_PPO, function(x, test.A2)  which(99999 == x | 99999 == x |99999 == x | 99999 == x | 99999 == x | 99999 == x), test.A2 = test.A2)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_PPO <- sparseMatrix(i, j, dims = c(nrow(test.A2), length(groups_PPO)))
    
    # Rename
    colnames(dummies_PPO) <- groups_PPO
    colnames(dummies_PPO) <- paste(colnames(dummies_PPO), ".PPO", sep = "")
    
    
    ## ----------------------- ##
    ##     Away SH Defense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_SHD, function(x, test.A2)  which(99999 == x | 99999 == x | 99999 == x | 99999 == x | 99999 == x | 99999 == x), test.A2 = test.A2)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_SHD <- sparseMatrix(i, j, dims = c(nrow(test.A2), length(groups_SHD)))
    
    # Rename
    colnames(dummies_SHD) <- groups_SHD
    colnames(dummies_SHD) <- paste(colnames(dummies_SHD), ".SHD", sep = "")
    
    
    ## ----------------------- ##
    ##     Away SH Offense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_SHO, function(x, test.A2)  which(test.A2[, "away_on_1"] == x | 
                                                          test.A2[, "away_on_2"] == x |
                                                          test.A2[, "away_on_3"] == x |
                                                          test.A2[, "away_on_4"] == x |
                                                          test.A2[, "away_on_5"] == x |
                                                          test.A2[, "away_on_6"] == x), test.A2 = test.A2)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_SHO <- sparseMatrix(i, j, dims = c(nrow(test.A2), length(groups_SHO)))
    
    # Rename
    colnames(dummies_SHO) <- groups_SHO
    colnames(dummies_SHO) <- paste(colnames(dummies_SHO), ".SHO", sep = "")
    
    
    ## ----------------------- ##
    ##     Away PP Defense     ##
    ## ----------------------- ##
    
    # Determine Columns
    tmp <- lapply(groups_PPD, function(x, test.A2)  which((test.A2[, "home_on_1"] == x | 
                                                           test.A2[, "home_on_2"] == x |
                                                           test.A2[, "home_on_3"] == x |
                                                           test.A2[, "home_on_4"] == x |
                                                           test.A2[, "home_on_5"] == x |
                                                           test.A2[, "home_on_6"] == x) & test.A2[, "away_goalie"] != x), test.A2 = test.A2)
    
    # Make Dummy Variables
    j = rep(seq_along(tmp), lengths(tmp))
    i = unlist(tmp)
    dummies_PPD <- sparseMatrix(i, j, dims = c(nrow(test.A2), length(groups_PPD)))
    
    # Rename
    colnames(dummies_PPD) <- groups_PPD
    colnames(dummies_PPD) <- paste(colnames(dummies_PPD), ".PPD", sep = "")
    
    
    ## ----------------------- ##
    ##          Combine        ##
    ## ----------------------- ##
    test_sparse.A2 <- cbind(test.A2, dummies_PPO, dummies_SHD, dummies_SHO, dummies_PPD)
    rm(test.A2)
    gc()
    
    ##############################################
    
    
    
    ## ------------------------------ ##
    ##             Big Join           ##
    ## ------------------------------ ##
    
    print("combine")
    
    test_sparse.All <- rbind(test_sparse.H1, test_sparse.A1, test_sparse.H2, test_sparse.A2)
    
    rm(test_sparse.H1, test_sparse.A1, test_sparse.H2, test_sparse.A2)
    gc()
    
    return(test_sparse.All)
    
    }
  APM_PP <- fun.APM_PP_sparse_GF(data =  pbp_part)
  
  
  # All players TOI and position in RAPM design matrix
  player_base_PP_ <- pbp_goalie %>% 
    mutate(position = 3) %>% 
    select(player, TOI, position) %>% 
    rbind(select(qual_skater_PP, player, TOI, position)) %>% 
    left_join(., select(names_match_PP, player, ID), by = "player") %>% 
    arrange(player)
  
  player_base_SH_ <- pbp_goalie %>% 
    mutate(position = 3) %>% 
    select(player, TOI, position) %>% 
    rbind(select(qual_skater_SH, player, TOI, position)) %>% 
    left_join(., select(names_match_SH, player, ID), by = "player") %>% 
    arrange(player)
  
  gc()
  
  
  ## ------------------------ ##
  ##          Model           ##
  ## ------------------------ ##
  
  # Cleanup / separate target, weights, and predictors
  # Response / Weights
  length_l <- list(APM_PP[, "length"])
  GF60_l <-   list(APM_PP[, "GF60"])
  CF60_l <-   list(APM_PP[, "CF60"])
  xGF60_l <-  list(APM_PP[, "xGF60"])
  
  # Remove NaNs
  length_PP <- unlist(rapply(length_l, f = function(x) ifelse(x == 0, 1, x), how = "replace"))
  GF60_PP <-   unlist(rapply(GF60_l, f = function(x) ifelse(is.nan(x), 0, x), how = "replace"))
  CF60_PP <-   unlist(rapply(CF60_l, f = function(x) ifelse(is.nan(x), 0, x), how = "replace"))
  xGF60_PP <-  unlist(rapply(xGF60_l, f = function(x) ifelse(is.nan(x), 0, x), how = "replace"))
  
  
  ## sparse matrix column names:
  # [1]  "n"                   "game_strength_state" "home_on_1"           "home_on_2"           "home_on_3"           "home_on_4"           "home_on_5"          
  # [8]  "home_on_6"           "away_on_1"           "away_on_2"           "away_on_3"           "away_on_4"           "away_on_5"           "away_on_6"          
  # [15] "home_goalie"         "away_goalie"         "off_team"            "def_team"            "length"              "GF60"                "CF60"               
  # [22] "xGF60"               "off_zonestart"       "def_zonestart"       "btb"                 "score_down_3"        "score_down_2"        "score_down_1"       
  # [29] "score_up_1"          "score_up_2"          "score_up_3"          "score_even"          "score_trail"         "score_lead"          "state_5v4"          
  # [36] "state_5v3"           "state_4v3"           "is_home"
  
  
  # Make Base Design Matrix - GF, xGF & CF
  APM_PP_g <- APM_PP[, -c(1:22, 26:32, 35)] # *** corrected factor variables, 3-level score state
  
  
  # Design matrix without goalies (xGF & CF)
  goalies_SHD <- paste0(qual_goalies, ".SHD")
  goalies_PPD <- paste0(qual_goalies, ".PPD")
  goalie_names <- c(goalies_SHD, goalies_PPD)
  
  no_goalie_names <- as.vector(colnames(APM_PP_g))
  no_goalie_names <- no_goalie_names[!no_goalie_names %in% goalie_names]
  
  APM_PP_s <- APM_PP_g[, no_goalie_names]
  
  
  # Cleanup
  rm(APM_PP, GF60_l, CF60_l, xGF60_l, length_l, goalies_PPD, goalies_SHD, goalie_names, no_goalie_names)
  gc()
  
  
  ## ---------------------------- ##
  ##   Cross Validation - Goals   ##
  ## ---------------------------- ##
  
  registerDoMC(cores = 2)
  
  print(" -- Cross Validation: Goals")
  
  CV_results_GF <- cv.glmnet(x = APM_PP_g, 
                             y = GF60_PP, 
                             weights = length_PP, 
                             alpha = 0, 
                             nfolds = 10, 
                             standardize = FALSE, 
                             parallel = TRUE)
  gc()
  
  
  ## ------------------------- ##
  ##   Cross Validation - xG   ##
  ## ------------------------- ##
  
  print(" -- Cross Validation: xG")
  
  CV_results_xG <- cv.glmnet(x = APM_PP_s, 
                             y = xGF60_PP, 
                             weights = length_PP, 
                             alpha = 0, 
                             nfolds = 10, 
                             standardize = FALSE, 
                             parallel = TRUE)
  gc()
  
  
  ## ---------------------------- ##
  ##   Cross Validation - Corsi   ##
  ## ---------------------------- ##
  
  print(" -- Cross Validation: Corsi")
  
  CV_results_CF <- cv.glmnet(x = APM_PP_s, 
                             y = CF60_PP, 
                             weights = length_PP, 
                             alpha = 0, 
                             nfolds = 10, 
                             standardize = FALSE, 
                             parallel = TRUE)
  gc()
  
  
  
  ## ---------------- ##
  ##   Results - GF   ##
  ## ---------------- ##
  
  print("Results / Combine")
  
  fun.APM_bind_PP <- function(model_data, names_data) {
    
    # Pull out coefficients
    APM_test <- data.frame(as.matrix(coef(model_data, s = model_data$lambda.min))) %>% 
      rownames_to_column(var = "APM_names")
    
    
    # Remove suffixes
    APM_test_SHD <- APM_test %>% 
      filter(grepl(".SHD", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".SHD", "", APM_names)) %>% 
      rename(SHD = X1)
    
    APM_test_PPO <- APM_test %>% 
      filter(grepl(".PPO", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".PPO", "", APM_names)) %>% 
      rename(PPO = X1)
    
    APM_test_SHO <- APM_test %>% 
      filter(grepl(".SHO", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".SHO", "", APM_names)) %>% 
      rename(SHO = X1)
    
    APM_test_PPD <- APM_test %>% 
      filter(grepl(".PPD", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".PPD", "", APM_names)) %>% 
      rename(PPD = X1)
    
    # Join
    APM_main <- APM_test_PPO %>% full_join(., APM_test_SHD, by = "APM_names")
    
    APM_xtra <- APM_test_SHO %>% full_join(., APM_test_PPD, by = "APM_names")
    
    APM_all <- full_join(APM_main, APM_xtra, by = "APM_names")
    
    APM_all$APM_names <- names_data$player[match(APM_all$APM_names, names_data$ID)]
    
    
    APM_list <- list(APM_all = APM_all, 
                     APM_coef = APM_test)
    
    return(APM_list)
    
    }
  APM_list_PP_GF <- fun.APM_bind_PP(model_data =   CV_results_GF, 
                                    names_data =   names_match_PP)
  
  APM_initial_PP_GF <-  APM_list_PP_GF$APM_all
  APM_raw_coef_PP_GF <- APM_list_PP_GF$APM_coef
  rm(APM_list_PP_GF)
  
  
  fun.APM_PP_final <- function(APM_data, games_PP, games_SH) {
    
    APM_PP <- APM_data %>% 
      rename(player = APM_names) %>% 
      left_join(., names_match_PP, by = c("player"))
    
    # TOI Joins
    TOI_PP <- games_PP %>% 
      filter(TOI > 0) %>% 
      group_by(player) %>% 
      mutate(n = n()) %>% 
      summarise(TOI_PP = sum(TOI)) %>% 
      data.frame()
    
    TOI_SH <- games_SH %>% 
      filter(TOI > 0) %>% 
      group_by(player) %>% 
      mutate(n = n()) %>% 
      summarise(TOI_SH = sum(TOI)) %>% 
      data.frame()
    
    APM_PP_join <- APM_PP %>% 
      left_join(., TOI_PP, by = "player") %>% 
      left_join(., TOI_SH, by = "player")
    
    
    # GF APM Chart
    final <- APM_PP_join %>% 
      filter(!is.na(player)) %>% 
      mutate(PPO_impact = PPO * (TOI_PP / 60), 
             SHD_impact = SHD * (TOI_SH / 60), 
             SHO_impact = SHO * (TOI_SH / 60), 
             PPD_impact = PPD * (TOI_PP / 60), 
             PPO = ifelse(PPO != 0 & position == 3, 0, PPO), 
             SHO = ifelse(SHO != 0 & position == 3, 0, SHO), 
             TOI_PP = round(TOI_PP, 2), 
             TOI_SH = round(TOI_SH, 2)
             ) %>% 
      select(player, position, TOI_PP, TOI_SH, PPO, SHD, SHO, PPD, PPO_impact:PPD_impact) %>% 
      mutate_at(vars(PPO:PPD), funs(round(., 3))) %>% 
      mutate_at(vars(PPO_impact:PPD_impact), funs(round(., 2))) %>% 
      rename_at(vars(PPO:PPD_impact), funs(paste0(., "_GF"))) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
    
    return(final)
    
    }
  APM_PP_GF <- fun.APM_PP_final(APM_data =  APM_initial_PP_GF, 
                                games_PP =  games_data_PP, 
                                games_SH =  games_data_SH)
  
  
  ## ---------------- ##
  ##   Results - xG   ##
  ## ---------------- ##
  
  fun.APM_bind_PP <- function(model_data, names_data) {
    
    # Pull out coefficients
    APM_test <- data.frame(as.matrix(coef(model_data, s = model_data$lambda.min))) %>% 
      rownames_to_column(var = "APM_names")
    
    
    # Remove suffixes
    APM_test_SHD <- APM_test %>% 
      filter(grepl(".SHD", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".SHD", "", APM_names)) %>% 
      rename(SHD = X1)
    
    APM_test_PPO <- APM_test %>% 
      filter(grepl(".PPO", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".PPO", "", APM_names)) %>% 
      rename(PPO = X1)
    
    APM_test_SHO <- APM_test %>% 
      filter(grepl(".SHO", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".SHO", "", APM_names)) %>% 
      rename(SHO = X1)
    
    APM_test_PPD <- APM_test %>% 
      filter(grepl(".PPD", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".PPD", "", APM_names)) %>% 
      rename(PPD = X1)
    
    # Join
    APM_main <- APM_test_PPO %>% full_join(., APM_test_SHD, by = "APM_names")
    
    APM_xtra <- APM_test_SHO %>% full_join(., APM_test_PPD, by = "APM_names")
    
    APM_all <- full_join(APM_main, APM_xtra, by = "APM_names")
    
    APM_all$APM_names <- names_data$player[match(APM_all$APM_names, names_data$ID)]
    
    
    APM_list <- list(APM_all = APM_all, 
                     APM_coef = APM_test)
    
    return(APM_list)
  
    }
  APM_list_PP_xG <- fun.APM_bind_PP(model_data =   CV_results_xG, 
                                    names_data =   names_match_PP)
  
  APM_initial_PP_xG <- APM_list_PP_xG$APM_all
  APM_raw_coef_PP_xG <- APM_list_PP_xG$APM_coef
  rm(APM_list_PP_xG)
  
  
  fun.APM_PP_final_xG <- function(APM_data, games_PP, games_SH) {
    
    APM_PP <- APM_data %>% 
      rename(player = APM_names) %>% 
      left_join(., names_match_PP, by = c("player"))
    
    # Testing Impact
    TOI_PP <- games_PP %>% 
      filter(TOI > 0) %>% 
      group_by(player) %>% 
      mutate(n = n()) %>% 
      summarise(TOI_PP = sum(TOI)) %>% 
      data.frame()
    
    TOI_SH <- games_SH %>% 
      filter(TOI > 0) %>% 
      group_by(player) %>% 
      mutate(n = n()) %>% 
      summarise(TOI_SH = sum(TOI)) %>% 
      data.frame()
    
    APM_PP_join <- APM_PP %>% 
      left_join(., TOI_PP, by = "player") %>% 
      left_join(., TOI_SH, by = "player")
    
    
    # GF APM Chart
    final <- APM_PP_join %>% 
      filter(!is.na(player)) %>% 
      mutate(PPO_impact = PPO * (TOI_PP / 60), 
             SHD_impact = SHD * (TOI_SH / 60), 
             SHO_impact = SHO * (TOI_SH / 60), 
             PPD_impact = PPD * (TOI_PP / 60), 
             PPO = ifelse(PPO != 0 & position == 3, 0, PPO), 
             SHO = ifelse(SHO != 0 & position == 3, 0, SHO), 
             TOI_PP = round(TOI_PP, 2), 
             TOI_SH = round(TOI_SH, 2)
             ) %>% 
      select(player, position, TOI_PP, TOI_SH, PPO, SHD, SHO, PPD, PPO_impact:PPD_impact) %>% 
      mutate_at(vars(PPO:PPD), funs(round(., 3))) %>% 
      mutate_at(vars(PPO_impact:PPD_impact), funs(round(., 2))) %>% 
      rename_at(vars(PPO:PPD_impact), funs(paste0(., "_xG"))) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
    
    return(final)
  
    }
  APM_PP_xG <- fun.APM_PP_final_xG(APM_data =  APM_initial_PP_xG, 
                                   games_PP =  games_data_PP, 
                                   games_SH =  games_data_SH)
  
  
  ## ------------------- ##
  ##   Results - Corsi   ##
  ## ------------------- ##
  
  fun.APM_bind_PP <- function(model_data, names_data) {
    
    # Pull out coefficients
    APM_test <- data.frame(as.matrix(coef(model_data, s = model_data$lambda.min))) %>% 
      rownames_to_column(var = "APM_names")
    
    
    # Remove suffixes
    APM_test_SHD <- APM_test %>% 
      filter(grepl(".SHD", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".SHD", "", APM_names)) %>% 
      rename(SHD = X1)
    
    APM_test_PPO <- APM_test %>% 
      filter(grepl(".PPO", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".PPO", "", APM_names)) %>% 
      rename(PPO = X1)
    
    APM_test_SHO <- APM_test %>% 
      filter(grepl(".SHO", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".SHO", "", APM_names)) %>% 
      rename(SHO = X1)
    
    APM_test_PPD <- APM_test %>% 
      filter(grepl(".PPD", APM_names), APM_names != "is_home") %>% 
      mutate(APM_names = gsub(".PPD", "", APM_names)) %>% 
      rename(PPD = X1)
    
    # Join
    APM_main <- APM_test_PPO %>% full_join(., APM_test_SHD, by = "APM_names")
    
    APM_xtra <- APM_test_SHO %>% full_join(., APM_test_PPD, by = "APM_names")
    
    APM_all <- full_join(APM_main, APM_xtra, by = "APM_names")
    
    APM_all$APM_names <- names_data$player[match(APM_all$APM_names, names_data$ID)]
    
    
    APM_list <- list(APM_all = APM_all, 
                     APM_coef = APM_test)
    
    return(APM_list)
  
    }
  APM_list_PP_CF <- fun.APM_bind_PP(model_data =   CV_results_CF, 
                                    names_data =   names_match_PP)
  
  APM_initial_PP_CF <- APM_list_PP_CF$APM_all
  APM_raw_coef_PP_CF <- APM_list_PP_CF$APM_coef
  rm(APM_list_PP_CF)
  
  
  fun.APM_PP_final_CF <- function(APM_data, games_PP, games_SH) {
    
    APM_PP <- APM_data %>% 
      rename(player = APM_names) %>% 
      left_join(., names_match_PP, by = c("player"))
    
    # Testing Impact
    TOI_PP <- games_PP %>% 
      filter(TOI > 0) %>% 
      group_by(player) %>% 
      mutate(n = n()) %>% 
      summarise(TOI_PP = sum(TOI)) %>% 
      data.frame()
    
    TOI_SH <- games_SH %>% 
      filter(TOI > 0) %>% 
      group_by(player) %>% 
      mutate(n = n()) %>% 
      summarise(TOI_SH = sum(TOI)) %>% 
      data.frame()
    
    APM_PP_join <- APM_PP %>% 
      left_join(., TOI_PP, by = "player") %>% 
      left_join(., TOI_SH, by = "player")
    
    
    # GF APM Chart
    final <- APM_PP_join %>% 
      filter(!is.na(player)) %>% 
      mutate(PPO_impact = PPO * (TOI_PP / 60), 
             SHD_impact = SHD * (TOI_SH / 60), 
             SHO_impact = SHO * (TOI_SH / 60), 
             PPD_impact = PPD * (TOI_PP / 60), 
             PPO = ifelse(PPO != 0 & position == 3, 0, PPO), 
             SHO = ifelse(SHO != 0 & position == 3, 0, SHO), 
             TOI_PP = round(TOI_PP, 2), 
             TOI_SH = round(TOI_SH, 2)
             ) %>% 
      select(player, position, TOI_PP, TOI_SH, PPO, SHD, SHO, PPD, PPO_impact:PPD_impact) %>% 
      mutate_at(vars(PPO:PPD), funs(round(., 3))) %>% 
      mutate_at(vars(PPO_impact:PPD_impact), funs(round(., 2))) %>% 
      rename_at(vars(PPO:PPD_impact), funs(paste0(., "_CF"))) %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
    
    return(final)
  
    }
  APM_PP_CF <- fun.APM_PP_final_CF(APM_data =  APM_initial_PP_CF, 
                                   games_PP =  games_data_PP, 
                                   games_SH =  games_data_SH)
  
  
  
  ## ------------------------------------ ##
  ##   Join GF, xGF, and CF RAPM Tables   ##
  ## ------------------------------------ ##
  
  teams <- games_data_PP %>% 
    group_by(player, Team) %>% 
    summarise(TOI = sum(TOI)) %>% 
    mutate(Team = paste0(Team, collapse = "/")) %>% 
    group_by(player, Team) %>% 
    summarise(TOI = sum(TOI)) %>% 
    select(player, Team) %>% 
    data.frame()
  
  goalie_teams <- rbind(
    pbp_data %>% 
      group_by(home_goalie, home_team) %>% 
      summarise() %>% 
      filter(!is.na(home_goalie)) %>% 
      rename(player = home_goalie, 
             Team = home_team) %>% 
      data.frame(), 
    pbp_data %>% 
      group_by(away_goalie, away_team) %>% 
      summarise() %>% 
      filter(!is.na(away_goalie)) %>% 
      rename(player = away_goalie, 
             Team = away_team) %>% 
      data.frame()
    ) %>% 
    group_by(player, Team) %>% 
    summarise() %>% 
    mutate(Team = paste0(Team, collapse = "/")) %>% 
    group_by(player, Team) %>% 
    summarise() %>% 
    data.frame()
  
  
  APM_PP_join <- APM_PP_GF %>% 
    filter(position != 3) %>% 
    left_join(., teams, by = "player") %>% 
    left_join(., APM_PP_xG, by = c("player", "position", "TOI_PP", "TOI_SH")) %>% 
    left_join(., APM_PP_CF, by = c("player", "position", "TOI_PP", "TOI_SH")) %>% 
    mutate(season = unique(games_data_PP$season)) %>% 
    select(player, position, season, Team, TOI_PP, TOI_SH,  
           PPO_GF:PPD_GF, PPO_xG:PPD_xG, PPO_CF:PPD_CF, 
           PPO_impact_GF:PPD_impact_GF, 
           PPO_impact_xG:PPD_impact_xG, 
           PPO_impact_CF:PPD_impact_CF)
  
  APM_PP_join_rates <- APM_PP_join %>% 
    select(player, position, season, Team, TOI_PP, TOI_SH, PPO_GF:PPD_GF, PPO_xG:PPD_xG, PPO_CF:PPD_CF)
  
  APM_PP_join_impact <- APM_PP_join %>% 
    select(player, position, season, Team, TOI_PP, TOI_SH, PPO_impact_GF:PPD_impact_GF, PPO_impact_xG:PPD_impact_xG, PPO_impact_CF:PPD_impact_CF)
  
  APM_PP_goalies <- APM_PP_GF %>% 
    filter(position == 3) %>% 
    left_join(., goalie_teams, by = "player") %>% 
    mutate(season = unique(games_data_PP$season)) %>% 
    select(player, position, season, Team, 
           SHD_GF, PPD_GF)
  
  
  ## ----------------------------- ##
  ##   List of Objects to Return   ##
  ## ----------------------------- ##
  
  return_list <- list(
    
    # Target variables
    target_GF = GF60_PP, 
    target_xG = xGF60_PP, 
    target_CF = CF60_PP, 
    
    # Design matrices
    design_GF =    APM_PP_g, 
    design_xG_CF = APM_PP_s, 
    
    # Weights
    weights = length_PP, 
    
    # cv.glmnet objects
    cv_results_GF = CV_results_GF, 
    cv_results_xG = CV_results_xG, 
    cv_results_CF = CV_results_CF,
    
    # Raw coefficients from cv.glmnet objects
    RAPM_coef_GF = APM_raw_coef_PP_GF, 
    RAPM_coef_xG = APM_raw_coef_PP_xG, 
    RAPM_coef_CF = APM_raw_coef_PP_CF, 
    
    # Player information
    player_base_PP = player_base_PP_,
    player_base_SH = player_base_SH_, 
    IDs_master_PP =  names_match_PP,
    IDs_master_SH =  names_match_SH,
    
    # Final RAPM tables
    RAPM_PP_join =    APM_PP_join, 
    RAPM_PP_rates =   APM_PP_join_rates, 
    RAPM_PP_impact =  APM_PP_join_impact, 
    RAPM_PP_goalies = APM_PP_goalies
    
    )
  
  return(return_list)
  
  }


#############################










