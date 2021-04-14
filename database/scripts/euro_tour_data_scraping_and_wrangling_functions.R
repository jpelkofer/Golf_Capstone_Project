library(tidyverse)
library(furrr)
library(jsonlite)
library(rvest)
library(lubridate)

plan(multisession, workers = 4)


get_tournaments <- function(tour, season) {
  
  # pulls schedule of european tour tournaments for particular season
  
  # tour can be one of c("euro", "ct")
  
  tour_entry <- ifelse(tour == "euro", "european-tour", "challenge-tour")
  
  url <- paste0("https://www.europeantour.com/", tour_entry, "/schedule/", season, "/")
  webpage <- read_html(url)
  
  tournament_link <-
    webpage %>% 
    html_nodes(".schedule-list-item__location-link") %>% 
    html_attr("href")
  
  tournament_name <- 
    webpage %>% 
    html_nodes(".schedule-list-item__name") %>% 
    html_nodes("span") %>% 
    html_text()
  
  tournament_course_location <- 
    webpage %>% 
    html_nodes(".schedule-list-item__course") %>% 
    html_text()
  
  tournaments_df <- cbind(tour, season, tournament_name, tournament_course_location, tournament_link)
  
  return(tournaments_df)
  
}

# Example
# tournaments <- get_tournaments("euro", 2020)

leaderboard_page <- 

get_tournament_id_and_dates <- function(link) {
  
  # gets tournament id and dates for specific tournament
  
  leaderboard_page <- read_html(paste0("https://www.europeantour.com/", link, "/leaderboard"))
  
  event_string <- 
    leaderboard_page %>% 
    html_nodes(".leaderboard") %>% 
    html_nodes("leaderboard") %>% 
    html_attr(":event-data") 
  
  event_id_data <- 
    event_string %>% 
    str_locate("EventId") %>% 
    as_tibble()
  
  tournament_id <- str_sub(event_string, start = event_id_data$end + 3, end = event_id_data$end + 9)
  
  tournament_end_date <- 
    leaderboard_page %>% 
    html_nodes(".event-hero__date") %>% 
    html_text() %>% str_sub(6,-1) %>% dmy()
  
  date <- seq(tournament_end_date, length = 4, by = -1)
  
  round <- c(4:1)
  
  tournament_ids_df <- tibble(tournament_id, round, date)
  
  Sys.sleep(1)
  
  return(tournament_ids_df)
  
}

# Example
# tournament_ids <- 
#   tournaments %>% 
#   pull(tournament_link) %>% 
#   future_map(get_tournament_id_and_dates)

# Will need to use this to join dates with data from api calls
# tournament_id_round_dates_df <- 
#   tournament_ids %>% 
#   enframe() %>% 
#   select(-name) %>% 
#   unnest(value)

# Tournaments data frame needs par and yards added
# tournaments_df <- 
#   tournament_id_round_dates_df %>% 
#   distinct(tournament_id) %>%  
#   bind_cols(tournaments) %>% 
#   separate(tournament_course_location, into = c("course", "location"), sep = ",", extra = "merge" ) %>% 
#   select(tour, season, everything())

scrape_euro_hole_description_function <- function(tournament_id, round) {
  
  holes_json <- jsonlite::read_json(paste0("https://www.europeantour.com/api/sportdata/HoleAverages/Event/",tournament_id,"/Round/", round))
  
  # get course id(s) for tournament
  courseId_list <- 
    holes_json %>% 
    purrr::pluck("Courses") %>% 
    map_chr( ~ purrr::pluck(.x, "CourseId"))
  
  courses <-  c(1:length(courseId_list))
  
  holes_json %>% 
    purrr::pluck("Courses") %>% 
    map(~ purrr::pluck(.x, "Holes")) %>% 
    enframe() %>% 
    mutate(course_id = courses) %>% 
    mutate(tournament_id = tournament_id,
           round = round) %>%
    unnest(value) %>% 
    unnest_wider(value) %>%
    rename(hole_number = HoleNo,
           par = HolePar) %>% 
    select(tournament_id, course_id, round, hole_number:Yards) %>% 
    rename(hole = hole_number) %>%
    janitor::clean_names()
  
  
}

get_rounds_data <- function(tour, season, tournament_id) {
  
  rounds_json <- jsonlite::read_json(paste0("https://www.europeantour.com/api/sportdata/Leaderboard/Strokeplay/", tournament_id))
  
  rounds_data <- 
    purrr::pluck(rounds_json, "Players") %>% 
    enframe() %>% 
    unnest_wider(value) %>% 
    janitor::clean_names() %>% 
    select(player_id, first_name, last_name,  rounds) %>% 
    unnest(rounds) %>% 
    unnest_wider(rounds) %>% 
    mutate(last_name = last_name %>% str_to_title()) %>% 
    unite("player_name", first_name, last_name, sep = " ") %>% 
    mutate(player_id = player_id %>% as.character(),
           tour = tour,
           season = season,
           tournament_id = tournament_id) %>% 
    rename(
      round     = RoundNo,
      tot_strokes = Strokes,
      course_id   = CourseNo) %>% 
    select(player_id:player_name, tour:tournament_id, course_id, round:tot_strokes)
  
  return(rounds_data)
  
}

# Example
# rounds_data <- 
#   tournaments_df %>% 
#   select(tour:tournament_id) %>% 
#   future_pmap_dfr(purrr::possibly(get_rounds_data, otherwise = NA)) 


get_holes_data <- function(tour, season, tournament_id, player_id, player_name) {
  
  
  holes_json <- jsonlite::read_json(paste0("https://www.europeantour.com/api/sportdata/Scorecard/Strokeplay/Event/", tournament_id, "/Player/", player_id))
  
  holes_data <- 
    holes_json %>% 
    purrr::pluck("Rounds") %>% 
    enframe() %>% 
    unnest_wider(value) %>% 
    unnest(Holes) %>% 
    select(RoundNo, CourseNo, Holes) %>% 
    unnest_wider(Holes) %>% 
    mutate(player_id = player_id,
           player_name = player_name, 
           tour = tour,
           season = season,
           tournament_id = tournament_id) %>% 
    rename(round = RoundNo,
           course_id = CourseNo,
           hole = HoleNo,
           score = Strokes,
           score_type = ScoreClass) %>% 
    select(player_id:tournament_id, course_id, everything())
  
  return(holes_data)
    
  
}

# Example
# holes_data <- 
#   rounds_data %>% 
#   select(tour, season, tournament_id, player_id, player_name) %>% 
#   distinct() %>% 
#   head(5000) %>% 
#   future_pmap_dfr(purrr::possibly(get_holes_data, otherwise = NA))


get_stats_data <- function(tour, season, tournament_id, player_id, player_name, round) {
  
  stats_json <- jsonlite::read_json(paste0("https://www.europeantour.com/api/sportdata/Scorecard/Strokeplay/Stats/Event/",tournament_id, "/Player/",player_id,"/Round/", round))
  
  stats_data <- 
    stats_json %>% 
    as_tibble() %>% 
    select(PlayerId, DrivingDistanceAvg, DrivingAccuracyAvg, GirAvg, PuttsPerGirAvg) %>% 
    janitor::clean_names() %>%
    mutate(
      player_name = player_name, 
      tour = tour,
      season = season,
      tournament_id = tournament_id) %>% 
    rename(driving_distance = driving_distance_avg, driving_accuracy = driving_accuracy_avg, gir = gir_avg, putts_per_gir = putts_per_gir_avg ) %>%   
    select(player_id, player_name:tournament_id, everything())
  
  return(stats_data)
  
}

# Example
# stats_data <-
#   rounds_data %>%
#   select(tour, season, tournament_id, player_id, player_name, round) %>%
#   group_by(tour, season, tournament_id, player_id, player_name) %>%
#   summarise(round = max(round)) %>%
#   ungroup() %>%
#   future_pmap(purrr::possibly(get_stats_data, otherwise = NA)) %>% 
#   enframe() %>% 
#   unnest_wider(value) %>% 
#   select(-c(name, `...1`))


# Preparing

# Organize rounds data for DB



prepare_data_for_db <- function(tournament_dates_df, rounds_data, holes_data, stats_data) {
  
  tournament_rounds <- 
    rounds_data %>% 
    group_by(season, tournament_id, course_id, round) %>% 
    summarise(field_avg_strokes_rd = tot_strokes %>% mean()) %>% 
    ungroup()
  
  rounds_df <- 
    rounds_data %>% 
    left_join(tournament_dates_df, by = c("tournament_id", "round")) %>% 
    left_join(tournament_rounds, by = c("season", "tournament_id", "course_id", "round")) %>% 
    mutate(player_id = player_id %>% as.character(),
           time              = NA_character_,
           score_to_par      = NA_character_,
           strokes_gained_rd = field_avg_strokes_rd - tot_strokes,
           start_position    = NA_integer_,
           end_position      = NA_integer_,
           movement          = NA_integer_
    ) %>% 
    select(player_id:course_id, date, time, round, tot_strokes, score_to_par, field_avg_strokes_rd, strokes_gained_rd:movement)
  
  # Organize holes data for DB
  
  hole_scoring <- 
    holes_data %>% 
    filter(!score_type %in% c("")) %>% 
    left_join(tournament_dates_df, by = c("tournament_id", "round")) %>% 
    mutate(player_id = player_id %>% as.character(),
           score_type = case_when(
             score_type == "bi" ~ "BIRDIE",
             score_type == "pa" ~ "PAR",
             score_type == "bo" ~ "BOGEY",
             score_type == "ea" ~ "EAGLE",
             score_type == "db" ~ "DOUBLE_BOGEY",
             score_type == "tb" ~ "TRIPLE_BOGEY",
             score_type == "al" ~ "DOUBLE_EAGLE"
           ),
           score_value = case_when(
             score_type == "BIRDIE" ~  -1,
             score_type == "PAR" ~   0,
             score_type == "BOGEY" ~  +1,
             score_type == "EAGLE" ~  -2,
             score_type == "DOUBLE_BOGEY" ~  +2,
             score_type == "TRIPLE_BOGEY" ~  +3,
             score_type == "DOUBLE_EAGLE" ~ -3
           )) %>% 
    mutate(par = score - score_value,
           dk_pts_classic = case_when(
             score_type %in% c("DOUBLE_EAGLE") ~ 13,
             score_type %in% c("EAGLE") ~ 8,
             score_type %in% c("BIRDIE") ~ 3,
             score_type %in% c("PAR") ~ .5,
             score_type %in% c("BOGEY") ~ -.5,
             score_type %in% c("DOUBLE_BOGEY", "TRIPLE_BOGEY", "OTHER") ~ -1
           ),
           dk_pts_wkend = case_when(
             score_type %in% c("DOUBLE_EAGLE") ~ 10,
             score_type %in% c("EAGLE") ~ 6,
             score_type %in% c("BIRDIE") ~ 3,
             score_type %in% c("PAR") ~ .5,
             score_type %in% c("BOGEY") ~ -.5,
             score_type %in% c("DOUBLE_BOGEY", "TRIPLE_BOGEY", "OTHER") ~ -1
           ),
           dk_pts_sd = case_when(
             score_type %in% c("DOUBLE_EAGLE") ~ 16,
             score_type %in% c("EAGLE") ~ 11,
             score_type %in% c("BIRDIE") ~ 5.75,
             score_type %in% c("PAR") ~ 1.5,
             score_type %in% c("BOGEY") ~ -1.8,
             score_type %in% c("DOUBLE_BOGEY", "TRIPLE_BOGEY", "OTHER") ~ -3.9
           ),
           fd_pts_classic = case_when(
             score_type %in% c("EAGLE", "DOUBLE_EAGLE") ~ 7,
             score_type %in% c("BIRDIE") ~ 3.1,
             score_type %in% c("PAR") ~ .5,
             score_type %in% c("BOGEY") ~ -1,
             score_type %in% c("DOUBLE_BOGEY", "TRIPLE_BOGEY", "OTHER") ~ -3
           ))
  
  hole_avg <- 
    hole_scoring %>%
    filter(!is.na(score)) %>%
    group_by(season, tournament_id, course_id, round, hole) %>%
    summarize(
      field_avg_score       = mean(score),
      field_dk_classic_avg  = mean(dk_pts_classic),
      field_dk_wkend_avg    = mean(dk_pts_wkend),
      field_dk_sd_avg       = mean(dk_pts_sd),
      field_fd_classic_avg  = mean(fd_pts_classic)
    ) %>%
    ungroup()
  
  holes_df <- 
    hole_scoring %>%
    left_join(select(hole_avg, everything()),
              by = c("season",
                     "tournament_id",
                     "course_id",
                     "round",
                     "hole")) %>%
    mutate(
      strokes_gained_hole    = field_avg_score - score,
      dk_pts_gained_classic  = dk_pts_classic - field_dk_classic_avg,
      dk_pts_gained_wkend    = dk_pts_wkend - field_dk_wkend_avg,
      dk_pts_gained_sd       = dk_pts_sd - field_dk_sd_avg,
      fd_pts_gained_classic  = fd_pts_classic - field_fd_classic_avg,
    ) %>% 
    select(player_id:course_id, date, round, hole, par, yards, score, score_type,
           field_avg_score, strokes_gained_hole,
           dk_pts_classic, field_dk_classic_avg, dk_pts_gained_classic,
           dk_pts_wkend, field_dk_wkend_avg, dk_pts_gained_wkend,
           dk_pts_sd, field_dk_sd_avg, dk_pts_gained_sd,
           fd_pts_classic, field_fd_classic_avg, fd_pts_gained_classic)
  
  # Organize Stats Data
  field_stats <-
    stats_data %>% 
    group_by(tournament_id) %>%
    summarise(
      across(.cols = c(driving_distance, driving_accuracy, gir, putts_per_gir),
             .fns = mean,
             .names = "field_{col}_avg")
    ) %>%
    ungroup()
  
  stats_df <- 
    stats_data %>% 
    left_join(select(tournament_dates_df, -round) %>% 
                group_by(tournament_id) %>% 
                summarise(date = min(date)) %>% 
                ungroup(), by = c("tournament_id")) %>%
    left_join(field_stats, by = c("tournament_id")) %>%
    mutate(player_id = player_id %>% as.character(),
           dob = NA_Date_,
           birth_place = NA_character_,
           hand = NA_character_,
           starting_score = NA_character_,
           total = NA_character_,
           score_to_par = NA_character_,
           possible_gir = NA_real_,
           sand_saves = NA_real_,
           possible_sand_saves = NA_real_,
           eagles = NA_real_,
           birdies = NA_real_,
           pars = NA_real_,
           bogeys = NA_real_,
           dbl = NA_real_,
           penalties = NA_real_,
           earnings = NA_character_,
           official_earnings = NA_character_,
           fed_ex_cup_points = NA_character_,
           dd_over_field = driving_distance - field_driving_distance_avg,
           da_over_field = driving_accuracy - field_driving_accuracy_avg,
           gir_over_field = gir - field_gir_avg,
           putts_per_gir_over_field = putts_per_gir - field_putts_per_gir_avg) %>% 
    select(date, player_id:player_name, dob:hand, tour:tournament_id, starting_score:score_to_par,
           driving_distance, field_driving_distance_avg, dd_over_field,
           driving_accuracy, field_driving_accuracy_avg, da_over_field,
           gir, field_gir_avg, gir_over_field,
           putts_per_gir, field_putts_per_gir_avg, putts_per_gir_over_field,
           possible_gir:fed_ex_cup_points) %>%
    mutate(across(.cols = c(driving_distance:possible_gir), ~ na_if(., 0))) %>%
    mutate(driving_distance = ifelse(driving_distance < 150, NA_real_, driving_distance)) %>% 
    mutate(dd_over_field = ifelse(driving_distance %>% is.na(), NA_real_, dd_over_field))
    
  
  df_names <- c("round_scoring", "hole_scores", "stats")
  list_of_dfs <- list(rounds_df, holes_df, stats_df) %>% 
    set_names(nm = df_names)
  
  return(list_of_dfs)
  
  
}                                   
                                    
            
# Check Data Prior to putting in database ----
check_rounds_data <- function(df) {
  
  # round 5 indicates a playoff, ok with NA here
  missing_dates <- 
    df %>% 
    filter(round != 5) %>% 
    filter(is.na(date))
  
  #* Check duplicate data (each player_id should have 1 occurrence for group by below)
  duplicate_rounds <- 
    df %>% 
    group_by(player_id, season, tournament_id, round) %>% 
    count() %>% 
    filter(n > 1) %>% 
    ungroup()
  
  rounds_names <- c("missing_dates", "duplicate_rounds")
  list_of_rounds_dfs <- list(missing_dates, duplicate_rounds) %>% 
    set_names(nm = rounds_names)
  
  # looking for two empty tables here
  return(list_of_rounds_dfs)
}

check_holes_data <- function(df) {
  
  # round 5 indicates a playoff, ok with NA here
  missing_dates <- 
    df %>% 
    filter(round != 5) %>% 
    filter(is.na(date))
  
  #* Check duplicate data (each player_id should have 1 occurrence for group by below)
  #* Round 5 playoff, players can play same hole in playoff
  duplicate_holes <- 
    df %>% 
    filter(round != 5) %>% 
    group_by(player_id, season, tournament_id, round, hole) %>% 
    count() %>% 
    filter(n > 1) %>% 
    ungroup()
  
  holes_nm <- c("missing_dates", "duplicate_holes")
  list_of_holes_dfs <- list(missing_dates, duplicate_holes) %>% 
    set_names(nm = holes_nm)
  
  # looking for two empty tables here
  return(list_of_holes_dfs)
}    

check_stats_data <- function(df) {
  
  # these are mainly going to be withdraws from tournaments, return df for review
  missing_dates <- 
    df %>% 
    filter(is.na(date))
  
  #* Check duplicate data (each player_id should have 1 occurrence for group by below)
  #* Round 5 playoff, players can play same hole in playoff
  duplicates <- 
    df %>% 
    group_by(player_id, season, tournament_id) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    filter(n > 1)
  
  stats_nm <- c("missing_dates", "duplicates")
  list_of_stats_dfs <- list(missing_dates, duplicates) %>% 
    set_names(nm = stats_nm)
  
  # looking for two empty tables here
  return(list_of_stats_dfs)
}          


fix_euro_names <- function(data) {
  
  data_cleaned <-
    data %>%
    mutate(
      player_name =
        case_when(
          player_name == "Alexander Bj?rk" ~ "Alexander Bjork",
          player_name == "Alex Noren" ~ "Alexander Noren",
          player_name == "Bryson Dechambeau" ~ "Bryson DeChambeau",
          player_name == "Byeong-Hun An" ~ "Byeong Hun An",
          player_name == "Dawie Van Der Walt" ~ "Dawie van der Walt",
          player_name == "Erik Van Rooyen" ~ "Erik van Rooyen",
          player_name == "Harold Varner Iii" ~ "Harold Varner III",
          player_name == "JC Ritchie" ~ "J.C. Ritchie",
          player_name == "Pablo Larraz?bal" ~ "Pablo Larrazabal",
          player_name == "Rafa Cabrera Bello" ~ "Rafael Cabrera Bello",
          player_name == "Robert Macintyre" ~ "Robert MacIntyre",
          player_name == "Rory Mcilroy" ~ "Rory McIlroy",
          player_name == "S?ren Kjeldsen" ~ "Soren Kjeldsen",
          player_name == "Thomas Bj?rn" ~ "Thomas Bjorn",
          player_name == "Thorbj?rn Olesen" ~ "Thorbjorn Olesen",
          player_name == "Ali Al-Shahrani" ~ "Ali Al Shahrani", # updates needed 2/26 starting here and below
          player_name == "Ashun Wu" ~ "A-Shun Wu",
          player_name == "Bryden Macpherson" ~ "Bryden MacPherson",
          player_name == "Cl?ment Sordet" ~ "Clement Sordet",
          player_name == "D A Points" ~ "D.A. Points",
          player_name == "Daniel Van Tonder" ~ "Daniel van Tonder",
          player_name == "Gonzalo Fdez-Casta?o" ~ "Gonzalo Fernandez-Castano",
          player_name == "Graeme Mcdowell" ~ "Graeme McDowell",
          player_name == "Graham Delaet" ~ "Graham DeLaet",
          player_name == "Gr?gory Bourdy" ~ "Gregory Bourdy",
          player_name == "Inhoi Hur" ~ "In-hoi Hur",
          player_name == "James Sugrue  (Am)" ~ "James Sugrue",
          player_name == "Jayden Trey Schaper" ~ "Jayden Schaper",
          player_name == "Jbe Kruger" ~ "Jbe' Kruger",
          player_name == "Jos? Mar?a Olaz?bal" ~ "Jose Maria Olazabal",
          player_name == "Lee Mccoy" ~ "Lee McCoy",
          player_name == "Leun-kwang Kim" ~ "Leun-Kwang Kim",
          player_name == "Marcos Pastor Rufain" ~ "Marcos Rufain",
          player_name == "Mark Power  (Am)" ~ "Mark Power",
          player_name == "Miguel ?ngel Jim?nez" ~ "Miguel Angel Jimenez",
          player_name == "Nick Mccarthy" ~ "Nick McCarthy",
          player_name == "Nicolai H?jgaard" ~ "Nicolai Hojgaard",
          player_name == "Pedro Lencart Silva" ~ "Pedro Lencart",
          player_name == "Per L?ngfors" ~ "Per Langfors",
          player_name == "Rasmus H?jgaard" ~ "Rasmus Hojgaard",
          player_name == "Richard T Lee" ~ "Richard T. Lee",
          player_name == "Robert Mcintyre" ~ "Robert McIntyre",
          player_name == "Rodolfo Cazaubon Jnr" ~ "Rodolfo Cazaubon",
          player_name == "Ryan Mccormick" ~ "Ryan McCormick",
          player_name == "Sami V?lim?ki" ~ "Sami Valimaki",
          player_name == "Sean O'hair" ~ "Sean O'Hair",
          player_name == "Sebastian Garcia Rdez" ~ "Sebastian Garcia Rodriguez",
          player_name == "Siyanda Mwandla" ~ "Siyanda Mwandia",
          player_name == "Soomin Lee" ~ "Soo-min Lee",
          player_name == "Tom Mckibbin (Am)" ~ "Tom Mckibbin",
          player_name == "Trevor Fisher Jnr" ~ "Trevor Fisher",
          
          TRUE ~ player_name
        )
    )
  
  return(data_cleaned)
  
}

















