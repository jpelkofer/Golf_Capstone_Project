library(tidyverse)
library(furrr)
library(jsonlite)
library(rvest)
library(lubridate)

plan(multisession, workers = 3)

# Scraping Functions ----
get_tournaments <- function(tour, season){
  # this function will return a tibble containing tournament ids, tournament name, and year
  
  # tour can be one of c("pga", "ntw")
  
  # season must be >= 2007 and >=2001 for pga
  
  url <- paste0("https://www.espn.com/golf/leaderboard/_/tour/", tour, "/season/", season)
  webpage <- read_html(url)
  
  tournament_data <- webpage %>% 
    html_nodes('optgroup') %>% 
    html_nodes('option')
  
  #reg expression to pull id from tournament/player links
  grab_id <- "[[:digit:]]+"
  
  #vector of tournament ids
  tournament_id <- tournament_data %>% 
    html_attr("data-url") %>% 
    map_chr(toString) %>%
    map_chr(~ str_extract(.x, pattern = grab_id))
  
  #reg expression(?<=> looks behind >), .* matches everything except new lines, (?=</option) looks ahead to match </option
  grab_tournament_name <- "(?<=>).*(?=</option)" 
  
  #vector of tournament names
  tournament_name <- tournament_data %>% 
    map_chr(toString) %>% 
    map_chr(~ str_extract(.x, grab_tournament_name))
  
  tournaments <- tibble(tournament_id, 
                        tour = tour,
                        tournament_name, 
                        season = season)
  
  #return(tournament_ids)
  return(tournaments)
}

# Example
# tournaments <- get_tournaments(tour = "pga", 2019)

scrape_tournament_locations <- function(tour, season, tournament_id, tournament_name){
  
  url <- paste0('https://www.espn.com/golf/leaderboard/_/tournamentId/', tournament_id, '/season/', season)
  
  webpage <- read_html(url)
  
  course <- webpage %>%
    html_nodes("div.Leaderboard__Course__Location") %>%
    toString() %>%
    str_extract(pattern = "(?<=>).*(?=<!-- --> -)")
  
  location <- webpage %>%
    html_nodes("div.Leaderboard__Course__Location") %>%
    toString() %>%
    str_extract(pattern = "(?<=- <!-- -->).*(?=</div>)")
  
  par <- webpage %>%
    html_nodes("div.Leaderboard__Course__Location__Detail") %>%
    toString() %>%
    str_extract(pattern = "(?<=>Par</span>).*(?=<span class=)")
  
  yards <- webpage %>%
    html_nodes("div.Leaderboard__Course__Location__Detail") %>%
    toString() %>%
    str_extract(pattern = "(?<=Yards</span>).*(?=</div>)")
  
  
  tournaments <- tibble(
    tour            = tour %>% as.character(),
    season          = season %>% as.integer(),
    tournament_id   = tournament_id %>% as.character(),
    tournament_name = tournament_name %>% as.character(),
    par,
    yards,
    course,
    location
  )
  return(tournaments)
  print(season)
  
  Sys.sleep(1)
  
}


#Example running for multiple tournaments
# tournaments <-
#   tournaments %>%
#   future_pmap_dfr(purrr::possibly(scrape_tournament_locations, otherwise = set_names(
#     ., nm = c("tour",
#               "season",
#               "tournament_id",
#               "tournament_name")
#   )))

scrape_hole_description_function <- function(tour, tournament_id) {
  
  # this function is essentially used to scrape the yardage for each hole
  
  # read json
  holes_json <- jsonlite::read_json(paste0("https://site.web.api.espn.com/apis/site/v2/sports/golf/", tour, "/leaderboard/course?region=us&lang=en&event=", tournament_id))
  
  # get course id(s) for tournament
  courseId_list <- 
    holes_json %>% 
    purrr::pluck("courses") %>% 
    map_chr( ~ purrr::pluck(.x, "courseId"))
  
  # get yardage data
  holes_json %>% 
    purrr::pluck("courses") %>% 
    map( ~ purrr::pluck(.x, "holes")) %>% 
    enframe() %>% 
    mutate(course_id = courseId_list) %>% 
    mutate(tournament_id = tournament_id) %>% 
    unnest_longer(value) %>% 
    unnest_wider(value) %>% 
    select(tournament_id, course_id, holeNumber:holeYards) %>% 
    rename(hole = holeNumber,
           yards = holeYards) %>% 
    janitor::clean_names()
  
}

# Example
# tournament_holes <-
#   tournaments_tbl %>% 
#   filter(tour %in% c("pga", "ntw")) %>% 
#   future_pmap(purrr::possibly(scrape_hole_description_function, otherwise = NA)) %>% 
#   enframe() %>% 
#   filter(!is.na(value)) %>% 
#   unnest(value) %>% 
#   select(-name)


scrape_player_ids_tournament <- function(tour, season, tournament_id){
  #funtion returns all player ids for a single tournament
  #enter tournament id and season to return a vector of all player ids in tournament
  #season should be a year (i.e. 2020). The earliest season allowed is 2001
  url <- paste0('https://www.espn.com/golf/leaderboard/_/tournamentId/', tournament_id, '/season/', season)
  
  webpage <- read_html(url)
  
  #reg expression to pull id from tournament/player links
  grab_id <- "[[:digit:]]+"
  
  player_id <- html_nodes(webpage, 'a.AnchorLink.leaderboard_player_name') %>% 
    html_attr("href") %>%
    map_chr(~ str_extract(.x, grab_id))
  
  
  player_ids_tbl <- tibble(player_id,
                        tour = tour,
                        season = season,
                        tournament_id = tournament_id 
                        )
  
  return(player_ids_tbl)
  print(season)
  
  Sys.sleep(1)
}          


# Example        
# player_ids_df <-
#   tournaments %>%
#   select(tour, season, tournament_id) %>%
#   future_pmap_dfr(purrr::possibly(scrape_player_ids_tournament, otherwise = set_names(., nm = c(
#     "tour",
#     "season",
#     "tournament_id"
#   ))))


call_player_api <- function(player_id, tour, season, tournament_id){
  
  #this function returns the api json for a particular player, in a particular tournament, in a particular season
  
  json <- jsonlite::read_json(paste0('https://site.web.api.espn.com/apis/site/v2/sports/golf/', tour, '/leaderboard/', tournament_id,'/playersummary?region=us&lang=en&season=', season, '&player=', player_id))
  
  Sys.sleep(1)
  
  return(json)
}


get_player_tourney_data <- function(player_id, tour, season, tournament_id) {
  
  json <- call_player_api(player_id, tour, season, tournament_id)
  
  player_name <- json %>% purrr::pluck("profile", "displayName")
  dob <- json %>% purrr::pluck("profile", "dateOfBirth")
  birth_place <- json %>% purrr::pluck("profile", "birthPlace")
  hand <- json %>% purrr::pluck("profile", "hand")
  
  
  
  # overall stats for player from tournament
  stats_df <-
    tibble(stats = purrr::pluck(json, "stats")) %>%
    unnest_wider(stats) %>%
    select(-name) %>%
    pivot_wider(names_from = displayName, values_from = displayValue) %>%
    transmute(
      player_id = player_id,
      player_name = player_name,
      dob = dob,
      birth_place = birth_place,
      hand = hand,
      tour = tour,
      season = season,
      tournament_id = tournament_id,
      across(everything())
    ) %>%
    janitor::clean_names()
  
  
  #expected columns for rounds_df prior to adding player_id, tournament_id, season
  colNames <-
    c(
      "period",
      "value",
      "displayValue",
      "linesores",
      "inScore",
      "outScore",
      "courseID",
      "startPosition",
      "currentPosition",
      "movement",
      "teeTime"
    )
  
  addCols <- function(data, colName) {
    add <- colName[!colName %in% names(data)]
    
    if (length(add) != 0) {
      data[add] <- NA
    }
    
    data
  }
  
  # round scores for player from tournament
  rounds_df <-
    tibble(rounds = purrr::pluck(json, "rounds")) %>%
    unnest_wider(rounds) %>%
    addCols(colNames) %>%
    select(
      round = period,
      tot_strokes = value,
      score_to_par = displayValue,
      inScore,
      outScore,
      courseId,
      startPosition,
      endPosition = currentPosition,
      movement,
      teeTime
    ) %>%
    transmute(
      player_id = player_id,
      player_name = player_name,
      tour = tour,
      season = season,
      tournament_id = tournament_id,
      across(everything())
    ) %>%
    janitor::clean_names()
  
  # individual hole scores for player from tournament
  holes_df <-
    tibble(rounds = purrr::pluck(json, "rounds")) %>%
    unnest_wider(rounds) %>%
    select(period, linescores) %>%
    unnest_longer(linescores) %>%
    hoist(linescores,
          hole = "period",
          par = "par",
          score = "value") %>%
    unnest_wider(linescores) %>%
    select(-displayValue) %>%
    unnest_wider(scoreType) %>%
    rename(round = period, score_type = name) %>%
    select(-displayName,-displayValue) %>%
    left_join(select(rounds_df, round, course_id)) %>%
    transmute(
      player_id = player_id,
      player_name = player_name,
      tour = tour,
      season = season,
      tournament_id = tournament_id,
      across(everything())
    ) %>%
    select(player_id:tournament_id, course_id, everything())
  
  df_names <- c("stats", "rounds", "holes")
  df_list <- list(stats_df, rounds_df, holes_df) %>%
    set_names(nm = df_names)
  
  return(df_list)
}

#Example
# list <-
#   player_ids_df %>%
#   head(10) %>%
#   filter(!is.na(player_id)) %>%
#   future_pmap(purrr::possibly(get_player_tourney_data, otherwise = NA))

          
prepare_data_for_db <- function(list){
  
  # formats player data, adds additional variables
  
  # extract round by round data
  rounds <-
    map_df(list, ~ purrr::pluck(.x, "rounds")) %>% 
    separate(
      tee_time,
      into   = c("date", "time"),
      sep    = "T",
      remove = TRUE
    ) %>%
    mutate(
      date          = date %>% ymd(),
      tournament_id = as.character(tournament_id)
    )
  
  # round averages
  round_avgs <- 
    rounds %>%
    group_by(tournament_id, course_id, season, round) %>%
    summarize(field_avg_strokes_rd = mean(tot_strokes)) %>%
    ungroup()
  
  # Join rounds scoring with round avg. Add strokes gained
  round_scoring <- 
    rounds %>%
    left_join(round_avgs,
              everything(),
              by = c("tournament_id",
                     "course_id",
                     "round",
                     "season"
                     )) %>%
    mutate(strokes_gained_rd = field_avg_strokes_rd - tot_strokes) %>%
    select(
      player_id:tournament_id,
      course_id,
      date,
      time,
      round,
      tot_strokes,
      score_to_par,
      field_avg_strokes_rd,
      strokes_gained_rd,
      start_position,
      end_position,
      movement
    )
  
  # extract hole by hole data
  holes <- 
    map_df(list, ~purrr::pluck(.x, "holes")) %>%
    mutate(tournament_id = as.character(tournament_id)) %>% 
    left_join(select(rounds, date, player_id, tournament_id, round), by = c("player_id",
                                                                            "tournament_id",
                                                                            "round")) %>% 
    select(player_id:course_id,
           date,
           everything())
  
  
  # add all DK scoring, fd classic scoring
  hole_scoring <- 
    holes %>%
    filter(!is.na(score)) %>%
    mutate(
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
      )
    )
  
  # hole averages
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
  
  # Join hole scoring with hole avg. Add strokes/score gained
  hole_scores <- 
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
    select(player_id:score_type, field_avg_score, strokes_gained_hole,
           dk_pts_classic, field_dk_classic_avg, dk_pts_gained_classic,
           dk_pts_wkend, field_dk_wkend_avg, dk_pts_gained_wkend,
           dk_pts_sd, field_dk_sd_avg, dk_pts_gained_sd,
           fd_pts_classic, field_fd_classic_avg, fd_pts_gained_classic)
  
  # extract tournament statistics data for each player
  player_stats <- 
    map_df(list, ~ purrr::pluck(.x, "stats")) %>%
    mutate(tournament_id = as.character(tournament_id),
           player_id     = as.character(player_id),
           dob           = dob %>% str_extract(pattern = "^([^T])+") %>% ymd()) %>%
    left_join(
      select(holes, player_id, tournament_id, round, hole, date) %>%
        filter(round  == 1 &
                 hole == 1),
      by = c("player_id"     = "player_id",
             "tournament_id" = "tournament_id")
    ) %>%
    select(-round,-hole) %>% 
    mutate(across(.cols = c(driving_distance:penalties),
                  .fns = as.numeric)) %>% 
    select(date, everything())
  
  field_stats <-
    player_stats %>% 
    mutate(across(.cols = c(driving_distance:penalties),
                  .fns = as.numeric)) %>%
    group_by(tournament_id, season) %>%
    summarise(
      across(.cols = c(driving_distance, driving_accuracy, gir, putts_per_gir),
             .fns = mean,
             .names = "field_{col}_avg")
    ) %>%
    ungroup()

  stats <-
    player_stats %>%
    left_join(select(field_stats, everything()),
              by = c("tournament_id", "season")) %>%
    mutate(
      dd_over_field = driving_distance - field_driving_distance_avg,
      da_over_field = driving_accuracy - field_driving_accuracy_avg,
      gir_over_field = gir - field_gir_avg,
      putts_per_gir_over_field = putts_per_gir - field_putts_per_gir_avg
    ) %>%
    select(
      date:driving_distance,
      field_driving_distance_avg,
      dd_over_field,
      driving_accuracy,
      field_driving_accuracy_avg,
      da_over_field,
      gir,
      field_gir_avg,
      gir_over_field,
      putts_per_gir,
      field_putts_per_gir_avg,
      putts_per_gir_over_field,
      everything()
    ) %>%
    mutate(across(.cols = c(driving_distance:possible_gir), ~ na_if(., 0))) %>%
    mutate(driving_distance = ifelse(driving_distance < 150, NA_real_, driving_distance)) %>%
    mutate(dd_over_field = ifelse(driving_distance %>% is.na(), NA_real_, dd_over_field))
  
  df_names <- c("round_scoring", "hole_scores", "stats")
  list_of_dfs <- list(round_scoring, hole_scores, stats) %>% 
    set_names(nm = df_names)
  
  return(list_of_dfs)
  
}        

# Example       
# prepare_data_for_db(list)
          
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
          player_name == "Matt Fitzpatrick" ~ "Matthew Fitzpatrick",
          TRUE ~ player_name
        )
    )
  
  return(data_cleaned)
  
}     
          