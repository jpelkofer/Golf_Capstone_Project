# Load Libs, source scripts, set cores for parallel processing ----

library(DataExplorer)

source(file = "database/scripts/espn_data_scraping_and_wrangling_functions.R")

plan(multisession, workers = 4)

# Updates for PGA TOUR ----
# Get Tournaments for season ----
tournament <- get_tournaments("pga", 2021)

# Filter for tournament that needs to be added to DB ----
tournament <- 
  tournament %>% 
  filter(tournament_id %in% c("401243009")) %>% 
  pmap_dfr(scrape_tournament_locations)

#* Get player data for tournament ----
pga_player_data <-
  tournament %>%
  select(tour, season, tournament_id) %>%
  future_pmap_dfr(purrr::possibly(scrape_player_ids_tournament, otherwise = set_names(
    ., nm = c("tour",
              "season" %>% as.integer(),
              "tournament_id")
  ))) %>%
  filter(!is.na(player_id)) %>%
  future_pmap(purrr::possibly(get_player_tourney_data, otherwise = NA)) %>%
  prepare_data_for_db()

#* Get hole descriptions from tournament ----
hole_descriptions <- scrape_hole_description_function("pga", "401056526") %>% mutate(tournament_id = "401243009")


# Examine Data ----
rounds <-
  pga_player_data %>% purrr::pluck("round_scoring") %>%
  filter(tot_strokes > 0) # if tot_strokes is 0, player did not play round

holes <- 
  pga_player_data %>% purrr::pluck("hole_scores") %>%
  left_join(
    select(hole_descriptions, everything()) %>% 
      mutate(tournament_id = tournament_id %>% as.character(),
             course_id = course_id %>% as.numeric()),
    by = c("tournament_id",
           "course_id",
           "hole")) %>%
  select(player_id:par, yards, score:fd_pts_gained_classic)

stats <- pga_player_data %>% purrr::pluck("stats")


#* Explore Missing Data ----

# Small portion of date variables missing, which is something I want to fix. Not concerned with other missing vars (end_position, start_position, movement)
rounds %>% plot_missing()
# Again small portion of data var missing, try and fix
holes %>% plot_missing()
# Again small portion of data var missing, try and fix
stats %>% plot_missing()

#* Check for date issues and duplicate data ----

#rounds
check_rounds_data(rounds)

#holes
check_holes_data(holes)

#stats
check_stats_data(stats)

# Fix Names
rounds <- 
  rounds %>% 
  fix_euro_names()

holes <- 
  holes %>% 
  fix_euro_names()

stats <- 
  stats %>% 
  fix_euro_names()

# Connect to Postgres DB ----
library(DBI)
library(RPostgres)

con <- dbConnect(drv = RPostgres::Postgres(), 
                 user = "postgres", 
                 password = keyring::key_get("postgres_db", "postgres"), 
                 dbname = "golf_db")

# Append data to db tables ----

# check if adding a new name
players <- 
  tbl(con, "rounds_tbl") %>%
  select(player_name, player_id) %>% 
  distinct() %>% 
  collect()

rounds %>% 
  select(player_name) %>% 
  distinct() %>% 
  left_join(players) %>% 
  filter(player_id %>% is.na())

tbl(con, "rounds_tbl") %>%
  select(player_name) %>%
  filter(player_name %>% str_detect("Hart")) %>%
  distinct()

DBI::dbAppendTable(conn = con, "rounds_tbl", rounds)
DBI::dbAppendTable(conn = con, "holes_tbl", holes)
DBI::dbAppendTable(conn = con, "stats_tbl", stats)
DBI::dbAppendTable(conn = con, "tournaments_tbl", tournament)

# Updates for Korn Ferry TOUR ----
# Get Tournaments for season ----
tournament <- get_tournaments("ntw", 2021)

# Filter for tournament that needs to be added to DB ----
tournament <- 
  tournament %>% 
  filter(tournament_id %in% c("401262688")) %>% 
  pmap_dfr(scrape_tournament_locations)

#* Get player data for tournament ----
ntw_player_data <-
  tournament %>%
  select(tour, season, tournament_id) %>%
  future_pmap_dfr(purrr::possibly(scrape_player_ids_tournament, otherwise = set_names(
    ., nm = c("tour",
              "season" %>% as.integer(),
              "tournament_id")
  ))) %>%
  filter(!is.na(player_id)) %>%
  future_pmap(purrr::possibly(get_player_tourney_data, otherwise = NA)) %>%
  prepare_data_for_db() 

#* Get hole descriptions from tournament ----
hole_descriptions <- scrape_hole_description_function("ntw", "401262688")

# Examine Data ----
rounds <-
  ntw_player_data %>% purrr::pluck("round_scoring") %>%
  filter(tot_strokes > 0) # if tot_strokes is 0, player did not play round

holes <- 
  ntw_player_data %>% purrr::pluck("hole_scores") %>%
  # left_join(
  #   select(hole_descriptions, everything()) %>% 
  #     mutate(tournament_id = tournament_id %>% as.character(),
  #            course_id = course_id %>% as.numeric()),
  #   by = c("tournament_id",
  #          "course_id",
  #          "hole")) %>%
  # select(player_id:par, yards, score:fd_pts_gained_classic) 
  select(player_id:par, score:fd_pts_gained_classic)


stats <- ntw_player_data %>% purrr::pluck("stats")

#* Explore Missing Data ----


rounds %>% plot_missing()

holes %>% plot_missing()

stats %>% plot_missing()

#* Check for date issues and duplicate data ----

#rounds
check_rounds_data(rounds)

#holes
check_holes_data(holes)

#stats
check_stats_data(stats)

# Append data to db tables ----

DBI::dbAppendTable(conn = con, "rounds_tbl", rounds)
DBI::dbAppendTable(conn = con, "holes_tbl", holes)
DBI::dbAppendTable(conn = con, "stats_tbl", stats)
DBI::dbAppendTable(conn = con, "tournaments_tbl", tournament)

dbDisconnect(con)
