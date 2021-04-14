# Load Libs, source scripts, set cores for parallel processing ----

library(DataExplorer)

source(file = "database/scripts/euro_tour_data_scraping_and_wrangling_functions.R")

plan(multisession, workers = 4)

# Pull all tournaments ----
tournaments_2021 <- get_tournaments("euro", 2021) %>% as_tibble()

# Filter for tournament needed ----
tournament <- 
  tournaments_2021 %>% 
  filter(tournament_name %in% c("Kenya Savannah Classic supported by Absa"))

#* Get tournament id ----
tournament_id <-
  tournament %>%
  pull(tournament_link) %>%
  future_map_dfr(get_tournament_id_and_dates)

# Construct tournament dataframe ----
tournaments_df <-
  tournament_id %>%
  bind_cols(tournament) %>% 
  separate(tournament_course_location, into = c("course", "location"), sep = ",", extra = "merge" ) %>%
  select(tour, season, everything())

# Get Rounds Data ----
rounds_data <-
  tournaments_df %>%
  select(tour:tournament_id) %>%
  distinct() %>% 
  future_pmap(purrr::possibly(get_rounds_data, otherwise = NA))

rounds_data <- 
  rounds_data %>% 
  enframe() %>% 
  unnest(value) %>% 
  select(-name) %>% 
  drop_na()

rounds_data %>% plot_missing()

rounds_data %>% 
  filter(tot_strokes %>% is.na())

rounds_data <- 
  rounds_data %>% 
  drop_na() # these players withdrew from tournament

# Get Holes Data ----
hole_description_data <- 
  tournaments_df %>% 
  select(tournament_id, round) %>% 
  future_pmap(purrr::possibly(scrape_euro_hole_description_function, otherwise = NA)) %>% 
  enframe() %>% 
  unnest(value)

holes_data <-
  rounds_data %>%
  select(tour, season, tournament_id, player_id, player_name) %>%
  distinct() %>%
  future_pmap(purrr::possibly(get_holes_data, otherwise = NA))

holes_data <- 
  holes_data %>% 
  enframe() %>% 
  unnest(value) %>% 
  select(-name) %>% 
  left_join(select(hole_description_data, tournament_id:yards), by = c("tournament_id", "course_id", "round", "hole"))


# Get Stats Data ----
stats_data <-
  rounds_data %>%
  select(tour, season, tournament_id, player_id, player_name, round) %>%
  group_by(tour, season, tournament_id, player_id, player_name) %>%
  summarise(round = max(round)) %>%
  ungroup() %>%
  future_pmap(purrr::possibly(get_stats_data, otherwise = NA)) %>%
  enframe() %>%
  unnest_wider(value) %>%
  select(-c(name))

stats_data <- 
  stats_data %>% 
  drop_na()

stats_data %>% 
  plot_missing()

# Prep Data for DB ----

euro_tour_player_data <- prepare_data_for_db(tournament_id, rounds_data, holes_data, stats_data)

# Examine Data ----
rounds <-euro_tour_player_data %>% purrr::pluck("round_scoring")
holes <- euro_tour_player_data %>% purrr::pluck("hole_scores")
stats <- euro_tour_player_data %>% purrr::pluck("stats")

#* Explore Missing Data ----

rounds %>% plot_missing() #euro tour does not give the data that is missing

holes %>% plot_missing()

stats %>% plot_missing() #euro tour does not give the data that is missing

#* Check for date issues and duplicate data ----

#rounds
check_rounds_data(rounds)

#holes
check_holes_data(holes)

#stats
check_stats_data(stats)

#* Connect to Postgres DB ----
library(DBI)
library(RPostgres)

con <- dbConnect(drv = RPostgres::Postgres(), 
                 user = "postgres", 
                 password = keyring::key_get("postgres_db", "postgres"), 
                 dbname = "golf_db")

# Append data to db tables ----
DBI::dbAppendTable(conn = con, "rounds_tbl", rounds)
DBI::dbAppendTable(conn = con, "holes_tbl", holes)
DBI::dbAppendTable(conn = con, "stats_tbl", stats)

# do tournament insert manually