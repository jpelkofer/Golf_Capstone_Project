# Load Libs, source scripts, set cores for parallel processing ----

library(DataExplorer)

source(file = "database/scripts/espn_data_scraping_and_wrangling_functions.R")

plan(multisession, workers = 4)


# Get PGA data from espn.com ----
pga_tbl <- tibble(tour = "pga",
       season = c(2016:2021))

#* Get tournaments for years 2016 through 2021 ----
tournaments <- 
  pga_tbl %>% 
  future_pmap_dfr(purrr::possibly(get_tournaments, otherwise = NA)) %>% 
  future_pmap_dfr(purrr::possibly(scrape_tournament_locations, otherwise = set_names(., nm = c("tour",
                                                                                       "season" %>% as.integer(),
                                                                                       "tournament_id",
                                                                                       "tournament_name"))))
# filter tournament that hasn't happened yet
tournaments <- 
  tournaments %>% 
  filter(!tournament_id %in% c("401242999"))

#* Get player data for years 2016 through 2021 (this will take awhile) ----
pga_player_data <-
  tournaments %>%
  select(tour, season, tournament_id) %>%
  future_pmap_dfr(purrr::possibly(scrape_player_ids_tournament, otherwise = set_names(
    ., nm = c("tour",
              "season" %>% as.integer(),
              "tournament_id")
  ))) %>%
  filter(!is.na(player_id)) %>%
  future_pmap(purrr::possibly(get_player_tourney_data, otherwise = NA)) %>%
  prepare_data_for_db()    

# Save
# write_rds(pga_player_data, "pga_player_data.rds")

# Examine Data ----
rounds <-
  pga_player_data %>% purrr::pluck("round_scoring") %>%
  filter(tot_strokes > 0) # if tot_strokes is 0, player did not play round

holes <- pga_player_data %>% purrr::pluck("hole_scores")
stats <- pga_player_data %>% purrr::pluck("stats")

#* Explore Missing Data ----

# Small portion of date variables missing, which is something I want to fix. Not concerned with other missing vars (end_position, start_position, movement)
rounds %>% plot_missing()
# Again small portion of date var missing, try and fix
holes %>% plot_missing()
# Again small portion of date var missing, try and fix
stats %>% plot_missing()

#* Check for date issues and duplicate data ----

#rounds
check_rounds_data(rounds)

#holes
check_holes_data(holes)

#check duplicates
# these were actually playoff holes, adjusting to round 5 for clarity
holes_fixed <- 
  holes %>%
  mutate(round = case_when(
    player_id %in% c("1676") & tournament_id %in% c("2491") & round == 4 & hole == 18 ~ as.integer(5),
    player_id %in% c("3599") & tournament_id %in% c("2491") & round == 4 & hole == 18 ~ as.integer(5),
    TRUE ~ round
  ))

#re check
check_holes_data(holes_fixed)

#stats
check_stats_data(stats) # missing dates are WD's, ok leaving since NA dates will get filtered out

# Initial Database Build ----
#* Connect to Postgres DB ----
library(DBI)
library(RPostgres)

con <- dbConnect(drv = RPostgres::Postgres(), 
                 user = "postgres", 
                 password = keyring::key_get("postgres_db", "postgres"), 
                 dbname = "golf_db")

#* Write tables ----
rounds %>% 
  DBI::dbWriteTable(conn = con, "rounds_tbl", ., field.types = c(date = "date"))

holes_fixed %>% 
  DBI::dbWriteTable(conn = con, "holes_tbl", ., field.types = c(date = "date"))

stats %>% 
  DBI::dbWriteTable(conn = con, "stats_tbl", ., field.types = c(date = "date"))

tournaments %>% 
  mutate(
         par = as.numeric(par),
         yards = as.numeric(yards)
         ) %>% 
  DBI::dbWriteTable(conn = con, "tournaments_tbl", .)

#* check tables were written ----
tbl(con, "rounds_tbl") # good
tbl(con, "holes_tbl") # good
tbl(con, "stats_tbl") # good
tbl(con, "tournaments_tbl") # good




# Get ntw data from espn.com ----
ntw_tbl <- tibble(tour = "ntw",
                  season = c(2016:2020))

# no data for 2017 on espn.com
tournaments <- 
  ntw_tbl %>% 
  future_pmap_dfr(purrr::possibly(get_tournaments, otherwise = NA)) %>% 
  future_pmap_dfr(purrr::possibly(scrape_tournament_locations, otherwise = set_names(., nm = c("tour",
                                                                                               "season" %>% as.integer(),
                                                                                               "tournament_id",
                                                                                               "tournament_name" ))))

#* Get player data for years 2016 through 2020 (this will take awhile) ----
ntw_player_data <-
  tournaments %>%
  select(tour, season, tournament_id) %>%
  future_pmap_dfr(purrr::possibly(scrape_player_ids_tournament, otherwise = set_names(
    ., nm = c("tour",
              "season" %>% as.integer(),
              "tournament_id")
  ))) %>%
  filter(!is.na(player_id)) %>%
  future_pmap(purrr::possibly(get_player_tourney_data, otherwise = NA)) %>%
  prepare_data_for_db()  

# Examine Data ----
rounds <-
  ntw_player_data %>% purrr::pluck("round_scoring") %>%
  filter(tot_strokes > 0) # if tot_strokes is 0, player did not play round

holes <- ntw_player_data %>% purrr::pluck("hole_scores")
stats <- ntw_player_data %>% purrr::pluck("stats")

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

# Append data to db tables

DBI::dbAppendTable(conn = con, "rounds_tbl", rounds)
DBI::dbAppendTable(conn = con, "holes_tbl", holes)
DBI::dbAppendTable(conn = con, "stats_tbl", stats)

tournaments %>%
  mutate(par = as.numeric(par),
         yards = as.numeric(yards)) %>%
  DBI::dbAppendTable(conn = con, "tournaments_tbl", .)

# check tables appended ----
tbl(con, "rounds_tbl") %>% 
  filter(tour %in% c("ntw"))

tbl(con, "holes_tbl") %>% 
  filter(tour %in% c("ntw"))

tbl(con, "stats_tbl") %>% 
  filter(tour %in% c("ntw"))

tbl(con, "tournaments_tbl") %>% 
  filter(tour %in% c("ntw"))

dbDisconnect(con)

# add euro/ct tournaments to database
euro_tournaments_tbl <- 
  read_csv("database/euro_tour_tournaments.csv")

DBI::dbAppendTable(conn = con, "tournaments_tbl", euro_tournaments_tbl)

# Fixing holes_tbl to add yardage ----

tournaments_tbl <- tbl(con, "tournaments_tbl") %>% collect()

# get pga and ntw holes
tournament_holes <-
  tournaments_tbl %>%
  filter(tour %in% c("pga", "ntw")) %>%
  select(tour, tournament_id) %>% 
  future_pmap(purrr::possibly(scrape_hole_description_function, otherwise = NA)) %>%
  enframe() %>%
  filter(!is.na(value)) %>%
  unnest(value) %>%
  select(-name) %>% 
  mutate(course_id = course_id %>% as.numeric())

source(file = "database/scripts/euro_tour_data_scraping_and_wrangling_functions.R")

# euro portion of getting holes, euro tour gives data for each round
euro_rounds <- 
  rep(1, 319) %>% 
  append(rep(2,319)) %>% 
  append(rep(3,319)) %>%
  append(rep(4,319))


euro_tournaments_rounds <- 
  tournaments_tbl %>%
  filter(tour %in% c("euro", "ct")) %>%
  bind_rows(tournaments_tbl %>%
              filter(tour %in% c("euro", "ct"))) %>% 
  bind_rows(tournaments_tbl %>%
              filter(tour %in% c("euro", "ct"))) %>%
  bind_rows(tournaments_tbl %>%
              filter(tour %in% c("euro", "ct"))) %>%
  mutate(round = euro_rounds)

# get euro and ct holes
euro_tournament_holes <- 
  euro_tournaments_rounds %>%
  select(tournament_id, round) %>% 
  future_pmap(purrr::possibly(scrape_euro_hole_description_function, otherwise = NA)) %>%
  enframe() %>%
  filter(!is.na(value)) %>%
  unnest(value) %>%
  select(-name) %>% 
  mutate(course_id = course_id %>% as.numeric())

holes_tbl <- 
  tbl(con, "holes_tbl") %>% 
  collect()


# Fix PGA and NTW holes
pga_holes_tbl <-
  holes_tbl %>%
  filter(tour %in% c("pga", "ntw")) %>%
  left_join(
    tournament_holes,
    by = c("tournament_id",
           "course_id",
           "hole")
  ) %>%
  select(player_id:par, yards, score:fd_pts_gained_classic)

plot_missing(pga_holes_tbl)

# After examining missing values it is determined the ones that are missing is because data is not available
pga_holes_tbl %>% 
  filter(is.na(yards)) %>% 
  View()

# Fix Euro and CT holes
euro_holes_tbl <-
  holes_tbl %>%
  filter(tour %in% c("euro", "ct")) %>%
  left_join(
    select(euro_tournament_holes, -par),
    by = c("tournament_id",
           "course_id",
           "round",
           "hole")
  ) %>%
  select(player_id:par, yards, score:fd_pts_gained_classic)

plot_missing(euro_holes_tbl)

# combine fixed holes tbl
updated_holes_tbl <- 
  pga_holes_tbl %>% 
  bind_rows(euro_holes_tbl)

plot_missing(updated_holes_tbl)

# Update DB
updated_holes_tbl %>% 
  DBI::dbWriteTable(conn = con, "holes_tbl", ., field.types = c(date = "date"), overwrite = TRUE)


