# Load Libs, source scripts, set cores for parallel processing ----

library(DataExplorer)

source(file = "scripts/euro_tour_data_scraping_and_wrangling_functions.R")

plan(multisession, workers = 4)

# Get euro data from europeantour.com ----

#* Get tournaments for years 2016 through 2021 ----
# will need to go through individually year by year, euro tour includes some pga tournaments that we already have data for
tournaments_2021 <- get_tournaments("euro", 2021) %>% as_tibble()
tournaments_2020 <- get_tournaments("euro", 2020) %>% as_tibble()
tournaments_2019 <- get_tournaments("euro", 2019) %>% as_tibble()
tournaments_2018 <- get_tournaments("euro", 2018) %>% as_tibble()
tournaments_2017 <- get_tournaments("euro", 2017) %>% as_tibble()
tournaments_2016 <- get_tournaments("euro", 2016) %>% as_tibble()

#* 2021 ----
# only three tournaments played so far in 2021 season
tournaments_2021_df <- 
  tournaments_2021 %>% 
  filter(tournament_name %in% c("Abu Dhabi HSBC Championship",
                                "Omega Dubai Desert Classic",
                                "Saudi International powered by SoftBank Investment Advisers"))

#* 2020 ----
tournaments_2020_df <- 
  tournaments_2020 %>% 
  filter(!tournament_name %in% c("THE MASTERS",
                                 "U.S. OPEN",
                                 "US PGA CHAMPIONSHIP",
                                 "WGC - FedEx St Jude Invitational",
                                 "WGC - Mexico Championship"
                                 ))

#* 2019 ----
tournaments_2019_df <-
tournaments_2019 %>%
  filter(
    !tournament_name %in% c(
      "148TH OPEN CHAMPIONSHIP",
      "THE MASTERS",
      "U.S. OPEN",
      "US PGA CHAMPIONSHIP",
      "WGC-FedEx St. Jude Invitational",
      "WGC-Mexico Championship",
      "WGC-HSBC Champions",
      "WGC-Dell Technologies Match Play"
    )
  ) %>% 
  filter(!tournament_course_location %in% c("Jumeirah Golf Estates, Dubai, UAE",
                                            "Gary Player CC, Sun City, South Africa"))

#* 2018 ----
tournaments_2018_df <-
  tournaments_2018 %>%
  filter(
    !tournament_name %in% c(
      "147TH OPEN CHAMPIONSHIP",
      "THE MASTERS",
      "U.S. OPEN",
      "US PGA CHAMPIONSHIP",
      "WGC-Bridgestone Invitational",
      "WGC - Mexico Championship",
      "WGC - HSBC Champions",
      "WGC - Dell Technologies Match Play",
      "The 2018 Ryder Cup"
    )
  ) %>% 
  filter(!tournament_course_location %in% c("The Metropolitan Golf Club, Melbourne, Australia",
                                            "Jumeirah Golf Estates, Dubai, UAE"))

#* 2017 ----
tournaments_2017_df <-
  tournaments_2017 %>%
  filter(
    !tournament_name %in% c(
      "146TH OPEN CHAMPIONSHIP",
      "Alfred Dunhill Championship", # data missing for this tournament in 2017
      "MASTERS TOURNAMENT",
      "U.S. OPEN",
      "US PGA CHAMPIONSHIP",
      "WGC-Bridgestone Invitational",
      "WGC-Mexico Championship",
      "WGC-HSBC Champions",
      "WGC-Dell Technologies Match Play",
      "GolfSixes"
    )
  ) %>% 
  filter(!tournament_course_location %in% c("Gary Player CC, Sun City, South Africa",
                                            "Jumeirah Golf Estates, Dubai, UAE"))

#* 2016 ----
tournaments_2016_df <-
  tournaments_2016 %>%
  filter(
    !tournament_name %in% c(
      "145TH OPEN CHAMPIONSHIP",
      "MASTERS TOURNAMENT",
      "U.S. OPEN",
      "US PGA CHAMPIONSHIP",
      "WGC-Cadillac Championship",
      "WGC-Mexico Championship",
      "WGC-HSBC Champions",
      "WGC-Dell Match Play",
      "The 2016 Ryder Cup"
    )
  ) %>% 
  filter(!tournament_course_location %in% c("Gary Player CC, Sun City, South Africa",
                                            "Jumeirah Golf Estates, Dubai, UAE"))


#* Get tournament ids ----
tournament_ids_2021 <-
  tournaments_2021_df %>%
  pull(tournament_link) %>%
  future_map_dfr(get_tournament_id_and_dates)

tournament_ids_2020 <-
  tournaments_2020_df %>%
  pull(tournament_link) %>%
  future_map_dfr(get_tournament_id_and_dates)

tournament_ids_2019 <-
  tournaments_2019_df %>%
  pull(tournament_link) %>%
  future_map_dfr(get_tournament_id_and_dates)

tournament_ids_2018 <-
  tournaments_2018_df %>%
  pull(tournament_link) %>%
  future_map_dfr(get_tournament_id_and_dates)

tournament_ids_2017 <-
  tournaments_2017_df %>%
  pull(tournament_link) %>%
  future_map_dfr(get_tournament_id_and_dates)

tournament_ids_2016 <-
  tournaments_2016_df %>%
  pull(tournament_link) %>%
  future_map_dfr(get_tournament_id_and_dates)

#* Bind dataframes together ----
tournament_ids <- 
  tournament_ids_2021 %>% 
  bind_rows(tournament_ids_2020) %>% 
  bind_rows(tournament_ids_2019) %>%
  bind_rows(tournament_ids_2018) %>%
  bind_rows(tournament_ids_2017) %>%
  bind_rows(tournament_ids_2016)

# Construct tournament dataframe ----
tournaments_df <-
  tournament_ids %>%
  distinct(tournament_id) %>%
  bind_cols(select(tournaments, everything()) %>% 
              filter(!tournament_link %in% c("/european-tour/alfred-dunhill-championship-2016/", "/european-tour/golfsixes-2017/"))) %>%
  separate(tournament_course_location, into = c("course", "location"), sep = ",", extra = "merge" ) %>%
  select(tour, season, everything())

# Get Rounds Data ----
rounds_data <-
  tournaments_df %>%
  select(tour:tournament_id) %>%
  future_pmap(purrr::possibly(get_rounds_data, otherwise = NA))

rounds_data <- 
  rounds_data %>% 
  enframe() %>% 
  unnest(value) %>% 
  select(-name, -value) %>% 
  drop_na()

rounds_data %>% plot_missing()

rounds_data %>% 
  filter(tot_strokes %>% is.na())

rounds_data <- 
  rounds_data %>% 
  drop_na() # these players withdrew from tournament

# Get Holes Data ----
holes_data <-
  rounds_data %>%
  select(tour, season, tournament_id, player_id, player_name) %>%
  distinct() %>%
  future_pmap(purrr::possibly(get_holes_data, otherwise = NA))

holes_data <- 
  holes_data %>% 
  enframe() %>% 
  unnest(value) %>% 
  select(-name, -value)

holes_data %>% 
  plot_missing()


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
  select(-c(name, `...1`))

stats_data <- 
  stats_data %>% 
  drop_na()

# Prep Data for DB ----

euro_tour_player_data <- prepare_data_for_db(tournament_ids, rounds_data, holes_data, stats_data)


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

write_csv(tournaments_df, "euro_tour_tournaments.csv")

# Get challenge tour data from europeantour.com ----

#* Get tournaments for years 2016 through 2020 ----
# will need to go through individually year by year, euro tour includes some pga tournaments that we already have data for
tournaments_2020 <- get_tournaments("ct", 2020) %>% as_tibble()
tournaments_2019 <- get_tournaments("ct", 2019) %>% as_tibble()
tournaments_2018 <- get_tournaments("ct", 2018) %>% as_tibble()
tournaments_2017 <- get_tournaments("ct", 2017) %>% as_tibble()
tournaments_2016 <- get_tournaments("ct", 2016) %>% as_tibble()

#* 2020 ----
tournaments_2020_df <- 
  tournaments_2020 %>% 
  mutate(tournament_course_location = case_when(
    tournament_name == "Limpopo Championship" ~ "Euphoria GC, Modimolle, South Africa",
    tournament_name == "RAM Cape Town Open" ~ "Royal Cape GC, Cape Town, South Africa",
    tournament_name == "Dimension Data Pro-Am" ~ "Fancourt Golf Estate, George, South Africa",
    tournament_name == "Austrian Open" ~ "Diamond CC, Atzenbrugg, near Vienna, Austria",
    tournament_name == "Euram Bank Open" ~ "GC Adamstal, Ramsau, Austria",
    tournament_name == "Northern Ireland Open supported by The R&A" ~ "Galgorm Castle Golf Club, Ballymena, Northern Ireland",
    tournament_name == "Open de Portugal at Royal Óbidos" ~ "Royal Óbidos Spa & Golf Resort, Vau Óbidos, Portugal",
    tournament_name == "Italian Challenge Open Eneos Motor Oil" ~ "Golf Club Castelconturbia, Agrate Conturbia, Italy",
    tournament_name == "Andalucía Challenge de España" ~ "Iberostar Real Club de Golf Novo Sancti Petri, Cadiz, Spain",
    tournament_name == "Andalucía Challenge de Cádiz" ~ "Iberostar Real Club de Golf Novo Sancti Petri, Cadiz, Spain",
    tournament_name == "Challenge Tour Grand Final" ~ "T-Golf & Country Club, Mallorca, Baleares, Spain",
    TRUE ~ tournament_course_location
  )) %>% 
  distinct()

#* 2019 ----
tournaments_2019_df <- 
  tournaments_2019 %>% 
  filter(!tournament_name %in% c("U.S. OPEN",
                                "148TH OPEN CHAMPIONSHIP",
                                "Andalucia - Costa del Sol Match Play 9")) %>% 
  filter(!tournament_course_location %in% c("Club de Golf Alcanada, Port d'Alcúdia, Mallorca, Spain")) %>% 
  mutate(tournament_course_location = case_when(
    tournament_name == "Italian Challenge Open Eneos Motor Oil" ~ "Terre Dei Consoli Golf Club, Monterosi, Italy",
    tournament_name == "D+D REAL Slovakia Challenge" ~ "Penati Golf Resort, Senica, Slovakia",
    tournament_name == "Le Vaudreuil Golf Challenge" ~ "Golf PGA France du Vaudreuil, Le Vaudreuil, France",
    tournament_name == "Euram Bank Open" ~ "GC Adamstal, Ramsau, Austria",
    tournament_name == "Vierumäki Finnish Challenge" ~ "Vierumäki Resort, Vierumäki, Finland",
    tournament_name == "Made in Denmark Challenge - Presented by FREJA" ~ "Silkeborg Ry Golfklub (Ry Course), Skanderborg, Denmark",
    tournament_name == "ISPS Handa World Invitational Men | Women, Presented by Modest! Golf Management
" ~ "Galgorm Castle Golf Club, Ballymena, Northern Ireland",
    tournament_name == "Rolex Trophy" ~ "Golf Club de Genève, Genève, Switzerland",
    tournament_name == "KPMG Trophy" ~ "Millennium Golf, Paal, Beringen, Belgium",
    tournament_name == "Open de Bretagne" ~ "Golf Bluegreen de Pléneuf Val André, Pléneuf, France",
    tournament_name == "57º OPEN DE PORTUGAL @ MORGADO GOLF RESORT" ~ "Morgado G&CC, Portimao, Portugal",
    tournament_name == "Hopps Open de Provence" ~ "Golf International de Pont Royal, Mallemort, France",
    tournament_name == "Lalla Aïcha Challenge Tour" ~ "Royal Golf Dar Es Salam, Rabat, Morocco",
    tournament_name == "Stone Irish Challenge" ~ "Headfort Golf Club, Kells, Co. Meath, Ireland",
    tournament_name == "Hainan Open" ~ "Sanya Luhuitou GC, Donghai Bay, Sanya, Hainan Island, China",
    tournament_name == "Foshan Open" ~ "Foshan GC, Guangdong, China",
    tournament_name == "Challenge Tour Grand Final" ~ "Challenge Tour Grand Final",
    TRUE ~ tournament_course_location
  )) %>% 
  distinct()

#* 2018 ----
tournaments_2018_df <- 
  tournaments_2018 %>% 
  filter(!tournament_name %in% c("U.S. OPEN",
                                 "147TH OPEN CHAMPIONSHIP",
                                 "Andalucia - Costa del Sol Match Play 9")) %>% 
  distinct()

#* 2017 ----
tournaments_2017_df <- 
  tournaments_2017 %>% 
  filter(!tournament_name %in% c("U.S. OPEN",
                                 "146TH OPEN CHAMPIONSHIP",
                                 "Andalucia - Costa del Sol Match Play 9")) %>% 
  distinct()

#* 2016 ----
tournaments_2016_df <- 
  tournaments_2016 %>% 
  distinct()

#* Bind Rows ----
tournaments <- 
  tournaments_2020_df %>% 
  bind_rows(tournaments_2019_df) %>% 
  bind_rows(tournaments_2018_df) %>% 
  bind_rows(tournaments_2017_df) %>% 
  bind_rows(tournaments_2016_df)

#* Get tournament ids ----

tournament_ids_2020 <-
  tournaments_2020_df %>%
  pull(tournament_link) %>%
  future_map_dfr(get_tournament_id_and_dates)

tournament_ids_2019 <-
  tournaments_2019_df %>%
  pull(tournament_link) %>%
  future_map_dfr(get_tournament_id_and_dates)

tournament_ids_2018 <-
  tournaments_2018_df %>%
  pull(tournament_link) %>%
  future_map_dfr(get_tournament_id_and_dates)

tournament_ids_2017 <-
  tournaments_2017_df %>%
  pull(tournament_link) %>%
  future_map_dfr(get_tournament_id_and_dates)

tournament_ids_2016 <-
  tournaments_2016_df %>%
  pull(tournament_link) %>%
  future_map_dfr(get_tournament_id_and_dates)

#* Bind dataframes together ----
tournament_ids <- 
  tournament_ids_2020 %>% 
  bind_rows(tournament_ids_2019) %>%
  bind_rows(tournament_ids_2018) %>%
  bind_rows(tournament_ids_2017) %>%
  bind_rows(tournament_ids_2016)

# Construct tournament dataframe ----
tournaments_df <-
  tournament_ids %>%
  distinct(tournament_id) %>%
  bind_cols(select(tournaments, everything())) %>%
  separate(tournament_course_location, into = c("course", "location"), sep = ",", extra = "merge" ) %>%
  select(tour, season, everything())

# Get Rounds Data ----
rounds_data <-
  tournaments_df %>%
  select(tour:tournament_id) %>%
  future_pmap(purrr::possibly(get_rounds_data, otherwise = NA))

rounds_data <- 
  rounds_data %>% 
  enframe() %>% 
  unnest(value) %>% 
  select(-name)

rounds_data %>% plot_missing()

rounds_data %>% 
  filter(tot_strokes %>% is.na())

rounds_data <- 
  rounds_data %>% 
  drop_na() # these players withdrew from tournament

# Get Holes Data ----
holes_data <-
  rounds_data %>%
  select(tour, season, tournament_id, player_id, player_name) %>%
  distinct() %>%
  future_pmap(purrr::possibly(get_holes_data, otherwise = NA))

holes_data <- 
  holes_data %>% 
  enframe() %>% 
  unnest(value) %>% 
  select(-name, -value)

holes_data %>% 
  plot_missing()

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
  select(-c(name, `...1`))

# not alot of stats data available for challenge tour, probably not going to add this to db
stats_data %>% plot_missing()

# Prep Data for DB ----

challenge_tour_player_data <- prepare_data_for_db(tournament_ids, rounds_data, holes_data, stats_data)

# Examine Data ----
rounds <-challenge_tour_player_data %>% purrr::pluck("round_scoring")
holes <- challenge_tour_player_data %>% purrr::pluck("hole_scores")
stats <- challenge_tour_player_data %>% purrr::pluck("stats")

#* Explore Missing Data ----

rounds %>% plot_missing() #euro tour does not give the data that is missing

holes %>% plot_missing()

stats %>% plot_missing() #euro tour does not give the data that is missing

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

write_csv(tournaments_df, "challenge_tour_tournaments.csv")

# Fix Euro Names
rounds <- tbl(con, "rounds_tbl") %>% collect()
holes <- tbl(con, "holes_tbl") %>% collect()
stats <-  tbl(con, "stats_tbl") %>% collect()

rounds_cleaned <- 
  rounds %>% 
  fix_euro_names()

holes_cleaned <- 
  holes %>%
  fix_euro_names()

stats_cleaned <- 
  stats %>% 
  fix_euro_names()

stats_cleaned <- 
  stats_cleaned %>% 
  mutate(across(.cols = c(driving_distance:possible_gir), ~ na_if(., 0))) %>%
  mutate(driving_distance = ifelse(driving_distance < 150, NA, driving_distance)) %>% 
  mutate(dd_over_field = ifelse(driving_distance %>% is.na(), NA, dd_over_field))

# Overwrite Tables
rounds_cleaned %>% 
  DBI::dbWriteTable(conn = con, "rounds_tbl", ., field.types = c(date = "date"), overwrite = TRUE)

holes_cleaned %>% 
  DBI::dbWriteTable(conn = con, "holes_tbl", ., field.types = c(date = "date"), overwrite = TRUE)

stats_cleaned %>% 
  DBI::dbWriteTable(conn = con, "stats_tbl", ., field.types = c(date = "date"), overwrite = TRUE)

  

#Quick check of rewrite
tbl(con, "holes_tbl") %>% 
  filter(player_name %>% str_detect("Rory M") & tour == "euro")

dbDisconnect(con)
