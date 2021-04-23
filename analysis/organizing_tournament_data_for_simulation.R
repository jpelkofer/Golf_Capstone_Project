library(DBI)
library(RPostgres)
library(tidyverse)
library(lubridate)

# load past results
dk_past_results <- 
  read_csv("analysis/dk_past_results.csv") %>% 
  janitor::clean_names() %>% 
  mutate(date = mdy(date)) %>% 
  filter(date %>% between(left = ymd("2019-01-13"), right = ymd("2019-08-25")))

# Remove Tournaments - that are non-cut events, three-day cut event, or alternate event
dk_past_results <- 
  dk_past_results %>% 
  filter(!tournament_id %in% c(401056511, 401056514, 401056516, 401056542, 401056559, 401056543, 401056546,401056517, 401056525))

# Pull Tournaments to Sim - align course and tournament names
tournaments_to_sim <- 
  dk_past_results %>% 
  select(tournament_id, tournament_name, course, date) %>% 
  mutate(date = date - days(4)) %>% 
  distinct() %>% 
  separate(course, into = c("course", "location"), sep = " - ") %>% 
  mutate(course = case_when(
    course == "Spyglass Hill" ~ "Spyglass Hill GC",
    course == "Riviera" ~ "Riviera Country Club", 
    course == "Torrey Pines" ~ "Torrey Pines North", 
    course == "PGA National" ~ "PGA National Champion Course", 
    course == "Innisbrook" ~ "Innisbrook Resort -Copperhead", 
    course == "TPC San Antonio" ~ "TPC San Antonio (Oaks)",
    course == "Muirfield Village" ~ "Muirfield Village GC",
    course == "Keene Trace" ~ "Keene Trace GC", 
    course == "TPC Southwind, Memphis, TN" ~ "TPC Southwind",
    TRUE ~ course
  )) %>% 
  mutate(tournament_name = case_when(
    tournament_name == "The Memorial Tournament pres. by Nationwide" ~ "the Memorial Tournament",
    tournament_name == "WGC-FedEx St. Jude Classic" ~ "FedEx St. Jude Classic",
    tournament_name == "Arnold Palmer Invitational Pres. by Mastercard" ~ "Arnold Palmer Invitational",
    TRUE ~ tournament_name
  ))

# load hole category data and join with database tournaments table
con <- dbConnect(
  drv = RPostgres::Postgres(),
  user = "postgres",
  password = keyring::key_get("postgres_db", "postgres"),
  dbname = "golf_db",
)

tournaments_tbl <- tbl(con, "tournaments_tbl") %>% collect()

hole_categories_tbl <- 
  read_csv("analysis/cluster_holes/past_holes_categorized/hole_categories_tbl.csv") %>% 
  separate(col = tournament_round_id, into = "tournament_id", sep = "_", remove = FALSE) %>% 
  # select(tournament_id, hole_category, adj_field_dk_avg) %>% 
  left_join(select(tournaments_tbl, tournament_id, tour, season, tournament_name, course), by = "tournament_id")

# Assign Hole Category Distributions Per Tournament - addtional course and tournament name correction for joining data
hole_difficulties_per_tournament <- 
  hole_categories_tbl %>% 
  filter(tour == "pga") %>% 
  mutate(tournament_name = case_when(
    tournament_name == "2017 Masters Tournament" ~ "Masters Tournament",
    tournament_name == "2018 Masters Tournament" ~ "Masters Tournament",
    tournament_name == "SBS Tournament of Champions" ~ "Sentry Tournament of Champions",
    tournament_name == "Fort Worth Invitational" ~ "Charles Schwab Challenge",
    tournament_name == "DEAN &amp; DELUCA Invitational" ~ "Charles Schwab Challenge",
    tournament_name == "TOUR Championship" ~ "Tour Championship",
    tournament_name == "World Golf Championships-Bridgestone Invitational" ~ "WGC-Bridgestone Invitational",
    tournament_name == "Shell Houston Open" ~ "Houston Open",
    tournament_name == "AT&amp;T Pebble Beach Pro-Am" ~ "AT&T Pebble Beach Pro-Am",
    tournament_name == "AT&amp;T Byron Nelson" ~ "AT&T Byron Nelson",
    tournament_name == "THE PLAYERS Championship" ~ "The Players Championship",
    tournament_name == "CareerBuilder Challenge" ~ "Desert Classic",
    tournament_name == "Northern Trust Open" ~ "Genesis Open",
    tournament_name == "World Golf Championships-Mexico Championship" ~ "WGC-Mexico Championship",
    TRUE ~ tournament_name
  )) %>% 
  group_by(tournament_name, course, hole_category) %>% 
  summarise(total = n()) %>% 
  ungroup() %>% 
  left_join(
    hole_categories_tbl %>% 
      filter(tour == "pga") %>% 
      mutate(tournament_name = case_when(
        tournament_name == "2017 Masters Tournament" ~ "Masters Tournament",
        tournament_name == "2018 Masters Tournament" ~ "Masters Tournament",
        tournament_name == "SBS Tournament of Champions" ~ "Sentry Tournament of Champions",
        tournament_name == "Fort Worth Invitational" ~ "Charles Schwab Challenge",
        tournament_name == "DEAN &amp; DELUCA Invitational" ~ "Charles Schwab Challenge",
        tournament_name == "TOUR Championship" ~ "Tour Championship",
        tournament_name == "World Golf Championships-Bridgestone Invitational" ~ "WGC-Bridgestone Invitational",
        tournament_name == "Shell Houston Open" ~ "Houston Open",
        tournament_name == "AT&amp;T Pebble Beach Pro-Am" ~ "AT&T Pebble Beach Pro-Am",
        tournament_name == "AT&amp;T Byron Nelson" ~ "AT&T Byron Nelson",
        tournament_name == "THE PLAYERS Championship" ~ "The Players Championship",
        tournament_name == "CareerBuilder Challenge" ~ "Desert Classic",
        tournament_name == "Northern Trust Open" ~ "Genesis Open",
        tournament_name == "World Golf Championships-Mexico Championship" ~ "WGC-Mexico Championship",
        TRUE ~ tournament_name
      )) %>%
      group_by(tournament_name, course) %>% 
      summarise(total_holes = n()) %>% 
      ungroup(),
    by = c("tournament_name", "course")
  ) %>% 
  mutate(hole_distribution = total / total_holes,
         holes_per_round = round(hole_distribution * 18, 0)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(tournament_name, course), names_from = hole_category, values_from = holes_per_round) %>% 
  mutate(total_holes = easy + medium + hard) %>% 
  mutate(medium = case_when(
    total_holes == 19 ~ medium - 1,
    total_holes == 17 ~ medium + 1,
    TRUE ~ medium)) %>% 
  mutate(total_holes = easy + medium + hard) %>% 
  mutate(course = case_when(
    course == "Bay Hill Club &amp; Lodge" ~ "Bay Hill",
    course == "Augusta National Golf Club" ~ "Augusta National",
    course == "Harbour Town Golf Links" ~ "Harbour Town",
    course == "Quail Hollow Club" ~ "Quail Hollow",
    course == "Trinity Forest Golf Club" ~ "Trinity Forest",
    course == "Colonial CC" ~ "Colonial",
    course == "Sedgefield Country Club" ~ "Sedgefield",
    course == "East Lake Golf Club" ~ "East Lake",
    course == "Club De Golf Chapultepec" ~ "Chapultepec",
    TRUE ~ course
  ))

# Hole Difficulties for 1st time course in Simulation and Majors Not Played on Same Course
#U.S. Open
hole_difficulties_per_tournament %>% 
  filter(tournament_name == "U.S. Open") %>% 
  group_by(tournament_name) %>% 
  summarise(across(
    .cols = c(easy:medium),
    .fns = mean
  )) %>% 
  # The Open
  bind_rows(
    hole_difficulties_per_tournament %>% 
      filter(tournament_name == "The Open") %>% 
      group_by(tournament_name) %>% 
      summarise(across(
        .cols = c(easy:medium),
        .fns = mean
      ))
  ) %>% 
  # PGA Championship
  bind_rows(
    hole_difficulties_per_tournament %>% 
      filter(tournament_name == "PGA Championship") %>% 
      group_by(tournament_name) %>% 
      summarise(across(
        .cols = c(easy:medium),
        .fns = mean
      ))
  ) %>% 
  # Standard PGA Event
  bind_rows(
    hole_difficulties_per_tournament %>% 
      filter(!tournament_name %in% c("U.S. Open", "The Open", "Masters Tournament", "PGA Championship") &
               !tournament_name %>% str_detect("World Golf Championship") &
               !tournament_name %>% str_detect("WGC")) %>% 
      mutate(tournament_name = "standard_pga_event") %>% 
      group_by(tournament_name) %>% 
      summarise(across(
        .cols = c(easy:medium),
        .fns = mean
      ))
  ) %>% 
  mutate(across(
    .cols = c(easy:medium),
    .fns = round
  )) %>% 
  mutate(total_holes = easy + medium + hard) %>% 
  mutate(medium = case_when(
    total_holes == 19 ~ medium - 1,
    total_holes == 17 ~ medium + 1,
    TRUE ~ medium)) %>% 
  mutate(total_holes = easy + medium + hard)

# Join Tournaments to Sim with Hole Difficulty Data
tournaments_with_difficulty_to_sim <- 
  tournaments_to_sim %>%
  left_join(hole_difficulties_per_tournament,
            by = c("tournament_name", "course")) %>%
  mutate(
    easy = case_when(
      tournament_name == "U.S. Open" ~ 1,
      tournament_name == "The Open" ~ 2,
      tournament_name == "PGA Championship" ~ 2,
      TRUE ~ easy
    )
  ) %>%
  mutate(
    medium = case_when(
      tournament_name == "U.S. Open" ~ 5,
      tournament_name == "The Open" ~ 5,
      tournament_name == "PGA Championship" ~ 7,
      TRUE ~ medium
    )
  ) %>%
  mutate(
    hard = case_when(
      tournament_name == "U.S. Open" ~ 12,
      tournament_name == "The Open" ~ 11,
      tournament_name == "PGA Championship" ~ 9,
      TRUE ~ hard
    )
  ) %>% 
  mutate(easy = if_else(is.na(easy), 2, easy),
         medium = if_else(is.na(medium), 9, medium),
         hard = if_else(is.na(hard), 7, hard)) %>% 
  mutate(total_holes = 18)

# correct dates
tournaments_to_sim <- 
  tournaments_to_sim %>%
  mutate(date = case_when(
    tournament_id == 401056515 ~ ymd("2019-02-13"),
    tournament_id == 401056512 ~ ymd("2019-01-23"),
    TRUE ~ date
  ))

# final data set for simulations
tournament_ids <- 
  tournaments_to_sim %>% 
  pull(tournament_id)

tournament_dates <- 
  tournaments_to_sim %>% 
  pull(date)

get_tournament_players <- function(df, tid) {
  
  df %>%
    filter(tournament_id %in% c(tid)) %>%
    pull(player)
  
}

tournament_players_tbl <-
  map(tournament_ids, ~ get_tournament_players(dk_past_results, .x)) %>% 
  enframe(value = "players") %>% 
  bind_cols(tournament_id = tournament_ids, date = tournament_dates) %>% 
  unnest(players) %>% 
  left_join(select(tournaments_with_difficulty_to_sim, tournament_id, easy, medium, hard), by = "tournament_id") %>% 
  nest(-name)

write_rds(tournament_players_tbl, "analysis/tournament_players_tbl.rds")
