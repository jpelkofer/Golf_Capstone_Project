library(tidyverse)

# Connect to Postgres DB ----
library(DBI)
library(RPostgres)

con <- dbConnect(drv = RPostgres::Postgres(), 
                 user = "postgres", 
                 password = keyring::key_get("postgres_db", "postgres"), 
                 dbname = "golf_db")

# collect distinct player ids, player names, tour combinations
player_ids_names_df <- 
  tbl(con, "rounds_tbl") %>% 
  select(player_id, player_name, tour) %>% 
  distinct() %>% 
  arrange(player_name) %>% 
  collect()

# distinct player name, player id combos for data collected from espn.com
player_ids_names_pga_ntw <- 
  player_ids_names_df %>% 
  filter(tour %in% c("pga", "ntw")) %>% 
  distinct(player_id, player_name)

# distinct player name, player id combos for data collected from europeantour.com
player_ids_names_euro_ct <- 
  player_ids_names_df %>% 
  filter(tour %in% c("euro", "ct")) %>% 
  distinct(player_id, player_name)

# join datasets
player_ids_names_pga_ntw %>% 
  full_join(player_ids_names_euro_ct, by = "player_name") %>% 
  rename(espn_id = player_id.x,
         euro_id = player_id.y) %>% 
  arrange(player_name) %>% 
  write_csv("player_ids_dataset.csv")

player_ids_names_euro_ct %>% 
  filter(player_name %>% str_detect("Tho"))
  


