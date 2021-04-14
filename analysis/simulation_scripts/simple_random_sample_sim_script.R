library(DBI)
library(RPostgres)
library(tidyverse)
library(lubridate)
library(tidymodels)
library(furrr)

get_player_sims <- function(sims, scores, n_samples) {
  
  # generates player simulations
  
  if (n_samples == 1) {
    tibble(score_type = sample(scores, sims, replace = TRUE)) %>%
      mutate(sim = str_glue("X{row_number()}") %>% as.character(),
             .before = score_type)
  } else if (n_samples > 1) {
    data.frame(replicate(sims,
                         sample(scores, n_samples, replace = TRUE))) %>%
      pivot_longer(cols = everything(),
                   names_to = "sim",
                   values_to = "score_type")
  }  else {
    tibble(sim = character(),
           score_type = character())
  }
  
}

player_simple_sim_func <- function(df, n, holes = 18) {
  
  # runs specified number of simulations for a single player
  
  scores <-
    df %>%
    pull(score_type)
  
  rd_1_sims <- get_player_sims(sims = n, scores, holes)
  rd_2_sims <- get_player_sims(sims = n, scores, holes)
  rd_3_sims <- get_player_sims(sims = n, scores, holes)
  rd_4_sims <- get_player_sims(sims = n, scores, holes)
  
  # Calculate Draft Kings Points and Score for Simulation
  rds_1_and_2_tbl <-
    bind_rows(rd_1_sims, rd_2_sims) %>%
    mutate(
      dk_pts_rds_1_and_2 = case_when(
        score_type %in% c("DOUBLE_EAGLE") ~ 13,
        score_type %in% c("EAGLE") ~ 8,
        score_type %in% c("BIRDIE") ~ 3,
        score_type %in% c("PAR") ~ .5,
        score_type %in% c("BOGEY") ~ -.5,
        score_type %in% c("DOUBLE_BOGEY", "TRIPLE_BOGEY", "OTHER") ~ -1
      ),
      score_rds_1_and_2 = case_when(
        score_type %in% c("DOUBLE_EAGLE") ~ -3,
        score_type %in% c("EAGLE") ~ -2,
        score_type %in% c("BIRDIE") ~ -1,
        score_type %in% c("PAR") ~ 0,
        score_type %in% c("BOGEY") ~ 1,
        score_type %in% c("DOUBLE_BOGEY") ~ 2,
        score_type %in% c("TRIPLE_BOGEY") ~ 3,
        score_type %in% c("OTHER") ~ 4
      )
    ) %>%
    group_by(sim) %>%
    summarise(across(dk_pts_rds_1_and_2:score_rds_1_and_2, .fns = sum))
  
  rds_3_and_4_tbl <-
    bind_rows(rd_3_sims, rd_4_sims) %>%
    mutate(
      dk_pts_rds_3_and_4 = case_when(
        score_type %in% c("DOUBLE_EAGLE") ~ 13,
        score_type %in% c("EAGLE") ~ 8,
        score_type %in% c("BIRDIE") ~ 3,
        score_type %in% c("PAR") ~ .5,
        score_type %in% c("BOGEY") ~ -.5,
        score_type %in% c("DOUBLE_BOGEY", "TRIPLE_BOGEY", "OTHER") ~ -1
      ),
      score_rds_3_and_4 = case_when(
        score_type %in% c("DOUBLE_EAGLE") ~ -3,
        score_type %in% c("EAGLE") ~ -2,
        score_type %in% c("BIRDIE") ~ -1,
        score_type %in% c("PAR") ~ 0,
        score_type %in% c("BOGEY") ~ 1,
        score_type %in% c("DOUBLE_BOGEY") ~ 2,
        score_type %in% c("TRIPLE_BOGEY") ~ 3,
        score_type %in% c("OTHER") ~ 4
      )
    ) %>%
    group_by(sim) %>%
    summarise(across(dk_pts_rds_3_and_4:score_rds_3_and_4, .fns = sum))
  
  left_join(rds_1_and_2_tbl, rds_3_and_4_tbl)
}

complete_simulation_framework <- function(df) {
  
  plan(multisession, workers = 4)
  
  con <- dbConnect(drv = RPostgres::Postgres(), 
                   user = "postgres", 
                   password = keyring::key_get("postgres_db", "postgres"), 
                   dbname = "golf_db",
  )
  
  tournament_players <-
    df %>%
    pull(players)
  
  end_date <- 
    df %>%  
    distinct(date) %>% 
    pull(date)
  
  start_date <- end_date - days(365)
  
  tournament_id <-
    df %>%
    distinct(tournament_id) %>% 
    pull(tournament_id)
  
  # pull in holes data
  holes_nested_tbl <-
    tbl(con, "holes_tbl") %>%
    mutate(player_name = case_when(
      player_name == "Ali Al-Shahrani" ~ "Ali Al Shahrani",
      TRUE ~ player_name
    )) %>%
    filter(player_name %in% c(tournament_players)) %>%
    filter(date %>% between(
      left = start_date,
      right = end_date
    )) %>%
    select(player_name, score_type) %>%
    collect() %>%
    nest(scores = -player_name)
  
  # run simulations
  sims <-
    holes_nested_tbl %>%
    pull(scores) %>%
    future_map( purrr::possibly(~ player_simple_sim_func(.x, n = 10000), otherwise = NA)) %>%
    enframe() %>%
    bind_cols(select(holes_nested_tbl, player_name)) %>%
    unnest(value) %>%
    group_by(sim) %>%
    mutate(rd_1_and_2_rank = min_rank(score_rds_1_and_2)) %>%
    ungroup() %>%
    mutate(
      dk_pts_rds_3_and_4 = if_else(rd_1_and_2_rank > 65, 0, dk_pts_rds_3_and_4),
      score_rds_3_and_4 = if_else(rd_1_and_2_rank > 65, 100, score_rds_3_and_4)
    ) %>%
    mutate(
      dk_scoring_pts = dk_pts_rds_1_and_2 + dk_pts_rds_3_and_4,
      total_score = score_rds_1_and_2 + score_rds_3_and_4
    ) %>%
    group_by(sim) %>%
    mutate(final_pos = min_rank(total_score)) %>%
    mutate(
      dk_finish_pts = case_when(
        final_pos == 1 ~ 30,
        final_pos == 2 ~ 20,
        final_pos == 3 ~ 18,
        final_pos == 4 ~ 16,
        final_pos == 5 ~ 14,
        final_pos == 6 ~ 12,
        final_pos == 7 ~ 10,
        final_pos == 8 ~ 9,
        final_pos == 9 ~ 8,
        final_pos == 10 ~ 7,
        final_pos %in% c(11:15) ~ 6,
        final_pos %in% c(16:20) ~ 5,
        final_pos %in% c(21:25) ~ 4,
        final_pos %in% c(26:30) ~ 3,
        final_pos %in% c(31:40) ~ 2,
        final_pos %in% c(41:50) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(dk_total_pts = dk_scoring_pts + dk_finish_pts) %>%
    mutate(dk_pts_rank = min_rank(-dk_total_pts)) %>%
    ungroup()
  
  
  # Sims Summarized
  sims %>%
    group_by(player_name) %>%
    summarise(
      win_perc = sum(final_pos == 1) / 10000 * 100,
      top5_perc = sum(final_pos <= 5) / 10000 * 100,
      top20_perc = sum(final_pos <= 20) / 10000 * 100,
      avg_dk_points = mean(dk_total_pts),
      top5_dk = sum(dk_pts_rank <= 5) / 10000 * 100,
      top20_dk = sum(dk_pts_rank <= 20) / 10000 * 100
    ) %>%
    arrange(desc(top20_dk)) %>%
    ungroup() %>%
    mutate(tournament_id = tournament_id)
  
}