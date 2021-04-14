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

player_adj_dk_gained_sim <- function(df, n = 5, holes = 18) {
  
  num_of_holes <- 
    df %>% 
    count() %>% 
    pull() %>% 
    as.double()
  
  scores <-
    df %>%
    mutate(num_of_holes = num_of_holes) %>% 
    mutate(adj_dk_pts_gained = if_else(num_of_holes >= 360, dk_pts_gained_classic, dk_pts_gained_classic - 0.35)) %>% 
    pull(adj_dk_pts_gained)
  
  rd_1_sims <- get_player_sims(sims = n, scores, holes)
  rd_2_sims <- get_player_sims(sims = n, scores, holes)
  rd_3_sims <- get_player_sims(sims = n, scores, holes)
  rd_4_sims <- get_player_sims(sims = n, scores, holes)
  
  # Calculate rds1 and rds2 dk points gained
  rds_1_and_2_tbl <-
    bind_rows(rd_1_sims, rd_2_sims) %>%
    group_by(sim) %>%
    summarise(dk_pts_gained_rds_1_2 = sum(score_type))
  
  # Calculate rds3 and rds4 dk points gained
  rds_3_and_4_tbl <-
    bind_rows(rd_3_sims, rd_4_sims) %>%
    group_by(sim) %>%
    summarise(dk_pts_gained_rds_3_4 = sum(score_type))
  
  left_join(rds_1_and_2_tbl, rds_3_and_4_tbl)
}

complete_simulation_framework <- function(df, num_of_sims = 10000, cut_range = 65, date_range = 365, 
                                          six_week_weight = 1, three_month_weight = 1, six_month_weight = 1) {
  
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
  
  start_date <- end_date - days(date_range)
  
  tournament_id <-
    df %>%
    distinct(tournament_id) %>% 
    pull(tournament_id)
  
  # pull in holes data
  holes_tbl <- 
    tbl(con, "holes_tbl") %>%
    filter(round != 5) %>% 
    mutate(player_name = case_when(
      player_name == "Ali Al-Shahrani" ~ "Ali Al Shahrani",
      TRUE ~ player_name
    )) %>%
    filter(date %>% between(left = start_date,
                            right = end_date)) %>% 
    select(player_name, date, season:course_id, round, hole, score_type, dk_pts_classic, field_dk_classic_avg, dk_pts_gained_classic) %>% 
    collect()
  
  #filter for players in tournament, apply recent form weights
  pts_gained_nested_tbl <-
    holes_tbl %>%
    filter(player_name %in% c(tournament_players)) %>%
    filter(!dk_pts_gained_classic %>% is.na()) %>%
    # weight scores
    mutate(dk_pts_gained_classic = case_when(
      date %>% between(left = end_date - days(45), right = end_date) ~ dk_pts_gained_classic * six_week_weight,
      date %>% between(left = end_date - days(90), right = end_date - days(46)) ~ dk_pts_gained_classic * three_month_weight,
      date %>% between(left = end_date - days(180), right = end_date - days(91)) ~ dk_pts_gained_classic * six_month_weight,
      TRUE ~ dk_pts_gained_classic
    )) %>% 
    select(player_name, dk_pts_gained_classic) %>%
    nest(pts_gained = -player_name)
  
  # run simulations for tournament
  tournament_sims <-
    pts_gained_nested_tbl %>%
    filter(player_name %in% c(tournament_players)) %>%
    pull(pts_gained) %>%
    future_map(purrr::possibly(~ player_adj_dk_gained_sim(.x, n = num_of_sims), otherwise = NA)) %>%
    enframe() %>%
    bind_cols(select(pts_gained_nested_tbl, player_name)) %>%
    unnest(value) %>%
    group_by(sim) %>%
    mutate(rd_1_and_2_rank = min_rank(-dk_pts_gained_rds_1_2)) %>%
    ungroup() %>%
    mutate(dk_pts_gained_rds_3_4 = if_else(rd_1_and_2_rank > cut_range, 0, dk_pts_gained_rds_3_4)) %>%
    mutate(dk_total_pts_gained = dk_pts_gained_rds_1_2 + dk_pts_gained_rds_3_4) %>%
    group_by(sim) %>%
    mutate(dk_pts_rank = min_rank(-dk_total_pts_gained)) %>%
    ungroup()
  
  # Sims Summarized
  tournament_sims %>%
    group_by(player_name) %>%
    summarise(
      avg_dk_pts_gained = mean(dk_total_pts_gained),
      min_dk_pts_gained = min(dk_total_pts_gained),
      max_dk_pts_gained = max(dk_total_pts_gained),
      win_dk = sum(dk_pts_rank == 1) / num_of_sims * 100,
      top5_dk = sum(dk_pts_rank <= 5) / num_of_sims * 100,
      top10_dk = sum(dk_pts_rank <= 10) / num_of_sims * 100,
      top20_dk = sum(dk_pts_rank <= 20) / num_of_sims * 100
    ) %>%
    arrange(desc(top20_dk)) %>%
    ungroup() %>%
    mutate(tournament_id = tournament_id)
  
}