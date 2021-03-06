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

player_adj_dk_gained_sim <- function(df, n = 5, easy = 2, medium = 9, hard = 7, holes_min = 360, hole_min_penalty = .35) {
  
  num_of_holes <- 
    df %>% 
    count() %>% 
    pull() %>% 
    as.double()
  
  easy_scores <-
    df %>%
    filter(hole_category == "easy") %>% 
    mutate(num_of_holes = num_of_holes) %>% 
    mutate(adj_dk_pts_gained = if_else(num_of_holes >= holes_min, adj_dk_pts_gained, adj_dk_pts_gained - hole_min_penalty)) %>% 
    pull(adj_dk_pts_gained)
  
  medium_scores <-
    df %>%
    filter(hole_category == "medium") %>%
    mutate(num_of_holes = num_of_holes) %>%
    mutate(adj_dk_pts_gained = if_else(num_of_holes >= holes_min, adj_dk_pts_gained, adj_dk_pts_gained - hole_min_penalty)) %>%
    pull(adj_dk_pts_gained)
  
  hard_scores <-
    df %>%
    filter(hole_category == "hard") %>%
    mutate(num_of_holes = num_of_holes) %>%
    mutate(adj_dk_pts_gained = if_else(num_of_holes >= holes_min, adj_dk_pts_gained, adj_dk_pts_gained - hole_min_penalty)) %>%
    pull(adj_dk_pts_gained)
  
  # Easy Hole Scores
  easy_hole_sims <- get_player_sims(sims = n, easy_scores, easy)

  # Medium Hole Scores
  medium_hole_sims <- get_player_sims(sims = n, medium_scores, medium)

  # Hard Hole Scores
  hard_hole_sims <- get_player_sims(sims = n, hard_scores, hard)
  
  
  # Calculate showdown dk points gained
  bind_rows(easy_hole_sims, medium_hole_sims, hard_hole_sims) %>%
    group_by(sim) %>%
    summarise(dk_sd_pts_gained = sum(score_type))
  
}

complete_simulation_framework <- function(df, num_of_sims = 10000, date_range = 365, 
                                          six_week_weight = 1.25, three_month_weight = 1.125, six_month_weight = 1.0625) {
  
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
  
  easy_holes <- df %>%
    head(1) %>%
    select(easy) %>%
    pull()
  
  medium_holes <- df %>%
    head(1) %>%
    select(medium) %>%
    pull()
  
  hard_holes <- df %>%
    head(1) %>%
    select(hard) %>%
    pull()
  
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
    select(player_name, date, season:course_id, round, hole, score_type, dk_pts_sd, field_dk_sd_avg, dk_pts_gained_sd) %>% 
    collect()
  
  # calculate total dk pts gained by each player in tournament/round combo
  dk_pts_gained_rd <-
    holes_tbl %>%
    unite("tournament_round_id", c(tournament_id:round)) %>%
    group_by(player_name, tournament_round_id) %>%
    summarise(total_dk_pts_gained = sum(dk_pts_gained_sd)) %>%
    ungroup() %>%
    # lumping players together that have played less than 25 rounds in past year
    mutate(player_name = fct_lump_min(player_name, 25)) %>%
    # left_join(avg_dk_hole_score_by_tourney_rd_tbl) %>%
    select(player_name, tournament_round_id, total_dk_pts_gained)
  
  # create dummy vars for all players and tournament/course/rd combo
  course_rd_effect_recipe <-
    recipe(~ ., data = dk_pts_gained_rd) %>%
    step_dummy(all_nominal(), one_hot = TRUE)
  
  course_rd_effect_data <-
    course_rd_effect_recipe %>% prep() %>% bake(new_data = NULL)
  
  # run course/field effects model
  course_rd_effect_model <-
    lm(total_dk_pts_gained ~ . , data = course_rd_effect_data)
  
  course_round_effect_tbl <-
    tidy(course_rd_effect_model) %>%
    filter(term %>% str_detect("tournament_round_id")) %>%
    arrange(estimate) %>%
    mutate(term = term %>% str_sub(22,-1)) %>%
    select(tournament_round_id = term, course_rd_effect = estimate) %>%
    mutate(course_rd_effect_per_hole = course_rd_effect / 18)
  
  #join holes data with course effects data, calculate adjusted dk points gained
  adj_pts_gained_nested_tbl <-
    holes_tbl %>%
    filter(player_name %in% c(tournament_players)) %>%
    unite("tournament_round_id", c(tournament_id:round)) %>%
    left_join(course_round_effect_tbl, by = "tournament_round_id") %>%
    mutate(course_rd_effect_per_hole = if_else(is.na(course_rd_effect_per_hole), 0, course_rd_effect_per_hole)) %>% 
    mutate(adj_field_dk_avg = field_dk_sd_avg  - course_rd_effect_per_hole) %>%
    mutate(adj_dk_pts_gained = (dk_pts_gained_sd - course_rd_effect_per_hole)) %>%
    filter(!adj_dk_pts_gained %>% is.na()) %>%
    mutate(
      hole_category = case_when(
        adj_field_dk_avg >= hole_category_values[2] ~ "easy",
        adj_field_dk_avg <= hole_category_values[1] ~ "hard",
        TRUE ~ "medium"
      )
    ) %>%
    # weight scores
    mutate(adj_dk_pts_gained = case_when(
      date %>% between(left = end_date - days(45), right = end_date) ~ adj_dk_pts_gained * six_week_weight,
      date %>% between(left = end_date - days(90), right = end_date - days(46)) ~ adj_dk_pts_gained * three_month_weight,
      date %>% between(left = end_date - days(180), right = end_date - days(91)) ~ adj_dk_pts_gained * six_month_weight,
      TRUE ~ adj_dk_pts_gained
    )) %>%
    select(player_name, adj_dk_pts_gained, hole_category) %>%
    nest(pts_gained = -player_name)
  
  # run simulations for tournament
  tournament_sims <-
    adj_pts_gained_nested_tbl %>%
    filter(player_name %in% c(tournament_players)) %>%
    pull(pts_gained) %>%
    future_map(purrr::possibly(~ player_adj_dk_gained_sim(.x, 
                                                          n = num_of_sims, 
                                                          easy = easy_holes,
                                                          medium = medium_holes,
                                                          hard = hard_holes
    ), otherwise = NA)) %>%
    enframe() %>%
    bind_cols(select(adj_pts_gained_nested_tbl, player_name)) %>%
    unnest(value) %>%
    group_by(sim) %>%
    mutate(dk_sd_rank = min_rank(-dk_sd_pts_gained)) %>%
    ungroup()
  
  # Sims Summarized
  tournament_sims %>%
    group_by(player_name) %>%
    summarise(
      avg_dk_pts_gained = mean(dk_sd_pts_gained),
      win_dk = sum(dk_sd_rank == 1) / num_of_sims * 100,
      top5_dk = sum(dk_sd_rank <= 5) / num_of_sims * 100,
      top10_dk = sum(dk_sd_rank <= 10) / num_of_sims * 100,
      top20_dk = sum(dk_sd_rank <= 20) / num_of_sims * 100
    ) %>%
    arrange(desc(top20_dk)) %>%
    ungroup() %>%
    mutate(tournament_id = tournament_id)
  
}
