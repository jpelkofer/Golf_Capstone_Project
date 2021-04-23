library(tidyverse)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

optimal_lineup_function <- function(df) {
  
  max_salary <- 50000
  max_players <-  6
  num_of_players <- length(df$player_name)
  salaries <- df$salary
  dk_pts_gained <- df$avg_dk_pts_gained
  dk_max_pts_gained <- df$max_dk_pts_gained
  dk_min_pts_gained <- df$min_dk_pts_gained
  dk_top_20 <- df$top20_dk
  
  avg_pts_lineup <- 
    MIPModel() %>% 
    add_variable(x[i], i = 1:num_of_players, type = "binary") %>% 
    add_constraint(sum_expr(x[i], i = 1:num_of_players) == max_players) %>% 
    add_constraint(sum_expr(salaries[i] * x[i], i = 1:num_of_players) <= max_salary) %>% 
    set_objective(sum_expr(dk_pts_gained[i] * x[i], i = 1:num_of_players), "max") %>% 
    solve_model(with_ROI(solver = "glpk")) %>% 
    get_solution(x[i]) %>% 
    mutate(tournament_id = df$tournament_id,
           lineup = "avg_pts",
           player_name = df$player_name,
           salary = df$salary) %>% 
    filter(value > 0) %>% 
    select(tournament_id, player_name, salary, lineup)
  
  max_pts_lineup <- 
    MIPModel() %>% 
    add_variable(x[i], i = 1:num_of_players, type = "binary") %>% 
    add_constraint(sum_expr(x[i], i = 1:num_of_players) == max_players) %>% 
    add_constraint(sum_expr(salaries[i] * x[i], i = 1:num_of_players) <= max_salary) %>% 
    set_objective(sum_expr(dk_max_pts_gained[i] * x[i], i = 1:num_of_players), "max") %>% 
    solve_model(with_ROI(solver = "glpk")) %>% 
    get_solution(x[i]) %>% 
    mutate(tournament_id = df$tournament_id,
           lineup = "max_pts",
           player_name = df$player_name,
           salary = df$salary) %>%  
    filter(value > 0) %>% 
    select(tournament_id, player_name, salary, lineup)
  
  min_pts_lineup <- 
    MIPModel() %>% 
    add_variable(x[i], i = 1:num_of_players, type = "binary") %>% 
    add_constraint(sum_expr(x[i], i = 1:num_of_players) == max_players) %>% 
    add_constraint(sum_expr(salaries[i] * x[i], i = 1:num_of_players) <= max_salary) %>% 
    set_objective(sum_expr(dk_min_pts_gained[i] * x[i], i = 1:num_of_players), "max") %>% 
    solve_model(with_ROI(solver = "glpk")) %>% 
    get_solution(x[i]) %>% 
    mutate(tournament_id = df$tournament_id,
           lineup = "min_pts",
           player_name = df$player_name,
           salary = df$salary) %>%  
    filter(value > 0) %>% 
    select(tournament_id, player_name, salary, lineup)

  top_20_lineup <- 
    MIPModel() %>% 
    add_variable(x[i], i = 1:num_of_players, type = "binary") %>% 
    add_constraint(sum_expr(x[i], i = 1:num_of_players) == max_players) %>% 
    add_constraint(sum_expr(salaries[i] * x[i], i = 1:num_of_players) <= max_salary) %>% 
    set_objective(sum_expr(dk_top_20[i] * x[i], i = 1:num_of_players), "max") %>% 
    solve_model(with_ROI(solver = "glpk")) %>% 
    get_solution(x[i]) %>% 
    mutate(tournament_id = df$tournament_id,
           lineup = "top_20",
           player_name = df$player_name,
           salary = df$salary) %>% 
    filter(value > 0) %>% 
    select(tournament_id, player_name, salary, lineup)
  
  list(avg_pts_lineup, max_pts_lineup, min_pts_lineup, top_20_lineup)

}



# optimal_lineup_counts <-
#   tournament_sims %>%
#   select(sim, player_name, dk_total_pts_gained) %>%
#   left_join(select(dr, players, salary), by = c("player_name" = "players")) %>%
#   nest(data = c(-sim)) %>%
#   select(data) %>%
#   pmap(~ optimal_lineup_function(.x)) %>%
#   enframe() %>%
#   unnest(value) %>%
#   group_by(player_name) %>%
#   summarise(optimal_lu_percentage = sum(optimal_lu)/10000)


