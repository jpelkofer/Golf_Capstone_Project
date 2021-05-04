library(tidyverse)
library(lubridate)
library(ragg)
library(ggtext)

theme_set(theme_minimal())

# load actual dk points data
dk_past_results <- 
  read_csv("analysis/dk_past_results.csv") %>% 
  janitor::clean_names() %>% 
  mutate(date = mdy(date)) %>% 
  filter(date %>% between(left = ymd("2019-01-13"), right = ymd("2019-08-25")))

# Remove Tournaments - that are non-cut events, three-day cut event, or alternate event
dk_results <- 
  dk_past_results %>% 
  filter(!tournament_id %in% c(401056511, 401056514, 401056516, 401056542, 401056559, 401056543, 401056546,401056517, 401056525))

# load past dk double up results
double_up_results_tbl <- read_csv("analysis/simulation_results/double_up_results.csv")

# load past salary data
past_salaries <- 
  read_csv("analysis/simulation_results/player_salaries.csv") %>% 
  janitor::clean_names() %>% 
  # some cleaning to match up names
  mutate(player_name = case_when(
    player_name == "Sung-jae Im" ~ "Sungjae Im",
    player_name == "Fredrik Jacobson" ~ "Freddie Jacobson",
    player_name == "JT Poston" ~ "J.T. Poston",
    player_name == "Byeong-Hun An" ~ "Byeong Hun An",
    TRUE ~ player_name
  ))

# load simulation results
simulation_results <-
  read_csv("analysis/simulation_results/non_adjusted_pts_gained_sims.csv") %>%
  bind_rows(read_csv("analysis/simulation_results/non_adjusted_weighted_recent_form_sims.csv")) %>% 
  bind_rows(read_csv("analysis/simulation_results/adjusted_pts_gained_sims.csv")) %>%
  bind_rows(read_csv("analysis/simulation_results/adjusted_with_course_fit_sims.csv")) %>%
  bind_rows(read_csv("analysis/simulation_results/adjusted_course_fit_weighted_recent_form.csv")) %>% 
  bind_rows(read_csv("analysis/simulation_results/adjusted_pts_gained_weighted_recent_form_sims.csv"))

# join sim results and player salaries
sims_with_sals_tbl <- 
  simulation_results %>% 
  left_join(past_salaries, by = c("player_name", "tournament_id")) %>%
  drop_na() %>% 
  mutate(tid = tournament_id) %>% 
  nest(-c(tid, sim_type))

# load optimal lineup script ----
source("analysis/simulation_scripts/optimal_lineup_script.R")

# Generate models optimal lineups

optimal_lineups <- 
  sims_with_sals_tbl %>%
  select(data) %>% 
  pmap(~ optimal_lineup_function(.x))

optimal_lineup_joined <- 
  tibble(optimal_lineups, sims_with_sals_tbl) %>% 
  unnest(optimal_lineups) %>% 
  unnest(optimal_lineups) %>% 
  select(-data)

# Join with actual points
optimal_lineup_totals <- 
  optimal_lineup_joined %>% 
  left_join(select(dk_results, tournament_id, tournament_name, player, dk_total_fp), by = c("player_name" = "player",
                                                                           "tournament_id")) %>%
  distinct() %>% 
  group_by(tournament_id, tournament_name, lineup, sim_type) %>% 
  summarise(total_lineup_points = sum(dk_total_fp))

# write csv will manually update with actual gpp results
# write_csv(optimal_lineup_totals, "analysis/simulation_results/optimal_lineup_totals.csv")

# evaluate gpp results
optimal_lineup_gpp_results <- read_csv("analysis/simulation_results/optimal_lineup_totals.csv") %>% 
  left_join(select(dk_results, tournament_id, date) %>% 
              distinct(), by = "tournament_id") %>% 
  # no dk results for this tournament
  filter(!tournament_id %in% c("401056557")) %>% 
  mutate(Model = case_when(
    sim_type == "non_adjusted" ~ "Non-Adj",
    sim_type == "non_adjusted_weighted_recent_form" ~ "Non-Adj, Weighted Form",
    sim_type == "adjusted" ~ "Adj DKPG",
    sim_type == "adjusted_with_course_fit" ~ "Adj, Course Difficulty",
    sim_type == "adjusted_with_weighted_recent_form" ~ "Adj, Weighted Form",
    sim_type == "adjusted_course_fit_weighted_recent_form" ~ "Adj, Weighted Form, Course Difficulty"
  )) %>% 
  mutate(Lineup = case_when(
    lineup == "avg_pts" ~ "Avg DKPG",
    lineup == "min_pts" ~ "Min DKPG",
    lineup == "max_pts" ~ "Max DKPG",
    lineup == "top_20" ~ "Top 20 Prob"
  ))

# ROI by sim_type/lineup combo
roi_sim_lineup <- 
  optimal_lineup_gpp_results %>% 
  group_by(Model, Lineup) %>%
  summarise(
    `Total Buyins` = sum(buy_in),
    `Total Prizes` = sum(prize),
    ROI = ((sum(prize) - sum(buy_in)) / sum(buy_in))
  ) %>% 
  ungroup() %>% 
  arrange(desc(ROI)) %>% 
  # slice(1:7) %>% 
  mutate_at(vars(3:4), dollar) %>% 
  mutate_at(vars(5), percent_format())

# write_rds(roi_sim_lineup, "misc/roi_sim_lineup.rds")

# ROI by model
roi_by_model <- 
  optimal_lineup_gpp_results %>% 
  group_by(Model) %>%
  summarise(
    `Total Buyins` = sum(buy_in),
    `Total Prizes` = sum(prize),
    ROI = ((sum(prize) - sum(buy_in)) / sum(buy_in))
  ) %>% 
  ungroup() %>% 
  arrange(desc(ROI)) %>% 
  # slice(1:7) %>% 
  mutate_at(vars(2:3), dollar) %>% 
  mutate_at(vars(4), percent_format())

write_rds(roi_by_model, "misc/roi_by_model.rds")

# week by week ROI adjusted model  
gpp_wk_by_wk_roi_adjusted <- 
  optimal_lineup_gpp_results %>%
  filter(sim_type == "adjusted") %>%
  group_by(date, Model) %>% 
  summarise(
    total_buyins = sum(buy_in),
    total_prizes = sum(prize),
    ROI = (sum(prize) - sum(buy_in)) / sum(buy_in)
    ) %>%
  ungroup() %>% 
  arrange(date) %>% 
  mutate(profit_color = ifelse(ROI > 0, "#79ea86", "#e75757")) %>% 
  mutate(
    cumulative_buyins = cumsum(total_buyins),
    cumulative_prizes = cumsum(total_prizes),
    cumulative_ROI = ( cumulative_prizes - cumulative_buyins ) / cumulative_buyins ) %>% 
  mutate(Tournament = c(1:24) %>% as.factor())

gpp_wk_by_wk_roi_adjusted %>% 
  ggplot(aes(Tournament, ROI, fill = profit_color)) + 
  geom_col() +
  scale_fill_identity() +
  # scale_x_continuous(limits = c(1,24)) + 
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 1, big.mark = "")) +
  labs(
       y = "",
       title = "<b>Figure 2</b>
       <br>",
       subtitle = "<b><em>Adjusted DKPG Model ROI by Tournament</em></b>
       <br>
       <br>") +
  theme(
    text = element_text(family = "Calibri"),
    plot.title.position = "plot",
    plot.title = element_markdown(size = 10),
    plot.subtitle = element_markdown(size = 10)
    )

# week by week ROI adjusted model 
gpp_wk_by_wk_roi_adj_diff_form <- 
  optimal_lineup_gpp_results %>%
  filter(sim_type == "adjusted_course_fit_weighted_recent_form") %>%
  group_by(date, Model) %>% 
  summarise(
    total_buyins = sum(buy_in),
    total_prizes = sum(prize),
    ROI = (sum(prize) - sum(buy_in)) / sum(buy_in)
  ) %>%
  ungroup() %>% 
  arrange(date) %>% 
  mutate(profit_color = ifelse(ROI > 0, "#79ea86", "#e75757")) %>% 
  mutate(
    cumulative_buyins = cumsum(total_buyins),
    cumulative_prizes = cumsum(total_prizes),
    cumulative_ROI = ( cumulative_prizes - cumulative_buyins ) / cumulative_buyins ) %>% 
  mutate(Tournament = c(1:24) %>% as.factor())

gpp_wk_by_wk_roi_adj_diff_form %>% 
  ggplot(aes(Tournament, ROI, fill = profit_color)) + 
  geom_col() +
  scale_fill_identity() +
  # scale_x_continuous(limits = c(1,24)) + 
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 1, big.mark = "")) +
  labs(
    y = "",
    title = "<b>Figure 3</b>
    <br>",
    subtitle = "<b><em>Adjusted DKPG, Weighted Form, and Course Model ROI by Tournament</em></b>
    <br>
    <br>") +
  theme(
    text = element_text(family = "Calibri"),
    plot.title.position = "plot",
    plot.title = element_markdown(size = 10),
    plot.subtitle = element_markdown(size = 10)
  )

# join with actual double up results
optimal_lineup_gpp_results %>% 
  select(-c(buy_in, prize)) %>% 
  left_join(double_up_results_tbl, by = "tournament_id") %>% 
  mutate(prize = ifelse(total_lineup_points > cash_line, 25, 0)) %>%
  group_by(sim_type, lineup) %>% 
  summarise(roi = (( sum(prize) - sum(buy_in) ) / sum(buy_in)) * 100 ) %>% 
  View()

# compare results against historical closing lines for top 20's
# read in event ids from Data Golf API
event_ids <- read_csv("https://feeds.datagolf.com/get-event-list?tour=pga&file_format=csv&key=b202c023a12e11636f374a60e9e9")

# pull test ids that match 2019 tested tournaments used for model
testing_ids <- 
  event_ids %>% 
  filter(calendar_year == 2019) %>% 
  filter(event_id %in% c(3:27, 30:41, 100, 475, 480, 525)) %>% 
  filter(!event_id %in% c(5,20))

# pull event ids
ids <- 
  testing_ids %>% 
  pull(event_id)

# function to read in multiple files of historical odds 
read_outrights_function <- function(id) {
  
  read_csv(paste0("https://feeds.datagolf.com/historical-odds/outrights?tour=pga&event_id=", id, "&year=2019&market=top_20&book=bet365&odds_format=percent&file_format=csv&key=b202c023a12e11636f374a60e9e9"))
  
}

# pull historical odds for tested tournaments
outrights <- 
  map(ids, ~ (read_outrights_function(.x))) %>% 
  bind_rows()

# clean outrights data, join with salary and dk total fp data
outrights_cleaned <-
  outrights %>% 
  separate(player_name, into = c("last", "first"), sep = ",") %>% 
  mutate(player_name = str_c(first, last, sep = " ") %>% str_trim()) %>% 
  select(player_name, event_name, event_id, top_20_odds = close_odds) %>%
  mutate(event_name = case_when(
    event_name == "The Open Championship" ~ "The Open",
    event_name == "THE NORTHERN TRUST" ~ "The Northern Trust",
    event_name == "the Memorial Tournament presented by Nationwide" ~ "The Memorial Tournament pres. by Nationwide",
    event_name == "The Masters" ~ "Masters Tournament",
    event_name == "THE PLAYERS Championship" ~ "The Players Championship",
    event_name == "Arnold Palmer Invitational presented by Mastercard" ~ "Arnold Palmer Invitational Pres. by Mastercard",
    TRUE ~ event_name
  )) %>% 
  left_join(select(dk_results, tournament_id, tournament_name) %>% 
              distinct(), by = c("event_name" = "tournament_name")) %>% 
    mutate(player_name = case_when(
      player_name == "Ben Silverman" ~ "Benjamin Silverman",
      player_name == "Rafa Cabrera Bello" ~ "Rafael Cabrera-Bello",
      player_name == "Alex Noren" ~ "Alexander Noren",
      player_name == "Haotong Li" ~ "	Hao-Tong Li",
      player_name == "HaoTong Li" ~ "	Hao-Tong Li",
      player_name == "Jr. Potter" ~ "Ted Potter Jr.",
      player_name == "Ted Potter Jr" ~ "Ted Potter Jr.",
      player_name == "Erik van Rooyen" ~ "Erik Van Rooyen",
      player_name == "Michael Lorenzo-Vera" ~ "Mike Lorenzo-Vera",
      player_name == "Sangmoon Bae" ~ "Sang-Moon Bae",
      TRUE ~ player_name
    )) %>% 
  left_join(past_salaries, by = c("player_name", "tournament_id")) %>%
  left_join(select(dk_results, player, tournament_id, dk_total_fp) %>% 
              mutate(player = case_when(
                player == "Ben Silverman" ~ "Benjamin Silverman",
                player == "Rafa Cabrera Bello" ~ "Rafael Cabrera-Bello",
                player == "Alex Noren" ~ "Alexander Noren",
                player == "Haotong Li" ~ "	Hao-Tong Li",
                player == "HaoTong Li" ~ "	Hao-Tong Li",
                player == "Jr. Potter" ~ "Ted Potter Jr.",
                player == "Ted Potter Jr" ~ "Ted Potter Jr.",
                player == "Erik van Rooyen" ~ "Erik Van Rooyen",
                player == "Michael Lorenzo-Vera" ~ "Mike Lorenzo-Vera",
                player == "Sangmoon Bae" ~ "Sang-Moon Bae",
                TRUE ~ player
              )), by = c("player_name" = "player", "tournament_id")) %>% 
    filter(!salary %>% is.na() )

#build optimal lineup function for bet365 lineups
optimal_lineup_function <- function(df) {
  
  max_salary <- 50000
  max_players <-  6
  num_of_players <- length(df$player_name)
  salaries <- df$salary
  dk_top_20 <- df$top_20_odds
  
  MIPModel() %>%
    add_variable(x[i], i = 1:num_of_players, type = "binary") %>%
    add_constraint(sum_expr(x[i], i = 1:num_of_players) == max_players) %>%
    add_constraint(sum_expr(salaries[i] * x[i], i = 1:num_of_players) <= max_salary) %>%
    set_objective(sum_expr(dk_top_20[i] * x[i], i = 1:num_of_players), "max") %>%
    solve_model(with_ROI(solver = "glpk")) %>%
    get_solution(x[i]) %>%
    mutate(
      player_name = df$player_name,
      tournament_id = df$tournament_id,
      tournament_name = df$event_name,
      salary = df$salary,
      dk_total_fp = df$dk_total_fp
    ) %>%
    filter(value > 0) %>%
    select(tournament_name, tournament_id, player_name, salary, dk_total_fp)
  
}

# get optimal lineups
bet_365_optimal_lineups <- 
  outrights_cleaned %>% 
  mutate(tid = tournament_id) %>% 
  nest(data = -tid) %>% 
  pull(data) %>% 
  map(optimal_lineup_function) %>% 
  bind_rows() %>% 
  # fix some missing player scores - found at fantasylabs.com
  mutate(dk_total_fp = case_when(
    player_name == "Julian Suri" & tournament_name == "PGA Championship" ~ 21,
    player_name == "Chip McDaniel" & tournament_name == "Wells Fargo Championship" ~ 21,
    TRUE ~ dk_total_fp
  ))

# summarise bet365 lineups
bet365_lineup_totals <- 
  bet_365_optimal_lineups %>% 
  group_by(tournament_name, tournament_id) %>% 
  summarise(total_lineup_points = sum(dk_total_fp))

write_csv(bet365_lineup_totals, "analysis/simulation_results/bet_365_lineup_totals.csv")

# read in after total profits of bet365 optimals
bet_365_gpp_results <- read_csv("analysis/simulation_results/bet_365_lineup_totals.csv")

bet_365_gpp_roi <- 
  bet_365_gpp_results %>% 
  summarise(`Total Buyins` = sum(buy_in),
            `Total Prizes` = sum(prize),
            ROI = ((sum(prize) - sum(buy_in)) / sum(buy_in)))


bet_365_double_up_results <- 
  bet365_lineup_totals %>% 
  left_join(select(double_up_results_tbl, tournament_id, cash_line)) %>% 
  mutate(buy_in = 25) %>% 
  mutate(prize = ifelse(total_lineup_points > cash_line, 50, 0))

bet_365_double_up_roi <- 
  bet_365_double_up_results %>% 
  summarise(ROI = ((sum(prize) - sum(buy_in)) / sum(buy_in)))


