library(tidyverse)
library(lubridate)
library(ragg)
library(systemfonts)

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

# write csv will manually update with actuall gpp results
write_csv(optimal_lineup_totals, "analysis/simulation_results/optimal_lineup_totals.csv")

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

write_rds(roi_sim_lineup, "misc/roi_sim_lineup.rds")

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
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 1, big.mark = ",")) +
  labs(
       y = "",
       title = "Figure 2",
       subtitle = "Adjusted DKPG Model ROI by Tournament") +
  theme(
    text = element_text(family = "Calibri"),
    plot.title.position = "plot"
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
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 1, big.mark = ",")) +
  labs(
    y = "",
    title = "Figure 3",
    subtitle = "Adjusted DKPG, Weighted Form, and Course Model ROI by Tournament") +
  theme(
    text = element_text(family = "Calibri"),
    plot.title.position = "plot"
  )

# join with actual double up results
optimal_lineup_gpp_results %>% 
  select(-c(buy_in, prize)) %>% 
  left_join(double_up_results_tbl, by = "tournament_id") %>% 
  mutate(prize = ifelse(total_lineup_points > cash_line, 25, 0)) %>%
  group_by(sim_type, lineup) %>% 
  summarise(roi = (( sum(prize) - sum(buy_in) ) / sum(buy_in)) * 100 ) %>% 
  View()
