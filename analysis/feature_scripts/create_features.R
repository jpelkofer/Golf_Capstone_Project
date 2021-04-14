library(tidyverse)
library(timetk)

# holes <- holes_tbl
overall_round_features <- function(holes) {
  # this function creates features at the round level
  # Example: overall birdie rate = sum of all birdies by all players who played round / sum of all holes played by all players in round
  
  holes %>%
    filter(round != 5) %>%
    unite("course_rd_id", c(season:course_id, round)) %>%
    group_by(course_rd_id) %>%
    summarise(
      total_holes = n(),
      birdies_or_better = sum(score_type == "BIRDIE") + sum(score_type == "EAGLE") + sum(score_type == "DOUBLE_EAGLE"),
      bogeys_or_worse = sum(score_type == "BOGEY") + sum(score_type == "DOUBLE_BOGEY") + sum(score_type == "TRIPLE_BOGEY") +
        sum(score_type == "OTHER"),
      eagles_or_better = sum(score_type == "EAGLE") + sum(score_type == "DOUBLE_EAGLE"),
      double_or_worse = sum(score_type == "DOUBLE_BOGEY") + sum(score_type == "TRIPLE_BOGEY"),
      birdie_or_better_rate = birdies_or_better / total_holes,
      bogeys_or_worse_rate = bogeys_or_worse / total_holes,
      eagles_or_better_rate = eagles_or_better / total_holes,
      double_or_worse_rate = double_or_worse / total_holes
    ) %>%
    ungroup()
  
}

player_smoothing_features <- function(holes) {
  # this function creates for players at the round level
  # Example: A loess smoothing algorithm is applied to a players birdie rate over time
  
  holes %>%
    filter(round != 5) %>%
    unite("course_rd_id", c(season:course_id, round)) %>%
    group_by(player_name, date, course_rd_id) %>%
    summarise(
      total_holes = n(),
      birdies_or_better = sum(score_type == "BIRDIE") + sum(score_type == "EAGLE") + sum(score_type == "DOUBLE_EAGLE"),
      bogeys_or_worse = sum(score_type == "BOGEY") + sum(score_type == "DOUBLE_BOGEY") + sum(score_type == "TRIPLE_BOGEY") +
        sum(score_type == "OTHER"),
      eagles_or_better = sum(score_type == "EAGLE") + sum(score_type == "DOUBLE_EAGLE"),
      double_or_worse = sum(score_type == "DOUBLE_BOGEY") + sum(score_type == "TRIPLE_BOGEY"),
      dk_hole_pts_in_rd = sum(dk_pts_classic),
      birdie_or_better_rate = birdies_or_better / total_holes,
      bogeys_or_worse_rate = bogeys_or_worse / total_holes,
      eagles_or_better_rate = eagles_or_better / total_holes,
      double_or_worse_rate = double_or_worse / total_holes
    ) %>%
    ungroup() %>%
    filter(total_holes == 18) %>%
    arrange(date) %>% 
    group_by(player_name) %>%
    mutate(
      across(
        .cols = dk_hole_pts_in_rd:double_or_worse_rate,
        .fns = ~ smooth_vec(.x, period = 50, degree = 1),
        .names = "{.col}_smoothed"
      )
    ) %>%
    ungroup()
  
}


overall_hole_features <- function(holes) {
  
  # this function creates features at the hole level
  # Example: overall birdie rate = sum of all birdies by all players who played hole / total times hole was played in round
  
  holes %>% 
    filter(round != 5) %>%
    filter(par < 6) %>% 
    unite("course_rd_hole_id", c(season:course_id, round, hole)) %>%
    group_by(course_rd_hole_id) %>% 
    summarise(
      total_holes = n(),
      birdies_or_better = sum(score_type == "BIRDIE") + sum(score_type == "EAGLE") + sum(score_type == "DOUBLE_EAGLE"),
      bogeys_or_worse = sum(score_type == "BOGEY") + sum(score_type == "DOUBLE_BOGEY") + sum(score_type == "TRIPLE_BOGEY") +
        sum(score_type == "OTHER"),
      eagles_or_better = sum(score_type == "EAGLE") + sum(score_type == "DOUBLE_EAGLE"),
      double_or_worse = sum(score_type == "DOUBLE_BOGEY") + sum(score_type == "TRIPLE_BOGEY"),
      birdie_or_better_rate = birdies_or_better / total_holes,
      bogeys_or_worse_rate = bogeys_or_worse / total_holes,
      eagles_or_better_rate = eagles_or_better / total_holes,
      double_or_worse_rate = double_or_worse / total_holes
    ) %>%
    ungroup() %>% 
    left_join(select(holes, season:course_id, round:yards, field_dk_classic_avg) %>% 
                filter(round != 5) %>%
                filter(par < 6) %>% 
                unite("course_rd_id", c(season:course_id, round), remove = FALSE) %>% 
                unite("course_rd_hole_id", c(season:course_id, round, hole)) %>% 
                        distinct(), by = "course_rd_hole_id")
}


  
  


