library(tidyverse)

tournament_ids <- 
  tournaments_to_sim %>%
  arrange(date) %>% 
  pull(tournament_id)

salary_links <- c("https://www.fantasylabs.com/api/contest-ownership/5/1_13_2019/4/23959/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/1_27_2019/4/24274/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/2_3_2019/4/24390/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/2_17_2019/4/24434/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/3_3_2019/4/24962/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/3_10_2019/4/25167/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/3_17_2019/4/25035/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/3_24_2019/4/25504/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/4_7_2019/4/25910/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/4_14_2019/4/25452/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/4_21_2019/4/26336/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/5_5_2019/4/26545/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/5_12_2019/4/26863/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/5_19_2019/4/26337/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/5_26_2019/4/27191/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/6_2_2019/4/27319/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/6_9_2019/4/27450/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/6_16_2019/4/27192/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/6_23_2019/4/27764/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/6_30_2019/4/27908/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/7_7_2019/4/28064/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/7_14_2019/4/28169/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/7_21_2019/4/27765/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/8_4_2019/4/28594/0/",
                  "https://www.fantasylabs.com/api/contest-ownership/5/8_11_2019/4/28776/0/")

scrape_salaries <- function(tournament_id, link) {

  salary_data <-
    jsonlite::read_json(link)
  
  salary_data %>%
    enframe() %>%
    unnest_wider(value) %>%
    select(Properties) %>%
    unnest_wider(Properties) %>%
    janitor::clean_names() %>%
    select(player_name, salary) %>%
    mutate(tournament_id = tournament_id)
}

player_salaries <- map2_dfr(tournament_ids, salary_links, scrape_salaries)

# check for 25 distinct tournament ids
player_salaries %>% 
  distinct(tournament_id) %>% 
  count()

write_csv(player_salaries, "analysis/simulation_results/player_salaries.csv")
