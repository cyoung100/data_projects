library(tidyverse)
library(janitor)
library(lubridate)
library(here)

bed_occupancy <- read_csv(here("raw_data/bed_occupancy.csv"))

bed_occupancy_clean <- bed_occupancy %>% 
  clean_names() %>% 
  filter(quarter != "2017Q4") %>% 
  filter(location_qf == "d") %>% 
  select(-c(
    all_staffed_beddays_qf, 
    total_occupied_beddays_qf,
    average_available_staffed_beds_qf,
    average_occupied_beds_qf,
    percentage_occupancy_qf,
    location,
    location_qf,
    specialty,
    specialty_qf
    )) %>% 
  mutate(quarter_date = case_match(quarter,
                                   "2018Q1" ~ "2018-03-31",
                                   "2018Q2" ~ "2018-06-30",
                                   "2018Q3" ~ "2018-09-30",
                                   "2018Q4" ~ "2018-12-31",
                                   "2019Q1" ~ "2019-03-31",
                                   "2019Q2" ~ "2019-06-30",
                                   "2019Q3" ~ "2019-09-30",
                                   "2019Q4" ~ "2019-12-31",
                                   "2020Q1" ~ "2020-03-31",
                                   "2020Q2" ~ "2020-06-30",
                                   "2020Q3" ~ "2020-09-30",
                                   "2020Q4" ~ "2020-12-31",
                                   "2021Q1" ~ "2021-03-31",
                                   "2021Q2" ~ "2021-06-30",
                                   "2021Q3" ~ "2021-09-30",
                                   "2021Q4" ~ "2021-12-31",
                                   "2022Q1" ~ "2022-03-31",
                                   "2022Q2" ~ "2022-06-30",
                                   "2022Q3" ~ "2022-09-30",
                                   "2022Q4" ~ "2022-12-31"
                                   )) %>% 
  mutate(year_quarter = str_c(str_sub(quarter, 1, 4),
                              str_sub(quarter, 5, 6),
                              sep = " ")) %>%
  mutate(quarter_date = ymd(quarter_date)) %>% 
  mutate(year = year(quarter_date), quarter = str_sub(year_quarter, 6, 7)) %>% 
  mutate(season = case_match(quarter, "Q1" ~ "winter", "Q4" ~ "winter",
                             "Q2" ~ "summer", "Q3" ~ "summer")) %>% 
  mutate(covid = if_else(year > 2019, TRUE, FALSE))

write_csv(bed_occupancy_clean, here("clean_data/bed_occupancy.csv"))
