---
  title: "A and E Waiting times"
output:
  html_document:
  df_print: paged
---

library(tidyverse)
library(lubridate)
library(infer)
waiting_times <- janitor::clean_names(read_csv("../raw_data/monthly_ae_activity_202307.csv"))
names(waiting_times)

# Extract time info
waiting_times_clean <- waiting_times %>% 
  mutate(
    date = ymd(month, truncated = 2)
  ) 

# remove unwanted columns
waiting_times_clean <- waiting_times_clean %>%   
  select(-number_within4hours_episode_qf, -number_of_attendances_episode_qf,
         -number_over4hours_episode_qf, -percentage_over8hours_episode_qf,
         -number_over8hours_episode_qf,percentage_over12hours_episode_qf)

waiting_times_clean <- waiting_times_clean 

# rename healthboards
waiting_times_clean <- waiting_times_clean %>% 
  mutate(health_board = case_when(
    hbt %in% "S08000015" ~ "Ayrshire and Arran",
    hbt %in% "S08000016" ~ "Borders",
    hbt %in% "S08000017" ~ "Dumfries and Galloway",
    hbt %in% "S08000029" ~ "Fife",
    hbt %in% "S08000019" ~ "Forth Valley",
    hbt %in% "S08000020" ~ "Grampian",
    hbt %in% "S08000031" ~ "Greater Glasgow and Clyde",
    hbt %in% "S08000022" ~ "Highland",
    hbt %in% "S08000032" ~ "Lanarkshire",
    hbt %in% "S08000024" ~ "Lothian",
    hbt %in% "S08000025" ~ "Orkney",
    hbt %in% "S08000026" ~ "Shetland",
    hbt %in% "S08000030" ~ "Tayside",
    hbt %in% "S08000028" ~ "Western Isles",
    T ~ country
  )
  )

# make new column with yearly quarter data
waiting_times_clean <- waiting_times_clean %>% 
  mutate(
    year_quarter = paste0(year(date),
                          " Q",
                          quarter(date)),
    year = year(date),
    quarter = quarter(date)
  )

# group into winter/non-winter based on quarter
waiting_times_clean <- waiting_times_clean %>% 
  mutate(winter = case_when(str_detect(year_quarter, "Q1") ~ "Winter",
                            str_detect(year_quarter, "Q4") ~ "Winter",
                            .default = "Non-Winter"),
         post_covid = year > 2019)

# filter to remove unwanted years
waiting_times_clean <- waiting_times_clean %>% 
  filter(year > 2017)

# make new col with percentage of waits
waiting_times_clean <- waiting_times_clean %>% 
  group_by(year_quarter) %>% 
  mutate(q_percent_within_4_hours = (number_within4hours_all/number_of_attendances_all)* 100,
         q_percent_over_4_hours = (number_over4hours_all/number_of_attendances_all)* 100,
         q_percent_above_8_hours = (number_over8hours_episode/number_of_attendances_all)* 100,
         q_percent_above_12_hours = (number_over12hours_episode/number_of_attendances_all)* 100)

# rename variables to match elsewhere
waiting_times_clean <- waiting_times_clean %>% 
  mutate(
    hb_name = health_board,
    hb = hbt)

# rename and save
waiting_times_condense <- waiting_times_clean

write.csv(waiting_times_condense, file = "../clean_data/waiting_times_condense.csv")

