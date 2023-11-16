library(tidyverse)

raw_age_sex_id <- 
  
  read_csv("raw_data/inpatient_and_daycase_by_nhs_board_of_treatment_age_and_sex.csv") %>% 
  
  janitor::clean_names() %>%
  
  filter(admission_type %in%
           c("Elective Inpatients", "All Day cases", "Emergency Inpatients")) %>% 
  
  ### removing duplicate rows
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate_if(is.character, ~replace_na(., "N")) %>%
  mutate(id = str_c(quarter, hb, admission_type, sex, age,
                    episodes, length_of_episode, average_length_of_episode,
                    stays, length_of_stay, average_length_of_stay))

repeated_id <- raw_age_sex_id %>%
  distinct(id)

age_sex_without_duplicates <-
  left_join(repeated_id, raw_age_sex_id, by = "id", multiple = "first") %>%
  select(quarter, hb, admission_type, sex, age, stays, average_length_of_stay) %>%
  ###
  
  mutate(year_quarter = tsibble::yearquarter(quarter),
         year = year(year_quarter),
         .after = quarter) %>% 
  
  mutate(age_range = case_when(age == "0-9 years" ~ "0-19",
                               age == "10-19 years" ~ "0-19",
                               age == "20-29 years" ~ "20-39",
                               age == "30-39 years" ~ "20-39",
                               age == "40-49 years" ~ "40-59",
                               age == "50-59 years" ~ "40-59",
                               age == "60-69 years" ~ "60-79",
                               age == "70-79 years" ~ "60-79",
                               age == "80-89 years" ~ "80 and over",
                               age == "90 years and over" ~ "80 and over")) %>% 
  
  select(year_quarter, year, hb, admission_type, sex, age, age_range, 
         stays, average_length_of_stay) %>% 
  
  mutate(winter = case_when(str_detect(year_quarter, "Q1") ~ TRUE,
                            str_detect(year_quarter, "Q4") ~ TRUE,
                            .default = FALSE),
         post_covid = year > 2019) %>% 
  
  filter(year >= 2018)

write_csv(age_sex_without_duplicates, file = "clean_data/age_and_sex_without_duplicates.csv")

rm(raw_age_sex_id, repeated_id, age_sex_without_duplicates)