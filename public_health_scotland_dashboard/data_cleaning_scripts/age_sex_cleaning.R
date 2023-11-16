library(tidyverse)

age_sex <- 
  
  read_csv("raw_data/inpatient_and_daycase_by_nhs_board_of_treatment_age_and_sex.csv") %>% 
  
  janitor::clean_names() %>%
  
  filter(admission_type %in%
           c("Elective Inpatients", "All Day cases", "Emergency Inpatients")) %>% 
  
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

  select(year_quarter, year, hb, location, admission_type, sex, age, age_range, 
         stays, average_length_of_stay) %>% 
 
  mutate(winter = case_when(str_detect(year_quarter, "Q1") ~ TRUE,
                            str_detect(year_quarter, "Q4") ~ TRUE,
                            .default = FALSE),
         post_covid = year > 2019)

write_csv(age_sex, file = "clean_data/age_and_sex.csv")

