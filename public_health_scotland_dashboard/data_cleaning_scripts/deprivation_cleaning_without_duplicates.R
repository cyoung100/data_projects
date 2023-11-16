library(tidyverse)

deprivation <- 
  
  read_csv("raw_data/inpatient_and_daycase_by_nhs_board_of_residence_and_simd.csv") %>% 
  
  janitor::clean_names() %>% 
  
  filter(admission_type %in%
           c("Elective Inpatients", "All Day cases", "Emergency Inpatients")) %>% 
  
  ### removing duplicate rows
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate_if(is.character, ~replace_na(., "N")) %>%
  mutate(id = str_c(quarter, hb, admission_type, simd,
                    stays, average_length_of_stay))

repeated_id <- deprivation %>%
  distinct(id)

deprivation_without_duplicates <-
  left_join(repeated_id, deprivation, by = "id", multiple = "first") %>%
  select(quarter, hb, admission_type, simd, stays, average_length_of_stay) %>%
  ###
  
  mutate(year_quarter = tsibble::yearquarter(quarter),
         year = year(year_quarter),
         .after = quarter) %>% 
  
  select(year_quarter, year, hb, admission_type, simd, 
         stays, average_length_of_stay) %>% 

  filter(simd > 0,
         year >= 2018) %>% 
  
  mutate(winter = case_when(str_detect(year_quarter, "Q1") ~ TRUE,
                            str_detect(year_quarter, "Q4") ~ TRUE,
                            .default = FALSE),
         post_covid = year > 2019)

write_csv(deprivation_without_duplicates, file = "clean_data/deprivation_without_duplicates.csv")

rm(deprivation, repeated_id, deprivation_without_duplicates)
