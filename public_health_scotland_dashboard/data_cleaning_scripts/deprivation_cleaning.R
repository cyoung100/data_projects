library(tidyverse)

deprivation <- 
  
  read_csv("raw_data/inpatient_and_daycase_by_nhs_board_of_residence_and_simd.csv") %>% 
  
  janitor::clean_names() %>% 
  
  filter(admission_type %in%
           c("Elective Inpatients", "All Day cases", "Emergency Inpatients")) %>% 
  
  mutate(year_quarter = tsibble::yearquarter(quarter),
         year = year(year_quarter),
         .after = quarter) %>% 
  
  select(year_quarter, year, hb, admission_type, simd, 
         stays, average_length_of_stay) %>% 

  filter(simd > 0) %>% 
  
  mutate(winter = case_when(str_detect(year_quarter, "Q1") ~ TRUE,
                            str_detect(year_quarter, "Q4") ~ TRUE,
                            .default = FALSE),
         post_covid = year > 2019)

write_csv(deprivation, file = "clean_data/deprivation.csv")

