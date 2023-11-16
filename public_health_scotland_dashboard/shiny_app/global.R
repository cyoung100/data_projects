library(shiny)
library(tidyverse)
library(leaflet)
library(thematic)
library(shinythemes)
library(here)


hb_codes <- c("S08000015", "S08000016", "S08000017", "S08000029", "S08000019",
              "S08000020", "S08000031", "S08000022", "S08000032", "S08000024",
              "S08000025", "S08000026", "S08000030", "S08000028")

hb_names <- c("Ayrshire and Arran", "Borders","Dumfries and Galloway",
              "Fife", "Forth Valley", "Grampian", "Greater Glasgow and Clyde",
              "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland",
              "Tayside", "Western Isles")

hb_codes_with_names <- c("Ayrshire and Arran" = "S08000015", 
                         "Borders" = "S08000016",
                         "Dumfries and Galloway" = "S08000017",
                         "Fife" = "S08000029",
                         "Forth Valley" = "S08000019",
                         "Grampian" = "S08000020",
                         "Greater Glasgow and Clyde" = "S08000031",
                         "Highland" = "S08000022",
                         "Lanarkshire" = "S08000032",
                         "Lothian" = "S08000024",
                         "Orkney" = "S08000025",
                         "Shetland" = "S08000026",
                         "Tayside" = "S08000030",
                         "Western Isles" = "S08000028")

default_font_base_size = 14

# read in data
## Health Board Multipolygon data
hb_polygons <- sf::st_read(here("clean_data/hb_polygons/hb.shp"))

# Age & Sex
age_sex <- read_csv(here("clean_data/age_and_sex_without_duplicates.csv"))

# Deprivation
deprivation <- read_csv(here("clean_data/deprivation_without_duplicates.csv"))

# Wait times
wait <- read_csv(here("clean_data/waiting_times_condense.csv"))

waiting_map <- left_join(hb_polygons, wait, "hb_name")
  
# Bed occupancy
bed_occupancy <- read_csv(here("clean_data/bed_occupancy.csv"))




