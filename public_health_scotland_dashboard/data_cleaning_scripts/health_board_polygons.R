hb <- sf::st_read("raw_data/SG_NHS_HealthBoards_2019/SG_NHS_HealthBoards_2019.shp") %>% 
  janitor::clean_names()

# Simplify multipolygons to make plotting quicker
hb_simpl <- sf::st_simplify(hb,
                            preserveTopology = FALSE, 
                            dTolerance = 2000) 

# Save as sdhapefile
sf::st_write(obj = hb_simpl, dsn = "clean_data/hb_polygons/hb.shp", driver = "ESRI Shapefile")

rm(hb, hb_simpl)