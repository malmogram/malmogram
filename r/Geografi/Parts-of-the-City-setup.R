# ---
# Title: Data collection for mgs on parts of Malmö
# Purpose: 
# Start date: 211009
# Comments:
# ---

# Packages ----
library(tidyverse)
library(osmdata)
library(sf)

# Malmö bbox ----
m_bbox <- c(xmin = 12.8806980032474, ymin = 55.4908024744868, 
            xmax = 13.1517619319314, ymax = 55.641516936621)

# Administrative levels ----
dat_adm <- opq(m_bbox) %>%
  add_osm_feature(key = "admin_level") %>%
  osmdata_sf()

dat_adm_mpol <- dat_adm$osm_multipolygons
Encoding(dat_adm_mpol$name) <- "UTF-8"

# Delområden, read from Malmö Stad GIS map ----
dat_temp <- read_csv("Data/Geografi/Data-Parts-of-the-City-Delområden.csv")

dat_adm_mpol <- dat_adm_mpol %>% 
  filter(admin_level == 10) %>% 
  left_join(dat_temp)

dat_adm_delomr <- dat_adm_mpol %>% 
  group_by(stadsdel) %>% 
  summarise()

# DeSO. SCB, downloaded file from https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/deso--demografiska-statistikomraden/ ----
dat_deso <- read_sf("Data/Geografi/deso_2018_v2/DeSO_2018_v2.gpkg") %>%  
  filter(kommun == 1280)

# Main streets. OSM ----
dat_roads <- opq(m_bbox) %>%
  add_osm_feature(key = "highway", value = c("motorway", "motorway_link", "primary", 
                                             "secondary", "tertiary")) %>%
  osmdata_sf() %>% 
  `[[`("osm_lines")


