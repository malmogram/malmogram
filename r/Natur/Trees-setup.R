# ---
# Title: Malmö municipality tree map - download and setup 
# Purpose: 
# Start date: 211128
# Comments:
# ---

# Packages ----
library(sf)
library(rvest)
library(tidyverse)

# Scraping ----
# a <- "https://kartor.malmo.se/geoserver/wms?service=WMS&request=GetFeatureInfo&bbox=6140000.0,114000.7,6170000.6,125000.9&layers=malmows:gk_trad_barr_pt,malmows:gk_trad_lov_pt,malmows:gk_trad_exotiska_pt,malmows:gk_trad_nordiska_pt&styles=&typename=malmows:gk_trad_barr_pt,malmows:gk_trad_lov_pt,malmows:gk_trad_exotiska_pt,malmows:gk_trad_nordiska_pt&query_layers=malmows:gk_trad_barr_pt,malmows:gk_trad_lov_pt,malmows:gk_trad_exotiska_pt,malmows:gk_trad_nordiska_pt&info_format=application/json&feature_count=4000000&x=10&y=10&buffer=500&width=20&height=20&srs=EPSG:3008&exceptions=application%2Fvnd.ogc.se_xml&version=1.3.0"
# b <- st_read(a)
# 
# st_write(b, "Data/Natur/Trees.geoJSON")

# Import from local file ----
dat_trees_geo <- st_read("Data/Natur/Trees.geoJSON")

dat_trees <- dat_trees_geo %>%
  mutate(X = st_coordinates(dat_trees_geo)[,1],
         Y = st_coordinates(dat_trees_geo)[,2],
         Årtionde = 10 * floor(planteringsar / 10)) %>%
  as_tibble() %>% 
  filter(X < 130000)
