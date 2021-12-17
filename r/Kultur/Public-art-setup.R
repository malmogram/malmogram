# ---
# Title: PUblic art. Malmö art map and Wikipedia page - setup
# Purpose: 
# Start date: 211211
# Comments:
# ---

library(tidyverse)
library(rvest)
library(sf)
# path <- "https://kartor.malmo.se/geoserver/wms?service=WMS&request=GetFeatureInfo&bbox=6151005.213758019%2C105336.01219551433%2C6171900.052771502%2C128384.18630059948&layers=malmows%3AGK_SKULPTURER_PT&styles=&typename=malmows%3AGK_SKULPTURER_PT&query_layers=malmows%3AGK_SKULPTURER_PT&info_format=application%2Fjson&feature_count=4000000&x=10&y=10&buffer=500&width=20&height=20&srs=EPSG%3A3008&exceptions=application%252Fvnd.ogc.se_xml&version=1.3.0"
# dl <- st_read(path)
# 
# st_write(dl, "Data/Kultur/Offentlig_konst.geoJSON")

# Import from local file ----
dat_art_geo <- st_read("Data/Kultur/Offentlig_konst.geoJSON")

dat_art <- dat_art_geo %>%
  mutate(X = st_coordinates(dat_art_geo)[,1],
         Y = st_coordinates(dat_art_geo)[,2],
         Årtionde = 10 * floor(as.numeric(avtackningsar) / 10)) %>%
  as_tibble()

# Wikipedia page
url <- "https://sv.wikipedia.org/wiki/Lista_%C3%B6ver_offentlig_konst_i_Malm%C3%B6_kommun"

tabs <- read_html(url) %>% 
  html_table()

dat_wiki_art <- tibble(tabs = tabs, antal = map_dbl(tabs, ~ dim(.x)[1])) %>% 
  slice(2:12) %>% 
  unnest_wider(tabs) %>% 
  mutate(Årtal = map(Årtal, as.character)) %>% 
  unnest(everything())

layers <-  st_layers("Data/Kultur/wikipedia-offentlig-konst-malmo-211211.kml") %>% unlist() %>% `[`(1:11)
dat_temp <- st_read("Data/Kultur/wikipedia-offentlig-konst-malmo-211211.kml", layers[1]) %>% mutate(Område = layers[1])
for(i in 2:11){dat_temp <- bind_rows(dat_temp, st_read("Data/Kultur/wikipedia-offentlig-konst-malmo-211211.kml", layers[i]) %>% mutate(Område = layers[i]))}

dat_wiki_art_geo <- dat_temp %>% 
  separate(Name, c("Namn", "Rest"), sep = " \\(", extra = "merge") %>% 
  separate(Rest, c("År", "Konstnär"), sep = "\\) ", extra = "merge") %>% 
  select(-Description)
       