# ---
# Title: Setup for weather
# Purpose: 
# Start date: 211017
# Comments:
# ---

# Packages ----
library(tidyverse)
library(lubridate)
library(magrittr)
library(osmdata)
library(sf)

# Import. SMHI open weather station data
dat_weatherstation <- tibble(Station = c("c", "b", "a"),
                             file = c("smhi-opendata_1_52350_20211017_173216.csv", 
                                      "smhi-opendata_1_53360_20211017_173207.csv", 
                                      "smhi-opendata_1_53370_20211017_173159.csv"),
                             skip = c(10, 10, 9)) %>% 
  mutate(data = map2(file, skip, ~ read_csv2(paste0("Data/Klimat/", .x), skip = .y))) %>% 
  unnest(data) %>%
  mutate(Lufttemperatur = as.numeric(Lufttemperatur)) %>% 
  select(1,3,4,5,6)

dat_meta_station <- tibble(Station = c("a", "b", "c"),
                          Startdatum = map(Station, ~ dat_weatherstation %>% 
                                                 filter(Station == .x) %$% 
                                                 min(Datum)),
                          Slutdatum = map(Station, ~ dat_weatherstation %>% 
                                             filter(Station == .x) %$% 
                                             max(Datum))) %>% 
  unnest(c(Startdatum, Slutdatum)) %>% 
  mutate(lat = c(55.6048,55.5782,55.5715),
         long = c(12.9841,13.0141,13.0708)) %>% 
  st_as_sf(crs = 4326, coords = c("long", "lat"))

# Malmö bbox ----
m_bbox <- c(xmin = 12.8806980032474, ymin = 55.4908024744868, 
            xmax = 13.1517619319314, ymax = 55.641516936621)

# Administrative levels ----
dat_adm <- opq(m_bbox) %>%
  add_osm_feature(key = "admin_level", value = 10) %>%
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

ggplot() + 
  geom_sf(aes(fill = stadsdel), data = dat_adm_delomr, alpha = 0.4) +
  geom_sf(data = dat_meta_station) + geom_sf_text(aes(label = Station), nudge_x = 0.005, data = dat_meta_station)

dat_weatherstation %>% 
  group_by(year(Datum), Station) %>% 
  summarise(Lufttemperatur = mean(Lufttemperatur)) %>% 
  ggplot(aes(`year(Datum)`, Lufttemperatur, col = Station)) + geom_line()

dat_temp <- dat_weatherstation %>% 
  filter(Station == "c") %>% 
  mutate(yday = yday(Datum), year = year(Datum)) %>%
  group_by(yday, year) %>% 
  summarise(Lufttemperatur = mean(Lufttemperatur)) 

dat_temp %>% 
  ggplot(aes(yday, Lufttemperatur, group = year)) + 
  geom_line(col = "grey60") +
  geom_line(data = dat_temp %>% filter(year > 2014))

dat_weatherstation %>% 
  filter(Station == "c") %>% 
  drop_na() %>% 
  ggplot(aes(yday(Datum), `Tid (UTC)`)) + geom_point() + facet_wrap(~ year(Datum))

dat_weatherstation %>% 
  filter(Station == "c") %>% 
  mutate(hour = hour(`Tid (UTC)`),
         yday = yday(Datum),
         year = year(Datum)) %>% 
  group_by(hour, yday) %>% 
  summarise(Lufttemperatur = mean(Lufttemperatur), na.rm = T) %>% 
  ggplot(aes(y = yday, hour, fill = Lufttemperatur)) +
  geom_tile() +
  scale_fill_binned()

dat_weatherstation %>% 
  filter(Station == "c") %>% 
  mutate(hour = hour(`Tid (UTC)`),
         yday = yday(Datum),
         year = year(Datum)) %>% 
  filter(year %in% 2015:2019) %>% 
  ggplot(aes(Datum, hour, fill = Lufttemperatur)) +
  geom_tile() +
  scale_fill_steps2(n.breaks = 5, high = "red", low = "blue", midpoint = 10)
