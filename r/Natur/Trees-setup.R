library(sf)
library(rvest)
library(tidyverse)

a <- "https://kartor.malmo.se/geoserver/wms?service=WMS&request=GetFeatureInfo&bbox=6140000.0,114000.7,6170000.6,125000.9&layers=malmows:gk_trad_barr_pt,malmows:gk_trad_lov_pt,malmows:gk_trad_exotiska_pt,malmows:gk_trad_nordiska_pt&styles=&typename=malmows:gk_trad_barr_pt,malmows:gk_trad_lov_pt,malmows:gk_trad_exotiska_pt,malmows:gk_trad_nordiska_pt&query_layers=malmows:gk_trad_barr_pt,malmows:gk_trad_lov_pt,malmows:gk_trad_exotiska_pt,malmows:gk_trad_nordiska_pt&info_format=application/json&feature_count=4000000&x=10&y=10&buffer=500&width=20&height=20&srs=EPSG:3008&exceptions=application%2Fvnd.ogc.se_xml&version=1.3.0"
# a <- a %>% readLines()
b <- st_read(a)
b

st_coordinates(b) %>% 
  as_tibble() %>% 
  ggplot(aes(X, Y, color = b$ursprung)) + 
  geom_point(size = 0.1) + 
  coord_equal() + 
  cowplot::theme_nothing()

dat <- b %>% 
  mutate(X = st_coordinates(b)[,1],
         Y = st_coordinates(b)[,2],
         Årtionde = 10 * floor(planteringsar / 10))

g1 <- ggplot(dat, aes(X, Y)) +
  geom_point(col = "red") +
  facet_wrap(~ Årtionde)
g1

b

st_write(b, "Data/Natur/Trees.geoJSON")
