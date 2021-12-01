file <- "C:/eg/temp_c/geoserver-GetFeatureInfo.txt"

library(sf)

a <- st_read(file)
plot(a)
a %>% as_tibble() %>% count(svenskt_namn, sort = T)

ggplot(a, aes(color = svenskt_namn)) + geom_sf(shape = ".") + theme(legend.position = "none")

a %>% 
  mutate(X = st_coordinates(a)[,1], Y = st_coordinates(a)[,2]) %>% 
  as_tibble() %>% 
  ggplot(aes(X, Y, col = svenskt_namn)) +
  geom_point(shape = ".") +
  theme(legend.position = "none") +
  coord_equal()

# https://kartor.malmo.se/geoserver/wms?service=WMS&request=GetFeatureInfo&bbox=6140000.0,114000.7,6170000.6,125000.9&layers=malmows:gk_trad_barr_pt,malmows:gk_trad_lov_pt,malmows:gk_trad_exotiska_pt,malmows:gk_trad_nordiska_pt&styles=&typename=malmows:gk_trad_barr_pt,malmows:gk_trad_lov_pt,malmows:gk_trad_exotiska_pt,malmows:gk_trad_nordiska_pt&query_layers=malmows:gk_trad_barr_pt,malmows:gk_trad_lov_pt,malmows:gk_trad_exotiska_pt,malmows:gk_trad_nordiska_pt&info_format=application/json&feature_count=4000000&x=50&y=50&buffer=50&width=100&height=100&srs=EPSG:3008&exceptions=application%2Fvnd.ogc.se_xml&version=1.3.0