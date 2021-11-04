# ---
# Title: Geography. Parts of the city. Plots
# Purpose: 
# Start date: 211016
# Comments:
# ---

# Packages ---
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(spData)


# Read data script
source("r/Geografi/Parts-of-the-City-setup.R", encoding = "UTF-8")

# Map. Roads and main areas. mg15 ----
mg15 <- ggplot() +
  geom_sf(data = dat_roads, col = "grey50") +
  geom_sf(data = dat_adm_delomr, aes(fill = stadsdel), alpha = 0.4) +
  theme_mg3()
mg15

ggmap(get_stamenmap(as.vector(m_bbox), maptype = "terrain-background", zoom = 12)) +
  geom_sf(data = dat_roads, inherit.aes = F, alpha = 0.4) +
  geom_sf(data = dat_adm_delomr, aes(fill = stadsdel), 
          inherit.aes = F, alpha = 0.4)

# Map. Roads and city parts, mg 16 ----
mg16 <- ggplot() +
  # geom_sf(data = dat_roads) +
  geom_sf(data = dat_adm_delomr, aes(fill = stadsdel), alpha = 0.2) +
  geom_sf(data = dat_adm_mpol, fill = NA) +
  geom_sf_text(data = dat_adm_mpol, aes(label = name), size = 1) +
  theme_mg3()
mg16

# Roundest city part, mg 18 ----
dat_temp <- dat_adm_mpol %>% 
  select(name, stadsdel) %>% 
  st_transform(3006) %>% 
  mutate(geo = map(geometry, ~ as.matrix(st_coordinates(.x)))) %>% 
  as_tibble() %>% 
  select(-geometry) %>%
  mutate(geo = map(geo, ~ as_tibble(.x[,1:2]))) %>% 
  unnest(geo) %>% 
  group_by(name) %>% 
  mutate(xmin = min(X), xmax = max(X), ymin = min(Y), ymax = max(Y),
         xrange = xmax - xmin, yrange = ymax - ymin,
         xmid = (xmin + xmax) / 2, ymid = (ymin + ymax) / 2,
         x_stand = (X - xmid) / pmax(xrange, yrange),
         y_stand = (Y - ymid) / pmax(xrange, yrange)) %>% 
  nest(data = c(X, Y, x_stand, y_stand)) %>% 
  ungroup() %>% 
  mutate(group_y = floor((rank(-ymid) - 1) / 17)) %>% 
  group_by(group_y) %>% 
  mutate(group_x = rank(xmid),
         group_xy = 100 * group_y + group_x,
         name = forcats::fct_reorder(name, group_xy, mean)) %>% 
  unnest(data)

dat_temp %>% 
  ggplot(aes(x_stand, y_stand, group = name, fill = stadsdel)) + 
  geom_polygon(color = "black", alpha = 0.4) + 
  coord_equal() + 
  facet_wrap(~ name, ncol = 17) +
  theme_mg3() + 
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())

# DeSO, mg 19 ----
mg19 <- ggplot() +
  # geom_sf(data = dat_roads) +
  geom_sf(data = dat_adm_delomr, aes(fill = stadsdel), alpha = 0.2, col = NA) +
  geom_sf(data = dat_deso, fill = NA, size = 0.1) +
  geom_sf_text(data = dat_deso, aes(label = deso), size = 1) +
  theme_mg3()
mg19

# Area size, mg 20 ----
dat_temp <- dat_adm_mpol %>% 
  select(name, stadsdel) %>% 
  mutate(area = st_area(.),
         area_km2 = as.numeric(area / 1e6)) %>% 
  as_tibble() %>% 
  select(-geometry, -area)

dat_temp %>%  
  mutate(stadsdel = forcats::fct_reorder(stadsdel, area_km2, sum)) %>%
  arrange(stadsdel, +area_km2) %>% 
  mutate(name = ordered(name, name),
         stadsdel = factor(stadsdel, sort(levels(stadsdel)))) %>% 
  ggplot(aes(area_km2, name, fill = stadsdel)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Malmö stadsdelar efter delområde och area.", 
       x = bquote(Area (km^2))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_mg3() +
  theme(axis.title.y = element_blank())

# Malmö in the world, mg21 ----
dat_temp <- dat_adm_mpol %>% 
  select(name, stadsdel) %>% 
  mutate(area = st_area(.),
         area_km2 = as.numeric(area / 1e6)) %>% 
  as_tibble() %>% 
  select(-geometry, -area) %>% 
  mutate(rank = rank(-area_km2))

world %>% 
  mutate(rank = rank(-area_km2)) %>% 
  left_join(dat_temp, by = "rank") %>%
  ggplot(aes(fill = stadsdel)) +
  geom_sf(alpha = 0.35) +
  geom_sf_text(aes(label = name), size = 1) +
  scale_x_continuous(expand = c(0,0)) +
  labs(title = "Malmös stadsdelar - storleksordning om de vore länder") +
  theme_mg3() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
