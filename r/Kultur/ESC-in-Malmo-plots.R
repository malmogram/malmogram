# ---
# Title: Eurovision Song Contest in Malmö - plots
# Purpose: 
# Start date: 220108
# Comments:
# ---

# Packages ----
library(tidyverse)
library(rnaturalearth)
library(rvest)


# Import ----
dat_esc <- read_csv("Data/Kultur/ESC-1992-and-2013.csv")

# Tile plot of points ESC 1992, mg101 ----
dat_temp <- dat_esc %>% 
  filter(Year == 1992) %>% 
  select(Contestant, Voter, Score) %>% 
  pivot_wider(values_from = Score, names_from = Contestant, values_fill = 0) %>%
  mutate_at(2:24, ~ -(.x == 12)) %>% 
  arrange(Ireland, `United Kingdom`, Malta, Italy, Greece, Israel, Iceland, France, Netherlands, Austria, Cyprus, Denmark)

dat_esc %>% 
  filter(Year == 1992) %>% 
  ggplot(aes(ordered(Voter, dat_temp$Voter), fct_reorder(Contestant, -Score, sum), label = Score, fill = Score)) +
  geom_tile() +
  geom_text() +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "white", midpoint = 8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3), legend.position = "none") +
  labs(title = "Röster vid Eurovision 1992 (i Malmö isstadion)")

# Tile plot of points ESC 2013, mg102 ----
dat_temp <- dat_esc %>% 
  filter(Year == 2013) %>% 
  select(Contestant, Voter, Score) %>% 
  pivot_wider(values_from = Score, names_from = Contestant, values_fill = 0) %>%
  mutate_at(2:24, ~ -(.x == 12)) %>% 
  arrange(Denmark, Azerbaijan, Ukraine, Norway, Russia, Greece, Italy, Malta, Netherlands, Hungary, Belgium, Moldova, Romania, Sweden, Georgia, Belarus)

dat_esc %>% 
  filter(Year == 2013) %>% 
  ggplot(aes(ordered(Voter, dat_temp$Voter), fct_reorder(Contestant, -Score, sum), label = Score, fill = Score)) +
  geom_tile() +
  geom_text() +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "white", midpoint = 8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3), legend.position = "none") +
  labs(title = "Röster vid Eurovision 2013 (i Malmö Arena)")

# Map with arrows, ESC 1992, mg103 ----
world <- ne_countries(scale = "medium", returnclass = "sf")
url <- "http://techslides.com/list-of-countries-and-capitals"
dat <- url %>%
  read_html() %>%
  html_table(header = T) %>%
  `[[`(1)
names(dat) <- c("Country", "City", "Lat", "Long", "CoCode", "Continent")

dat_temp <- dat_esc %>%
  left_join(dat %>% select(Country, Lat, Long) %>% rename(Voter = Country, lat1 = Lat, long1 = Long)) %>%
  left_join(dat %>% select(Country, Lat, Long) %>% rename(Contestant = Country, lat2 = Lat, long2 = Long)) %>% 
  group_by(Contestant, Year) %>% 
  mutate(Total_points = sum(Score)) %>% 
  ungroup()

top_n <- dat_temp %>%
  filter(Year == 1992) %>% 
  group_by(Contestant) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(-Total_points) %>% 
  slice(1:6) %>% 
  pull(Contestant)

ggplot() +
  geom_curve(aes(x = long1, y = lat1, xend = long2, yend = lat2, size = Score, alpha = Score, color = ordered(Score, 1:12)),
             data = dat_temp %>% filter(Year == 1992, Contestant %in% top_n), curvature = 0.1) +
  geom_sf(data = world, fill = NA, color = "grey80") +
  coord_sf(xlim = c(-25,50), ylim = c(30,70)) +
  scale_size(range = c(0.5,2), guide = "none") +
  scale_color_discrete() +
  scale_alpha(guide = "none") +
  facet_wrap(~ ordered(Contestant, top_n)) +
  theme_bw() + 
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

# Map with arrows, ESC 1992, mg104 ----
top_n <- dat_temp %>%
  filter(Year == 2013) %>% 
  group_by(Contestant) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(-Total_points) %>% 
  slice(1:6) %>% 
  pull(Contestant)

ggplot() +
  geom_curve(aes(x = long1, y = lat1, xend = long2, yend = lat2, size = Score, alpha = Score, color = ordered(Score, 1:12)),
             data = dat_temp %>% filter(Year == 2013, Contestant %in% top_n), curvature = 0.1) +
  geom_sf(data = world, fill = NA, color = "grey80") +
  coord_sf(xlim = c(-25,50), ylim = c(30,70)) +
  scale_size(range = c(0.5,2), guide = "none") +
  scale_color_discrete() +
  scale_alpha(guide = "none") +
  facet_wrap(~ ordered(Contestant, top_n)) +
  theme_bw() + 
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

