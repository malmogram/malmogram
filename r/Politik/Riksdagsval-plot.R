# Add header

# Packages ----
library(tidyverse)

# Import data
source("r/Politik/Riksdagsval-setup.R", encoding = "UTF-8")

# Malmö and Sweden, piecharts over time, mg41

# Malmö, time series, mg42
dat_temp <- dat %>% 
  filter(Region == "Malmö") %>% 
  group_by(År) %>% 
  mutate(Totalt_antal = sum(Antal, na.rm = T),
         p = Antal / Totalt_antal)
dat_temp2 <- dat_temp %>% rename(Parti1 = Parti)

dat_temp %>% 
  ggplot(aes(År, p, col = Parti)) +
  geom_line(aes(col = NA, group = Parti1), data = dat_temp2, col = "grey80") +
  geom_line(size = 2) +
  geom_point(size = 3) +
  facet_wrap(~ Parti) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

# Path-graph, Malmö and Sweden, mg43
dat_temp <- dat %>% 
  filter(Region == "Malmö") %>% 
  bind_rows(dat_agg) %>% 
  group_by(År, Region) %>% 
  mutate(Totalt_antal = sum(Antal, na.rm = T),
         p = Antal / Totalt_antal) %>% 
  ungroup() %>% 
  select(Region, Parti, År, p) %>% 
  pivot_wider(values_from = p, names_from = Region) %>% 
  mutate(Sverige = ifelse(is.na(Malmö), NA, Sverige))

dat_temp %>% 
  ggplot(aes(Sverige, Malmö, label = År, col = År)) + 
  geom_point() +
  geom_path() +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~ Parti)
