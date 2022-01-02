# ---
# Title: Malmö University setup
# Purpose: 
# Start date: 211228
# Comments:
# ---

# Packages ----
library(tidyverse)

# Import ----
dat_uni <- read_csv("Data/Utbildning/Malmo-University-Courses.csv")

# To wider format ----
vars <- dat_uni %>% count(name) %>% pull(name)
dat_uni_wide <- dat_uni %>% 
  filter(name %in% vars[c(1,4,5,10,11,13,14,15,16,17,18:29,30:49,54:65,70:73)]) %>% 
  pivot_wider(names_from = name, values_from = value) %>%
  mutate_at(c(22:57), as.numeric)
