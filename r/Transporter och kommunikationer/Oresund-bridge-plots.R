# ---
# Title: Oresund bridge -plots
# Purpose: 
# Start date: 211217
# Comments:
# ---

# Packages ----
library(tidyverse)

# Read setup
source("R/Transporter och kommunikationer/Oresund-bridge-setup.R", encoding = "UTF-8")

# Yearly totals, mg81 ----
dat_ore %>% 
  group_by(Year) %>% 
  summarise(Total = sum(`In total`)) %>% 
  ggplot(aes(Year, Total)) +
  geom_col(width = 0.5) +
  scale_y_continuous(breaks = seq(0, 8e6, 2e6), labels = seq(0, 8, 2), limits = c(0, 8e6))

# Total timeline, mg82 ----
dat_ore %>% 
  mutate(Year_month = lubridate::ym(paste0(Year, "-", Month))) %>% 
  ggplot(aes(Year_month, `In total`)) +
  geom_line()

# Total timeline 2017 - 2021, mg82 ----
dat_ore %>% 
  mutate(Year_month = lubridate::ymd(paste0(Year, "-", Month, "-", 15))) %>%
  filter(Year %in% 2017:2021) %>% 
  ggplot(aes(Year_month, `In total`)) +
  geom_line()

# Total timeline as heatmap, mg83
dat_ore %>% 
  ggplot(aes(Month, -Year, fill = `In total`)) +
  geom_tile() +
  geom_vline(xintercept = seq(0.5,12.5, 3)) +
  geom_hline(yintercept = seq(-1999.5,-2019.5,-5)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_gradient2()

# Yearly totals by vehicle type, mg84 ----
dat_ore %>% 
  select(-`In total`, -stat_id) %>% 
  pivot_longer(-c(Year, Month), names_to = "Vehicle", values_to = "Count") %>% 
  mutate(Year_month = lubridate::ymd(paste0(Year, "-", Month, "-", 15))) %>%
  count(Year, Vehicle, wt = Count) %>% 
  ggplot(aes(Year, n, fill = Vehicle)) +
  geom_col(color = "black")

# Yearly totals by vehicle type, separate facets, mg85 ----
dat_ore %>% 
  select(-`In total`, -stat_id) %>% 
  pivot_longer(-c(Year, Month), names_to = "Vehicle", values_to = "Count") %>% 
  mutate(Year_month = lubridate::ymd(paste0(Year, "-", Month, "-", 15))) %>%
  count(Year, Vehicle, wt = Count) %>% 
  ggplot(aes(Year, n, fill = Vehicle)) +
  geom_line() +
  facet_wrap(~ Vehicle, scales = "free_y")
