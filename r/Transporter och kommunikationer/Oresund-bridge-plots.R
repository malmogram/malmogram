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

# Total timeline 2017 - 2021, mg83 ----
dat_ore %>% 
  mutate(Year_month = lubridate::ymd(paste0(Year, "-", Month, "-", 15))) %>%
  filter(Year %in% 2017:2021) %>% 
  ggplot(aes(Year_month, `In total`)) +
  geom_line()

# Total timeline as heatmap, mg84 ----
dat_ore %>% 
  ggplot(aes(Month, -Year, fill = `In total`)) +
  geom_tile() +
  geom_vline(xintercept = seq(0.5,12.5, 3)) +
  geom_hline(yintercept = seq(-1999.5,-2019.5,-5)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_gradient2()

# Yearly totals by vehicle type, mg85 ----
dat_ore %>% 
  select(-`In total`, -stat_id) %>% 
  pivot_longer(-c(Year, Month), names_to = "Vehicle", values_to = "Count") %>% 
  mutate(Year_month = lubridate::ymd(paste0(Year, "-", Month, "-", 15))) %>%
  count(Year, Vehicle, wt = Count) %>% 
  ggplot(aes(Year, n, fill = Vehicle)) +
  geom_col(color = "black")

# Yearly totals by vehicle type, separate facets, mg86 ----
dat_ore %>% 
  select(-`In total`, -stat_id) %>% 
  pivot_longer(-c(Year, Month), names_to = "Vehicle", values_to = "Count") %>% 
  mutate(Year_month = lubridate::ymd(paste0(Year, "-", Month, "-", 15))) %>%
  count(Year, Vehicle, wt = Count) %>% 
  ggplot(aes(Year, n, fill = Vehicle)) +
  geom_line() +
  facet_wrap(~ Vehicle, scales = "free_y")

# Monthly variation by vehicle type, mg87 ----
dat_ore %>% 
  select(-stat_id, -`In total`) %>% 
  pivot_longer(-c(Year, Month)) %>% 
  group_by(Year, name) %>% 
  mutate(Yearly_mean = mean(value),
         Ratio = value / Yearly_mean,
         log2_ratio = log2(Ratio)) %>% 
  filter(Year %in% 2010:2019) %>% 
  ggplot(aes(Month, log2_ratio, col = name)) +
  geom_line(aes(group = paste0(Year, name)), alpha = 0.25) +
  geom_smooth(aes(group = name), se = F)

# Monthly variation by vehicle type,separate facets, mg88 ----
dat_ore %>% 
  select(-stat_id, -`In total`) %>% 
  pivot_longer(-c(Year, Month)) %>% 
  filter(Year %in% 2001:2019) %>% 
  ggplot(aes(Month, value, col = name)) +
  geom_line(aes(group = paste0(Year, name)), alpha = 0.75) +
  facet_wrap(~ name, scales = "free_y", ncol = 5) +
  geom_text(aes(x, y, label = text), col = "black", size = 2, hjust = 0, nudge_x = 0.2,
            data = data.frame(x = 6, y = 13885, text = "Juni 2014: 13 885 bussar!?", name = "Coaches"))

# Year travels by train, mg89 ----
dat_ore_tag %>% 
  ggplot(aes(År, Resor)) +
  geom_line()

# Trains to and from Sweden, mg90 ----
dat_ore_tag %>% 
  select(År, `Dan - Sve, tåg`, `Sve - Dan, tåg`) %>% 
  pivot_longer(-År, names_to = "Riktning", values_to = "Antal") %>% 
  mutate(Riktning = ifelse(Riktning == "Dan - Sve, tåg", "Till Malmö", "Från Malmö")) %>% 
  filter(År %in% 2001:2019) %>% 
  ggplot(aes(År, Antal, col = Riktning)) +
  geom_line()

# Trains to and from Sweden, cumulative numbers, mg91 ----
dat_ore_tag %>% 
  select(År, `Dan - Sve, tåg`, `Sve - Dan, tåg`) %>% 
  pivot_longer(-År, names_to = "Riktning", values_to = "Antal") %>% 
  mutate(Riktning = ifelse(Riktning == "Dan - Sve, tåg", "Till Malmö", "Från Malmö")) %>% 
  filter(År %in% 2000:2020) %>%
  group_by(Riktning) %>% 
  mutate(`Kumulativt antal` = cumsum(Antal)) %>% 
  ggplot(aes(År, `Kumulativt antal`, col = Riktning)) +
  geom_line()

# Trains to and from Sweden, cumulative difference, mg92 ----
dat_ore_tag %>% 
  select(År, `Dan - Sve, tåg`, `Sve - Dan, tåg`) %>% 
  pivot_longer(-År, names_to = "Riktning", values_to = "Antal") %>% 
  mutate(Riktning = ifelse(Riktning == "Dan - Sve, tåg", "Till Malmö", "Från Malmö")) %>% 
  filter(År %in% 2000:2020) %>%
  group_by(Riktning) %>% 
  mutate(`Kumulativt antal` = cumsum(Antal)) %>%
  select(-Antal) %>% 
  pivot_wider(names_from = Riktning, values_from = `Kumulativt antal`) %>% 
  ggplot(aes(År, `Från Malmö` - `Till Malmö`)) +
  geom_line()
  
