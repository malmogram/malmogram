# ---
# Title: Population by age and year. Setup
# Purpose: 
# Start date: 211031
# Comments:
# ---

# Packages ----
library(tidyverse)
library(readxl)

# Import ----
dat <- read_excel("Data/Befolkning/BE0101N1_20211031-111248.xlsx", skip = 2) %>% 
  fill(c("...1", "...2", "...3", "...4")) %>% 
  rename(Kommun = ...1, Civilstånd = ...2, Ålder = ...3, Kön = ...4) %>% 
  drop_na(`2020`) %>% 
  mutate(Ålder = gsub("\\+", "", Ålder))

# Pivot data to long form ----
dat <- dat %>% 
  pivot_longer(-c(Kommun, Civilstånd, Ålder, Kön), names_to = "År", values_to = "Antal") %>% 
  mutate(Ålder = gsub(" år", "", Ålder) %>% as.numeric(),
          Kön = tools::toTitleCase(Kön))

# write_csv(dat, "Data/Befolkning/Data-Population-by-age.csv")
