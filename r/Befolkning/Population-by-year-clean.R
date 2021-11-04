# ---
# Title: Clean-up of Statistics Sweden data on population sizes
# Purpose: Clean-up of Statistics Sweden data on population sizes
# Start date: 210910
# Comments:   211003. Befolkningsprognoser
# ---

# Packages ----
library(tidyverse)
library(readxl)

# City data. Städer 1800-1967 ----
dat_cities <- read_excel("Data/Befolkning/stader_1800_1967.xls", skip = 3, col_names = T)

dat_cities <- dat_cities %>% 
  mutate_at(-(1:2), as.numeric) %>% 
  select(-2) %>% 
  rename("Stad" = "Städer", "1960" = "19603 4", "1965" = "19653 4", "1967" = "19673 4") %>% 
  slice(2:which(Stad == "Östhammar")) %>% 
  pivot_longer(-Stad, names_to = "År", values_to = "Befolkning") %>% 
  drop_na(Stad) %>% 
  mutate(År = as.numeric(År))

# Kommun data. BE0101 Folkmängdkom 2020 ----
dat_mun <- read_excel("Data/Befolkning/be0101_folkmangdkom2020.xlsx", skip = 5, col_names = T) %>% 
  slice(1:which(Kommun == "Övertorneå"))

dat_mun <- dat_mun %>% 
  mutate_at(-1, as.numeric) %>% 
  pivot_longer(-Kommun, names_to = "År", values_to = "Befolkning") %>% 
  mutate(År = as.numeric(År))

dat_cities %>% filter(Stad == "Malmö", År > 1949)
dat_mun %>% filter(Kommun == "Malmö", År %in% c(1950, 1960, 1965, 1967))

# write_csv(dat_cities, "Data/Befolkning/Data-Population-by-city.csv")
# write_csv(dat_mun, "Data/Befolkning/Data-Population-by-municipality.csv")

# Malmö kommun. Befolkningsprognoser ----
## Befolkningsprognos Malmö stad 2021-2031. Stadskontoret, Analys och hållbarhet
url <- "https://malmo.se/download/18.6e578a49179f31d1ef234/1623306886794/Rapport%20och%20appendix,%20befolkningsprognos%20Malm%C3%B6%20stad%202021-2031.pdf"
library(tabulizer)
dat_prog <- extract_tables(url, pages = 36) %>% 
  `[[`(1) %>% 
  `[`(2,-1) %>% 
  strsplit(split = " ") %>% 
  unlist() %>% 
  matrix(byrow = T, ncol = 2) %>% 
  apply(1, function(x) paste0(x, collapse = "")) %>% 
  as.numeric() %>% 
  tibble(År = 2020:(2019 + length(.))) %>% 
  rename("Befolkning" = ".")

# write_csv(dat_prog, "Data/Befolkning/Data-Population-forecast.csv")  
