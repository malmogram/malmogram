# ---
# Title: Allsvenskan herrar - data setup
# Purpose: Transform data on Allsvenskan for plotting
# Start date: 211003
# Comments:
# ---

# Packages
library(tidyverse)

# Import
dat <- read_csv("Data/Idrott/Allsvenskan, herrar, 1924-2021.csv") %>% 
  mutate(Säsong = gsub("_19", "-", Säsong),
         id = 1:n())

# Fix names
dat_renames <- tibble(Gammalt = c("IFK Norrköping FK", "IF Elfsborg", "Mjällby AIF", "IK Sirius FK", "Häcken"), 
                      Nytt = c("IFK Norrköping", "Elfsborg", "Mjällby", "Sirius", "BK Häcken"))
foo <- function(x){
  if(x %in% dat_renames$Gammalt) return(dat_renames$Nytt[which(x == dat_renames$Gammalt)]) else
    return(x)
}

dat <- dat %>% 
  mutate(Hemmalap = map_chr(Hemmalag, foo),
         Bortalag = map_chr(Bortalag, foo))

# Double row match data
dat_long <- dat %>% 
  rename(Fokuslag = Hemmalag, Motståndare = Bortalag, 
         "Gjorda mål" = Hemmamål, "Insläppta mål" = Bortamål) %>% 
  mutate(Plats = "Hemma") %>% 
  bind_rows(dat %>% 
              rename(Fokuslag = Bortalag, Motståndare = Hemmalag, 
                     "Gjorda mål" = Bortamål, "Insläppta mål" = Hemmamål) %>% 
              mutate(Plats = "Borta")) %>% 
  arrange(id) %>% 
  mutate(Utfall = ifelse(`Gjorda mål` > `Insläppta mål`, "Seger", 
                         ifelse(`Gjorda mål` == `Insläppta mål`, "Oavgjort", "Förlust")))


allsvenska_segrar <- c(1944, 1949, 1950, 1951, 1953, 1965, 1967, 1970, 1971, 1974, 
                       1975, 1977, 1985, 1986, 1987, 1988, 1989, 2004, 2010, 2013, 
                       2014, 2016, 2017, 2020, 2021)
mff_cols <- c("#A7D7FF", "#0091D2", "#005c86", "#DD3838", "#E2F1FF")
