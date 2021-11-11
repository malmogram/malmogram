# Add header ----

library(tidyverse)

dat <- read_csv("Data/Politik/ME0104B6_20211111-160107.csv", skip = 2) %>% 
  mutate(`parti mm` = `Encoding<-`(`parti mm`, "ASCII"),
         region = `Encoding<-`(region, "ASCII"))

# Pivot to longer format
dat <- dat %>% 
  pivot_longer(-c(region, `parti mm`), names_to = "År", values_to = "Antal") %>% 
  rename(Region = region, Parti = `parti mm`) %>% 
  arrange(År, Region, Parti) %>% 
  mutate(`Antal` = ifelse(`Antal`== "..", NA, Antal)) %>% 
  mutate_at(vars(År, Antal), as.numeric) %>% 
  separate(Region, c("Kommunkod", "Region"), extra = "merge")

# write_csv(dat, "Data/Politik/Riksdagsval.csv")

dat_parties <- tibble(Parti = unique(dat$Parti),
                      Skala = c(4,10,7,5,3,6,11,2,8,1,9)) %>% 
  arrange(Skala)

dat_agg <- dat %>% 
  group_by(År, Parti) %>% 
  summarise(Antal = sum(Antal, na.rm = T)) %>% 
  mutate(Region = "Sverige")
