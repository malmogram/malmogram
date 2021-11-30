# ---
# Title: Riksdag elections - setup
# Purpose: 
# Start date: 
# Comments:
# ---

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
  arrange(Skala) %>% 
  mutate(Kortform = c("V", "S", "MP", "C", "L", "M", "KD", "SD", "Övr.", "Ej röst.", "Ogilt."),
         Färg = c("#DA291C", "#E8112D", "#83CF39", "#009933", "#006AB3", "#52BDEC", "#000077", "#DDDD00", "grey40", "grey70", "grey90")) %>% 
  mutate(Parti = ordered(Parti, Parti))

dat <- dat %>% 
  mutate(Parti = ordered(Parti, dat_parties$Parti)) %>% 
  arrange(År, Parti)

dat_agg <- dat %>% 
  group_by(År, Parti) %>% 
  summarise(Antal = sum(Antal, na.rm = T)) %>% 
  mutate(Region = "Sverige")

ggplot(dat_parties, aes(y = Skala, x = 0, label = Parti)) +
  geom_hline(aes(yintercept = Skala, col = Parti), size = 14) +
  geom_text() +
  scale_color_manual(values = dat_parties$Färg) +
  theme(legend.position = "none")
