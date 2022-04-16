# ---
# Title: Plots of population statistics
# Purpose: Plotting script
# Start date: 210908
# Comments:
# ---

# Packages ----
library(tidyverse)
library(patchwork)

# Read data
dat_mun <- read_csv("Data/Befolkning/Data-Population-by-municipality.csv")
dat_cities <- read_csv("Data/Befolkning/Data-Population-by-city.csv")

# Merge ----
dat_merged <- dat_mun %>% 
  rename(Stad = Kommun) %>% 
  filter(Stad == "Malmö") %>% 
  bind_rows(dat_cities %>% filter(Stad == "Malmö", År < 1950)) %>% 
  arrange(År)

# Plot ----
## Population by year. Malmö only. mg1 ----
mg1 <- ggplot(dat_merged, aes(År, Befolkning / 1000)) +
  geom_line(col = "white", size = 1.5) +
  #geom_point(col = "white") +
  ylab("Befolkning (tusental)") +
  ggtitle("Malmö, befolkning över tid (1850 - 2020)") +
  ylim(0, 400) +
  labs(caption = "Källa: SCB, http://www.scb.se/be0101\nMalmögram 1\n2 januari 2022") +
  theme_mg1()
ggsave("Output/Befolkning/001-Population-by-year.png", mg1, width = 8, height = 5, units = "in")

## Population change per year. Malmö only. mg2 ----
mg2 <- dat_merged %>% 
  filter(År >= 1950) %>% 
  mutate(Diff = Befolkning - lag(Befolkning)) %>% 
  ggplot(aes(År, Diff)) +
  geom_line(col = "white") +
  geom_point(col = "white") +
  ggtitle("Malmö, befolkningsförändring mot föregående år (1950 - 2020)") +
  coord_cartesian(xlim = c(1950, 2025)) +
  labs(caption = "Källa: SCB, http://www.scb.se/be0101\nMalmögram 2\n2 januari 2022") +
  scale_x_continuous(breaks = seq(1950, 2025, 25)) +
  theme_mg1()
ggsave("Output/Befolkning/002-Population-change-by-year.png", mg2, width = 8, height = 5, units = "in")

## Population by year, divided by total national population. mg3 ----
dat_temp <- dat_mun %>% 
  group_by(År) %>% 
  mutate(Totalbefolkning = sum(Befolkning),
         Proportion = Befolkning / Totalbefolkning) %>% 
  ungroup()
mg3 <- ggplot(dat_temp, aes(År, Proportion, group = Kommun)) +
  geom_line(data = dat_temp %>% filter(Kommun %in% c("Stockholm", "Göteborg", "Uppsala", "Linköping")), 
            col = "grey60") +
  geom_line(data = dat_temp %>% filter(Kommun == "Malmö"), col = "white", size = 1) +
  # geom_point() +
  geom_text(aes(x = 2022, y = Proportion, label = Kommun), 
            data = dat_temp %>% filter(År == 2020, Kommun %in% c("Stockholm", "Göteborg", "Uppsala", "Linköping")),
            hjust = 0, col = "grey60") +
  geom_text(aes(x = 2022, y = Proportion, label = Kommun), 
            data = dat_temp %>% filter(År == 2020, Kommun == "Malmö"),
            hjust = 0, col = "white") +
  ggtitle("Malmö kommun, befolkning som andel av Sveriges befolkning (1950 - 2020)") +
  coord_cartesian(xlim = c(1950, 2030)) +
  labs(caption = "Källa: SCB, http://www.scb.se/be0101\nMalmögram 3\n2 januari 2022") +
  theme_mg1()
ggsave("Output/Befolkning/003-Population-proportion-by-year.png", mg3, width = 8, height = 5, units = "in")

## Population by year, Malmö and satellites, divided by total regional population. mg4 ----
dat_temp <- dat_mun %>% 
  filter(Kommun %in% c("Malmö",  "Lund", "Trelleborg", "Burlöv", "Kävlinge", 
                       "Lomma", "Staffanstorp", "Svedala", "Vellinge", "Eslöv", "Höör", "Skurup")) %>% 
  group_by(År) %>% 
  mutate(Totalbefolkning = sum(Befolkning),
         Proportion = Befolkning / Totalbefolkning) %>% 
  ungroup()

g1 <- ggplot(dat_temp, aes(År, Proportion, group = Kommun)) +
  geom_line(data = dat_temp %>% filter(Kommun != "Malmö"), col = "grey60") +
  geom_line(data = dat_temp %>% filter(Kommun == "Malmö"), col = "white", size = 1) +
  geom_text(aes(x = 2022, label = Kommun), 
            data = dat_temp %>% filter(År == 2020, Kommun %in% c("Malmö", "Lund", "Trelleborg")),
            hjust = 0, col = "grey60") +
  geom_text(aes(x = 2022, label = Kommun), 
            data = dat_temp %>% filter(År == 2020, Kommun == "Malmö"),
            hjust = 0, col = "white") +
  ggtitle("Malmö, befolkning som andel av Stormalmös befolkning (1950 - 2020)") +
  coord_cartesian(xlim = c(1950, 2030)) +
  theme_mg1() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

g2 <- ggplot(dat_temp %>% filter(!(Kommun %in% c("Malmö", "Lund", "Trelleborg"))), 
             aes(År, Proportion, group = Kommun)) +
  geom_line(col = "grey60") +
  geom_text(aes(x = 2022, label = Kommun), 
            data = dat_temp %>% filter(År == 2020, !(Kommun %in% c("Malmö", "Lund", "Trelleborg"))),
            hjust = 0, col = "grey60", size = 2) +
  ylab("") +
  scale_y_continuous(breaks = c(0, 0.02, 0.04, 0.06)) +
  coord_cartesian(xlim = c(1950, 2030), ylim = c(0, 0.065)) +
  labs(caption = "Källa: SCB, http://www.scb.se/be0101\nMalmögram 4\n2 januari 2022") +
  theme_mg1()

mg4 <- g1 / g2 + plot_layout(height = c(2, 1))
ggsave("Output/Befolkning/004-Population-proportion-by-year-satellites.png", mg4, width = 8, height = 10, units = "in")

## Population path, Malmö against Sweden, 1950 - 2020. mg5 ----
dat_temp <- dat_mun %>% 
  bind_rows(dat_mun %>% 
              group_by(År) %>% 
              summarise(Befolkning = sum(Befolkning)) %>% 
              mutate(Kommun = "Sverige")) %>% 
  filter(Kommun %in% c("Malmö", "Sverige")) %>% 
  pivot_wider(names_from = Kommun, values_from = Befolkning) %>% 
  mutate(Kvot = Malmö / Sverige)

mg5 <- ggplot(dat_temp, aes(Sverige, Malmö)) +
  geom_abline(intercept = 0, slope = dat_temp %>% filter(År == 2020) %>% pull(Kvot), col = "grey90") +
  geom_path(col = "white") +
  geom_segment(aes(x = Sverige, xend = Sverige, y = Malmö, yend = Malmö + 20000 * c(1,-1,1,-1,-1,1,-1,-1)), data = dat_temp %>% filter(År %in% seq(1950, 2020, 10)), col = "grey80") +
  geom_point(aes(y = Malmö + 20000 * c(1,-1,1,-1,-1,1,-1,-1)), data = dat_temp %>% filter(År %in% seq(1950, 2020, 10)), size = 12, shape = 21) +
  geom_point(aes(y = Malmö + 20000 * c(1,-1,1,-1,-1,1,-1,-1)), 
             data = dat_temp %>% filter(År %in% seq(1950, 2020, 10)), 
             size = 12, shape = 21, fill = "#000077", alpha = 1, color = "white") +
  geom_text(aes(y = Malmö + 20000 * c(1,-1,1,-1,-1,1,-1,-1), label = År), 
            data = dat_temp %>% filter(År %in% seq(1950, 2020, 10)),
            col = "white", size = 3, family = "Garamond") +
  scale_x_continuous(breaks = 1000000 * 7:10, labels = c("7 000", "8 000", "9 000", "10 000")) +
  scale_y_continuous(breaks = 100000 * c(2, 2.5, 3, 3.5), labels = c(200, 250, 300, 350)) +
  labs(title = "Malmö mot Sveriges befolkning över tid",
       caption = "Rak linje anger 2020 års andel malmöbor.\nKälla: SCB, http://www.scb.se/be0101\nMalmögram 5\n2 januari 2022") +
  theme_mg1()
ggsave("Output/Befolkning/005-Population-path-Malmo-to-Sweden.png", mg5, width = 8, height = 5.3, units = "in")

## Population by year, Malmö against rest of Stormalmö, 1950 - 2020. mg6 ----
dat_temp <- dat_mun %>% 
  filter(Kommun %in% c("Malmö", "Lund", "Trelleborg", "Burlöv", "Kävlinge", 
                       "Lomma", "Staffanstorp", "Svedala", "Vellinge", "Eslöv", "Höör", "Skurup")) %>% 
  mutate(Region = ifelse(Kommun %in% c("Malmö", "Lund"), Kommun, "Övriga stormalmö")) %>% 
  group_by(År, Region) %>% 
  summarise(Befolkning = sum(Befolkning) / 1000) %>% 
  mutate(Region = ordered(Region, c("Malmö", "Lund", "Övriga stormalmö")))

mg6 <- ggplot(dat_temp, aes(År, Befolkning, color = Region)) +
  geom_line(size = 2) +
  scale_color_manual(values = c("#aa0000", "#ddddff", "#5555ff")) +
  labs(title = "Befolkning i Malmö, Lund och övriga stormalmö, 1950 - 2020",
       y = "Befolkning (tusental)",
       caption = "Källa: SCB, http://www.scb.se/be0101\nMalmögram 6\n2 januari 2022") +
  theme_mg1()
ggsave("Output/Befolkning/006-Population-by-year-Malmo-and-neighbours.png", mg6, width = 8, height = 5, units = "in")

## Population by year, Malmö. mg7 ----
dat_prog <- read_csv("Data/Befolkning/Data-Population-forecast.csv") %>% 
  mutate(Typ = "Prognos")

dat_temp <- dat_mun %>% 
  filter(Kommun == "Malmö", År > 1989) %>% 
  mutate(Typ = "Verklig") %>% 
  bind_rows(dat_prog) %>% 
  mutate(Befolkning = Befolkning / 1000) %>% 
  mutate(Typ = ordered(Typ, c("Verklig", "Prognos")))

mg7 <- ggplot(dat_temp, aes(År, Befolkning, col = Typ)) +
  geom_line(size = 0.1) +
  geom_point() +
  scale_color_manual(values = c("#aa0000", "#ddddff")) +
  labs(title = "Befolkning i Malmö. Verkliga siffror 1990 - 2020, prognos 2020 - 2031",
       y = "Befolkning (tusental)",
       caption = "Källa: SCB, http://www.scb.se/be0101\nMalmö Stad, Stadskontoret, Analys och hållbarhet - Befolkningsprognos Malmö stad 2021-2031\n\nMalmögram 7\n2 januari 2022") +
  theme_mg1() +
  theme(legend.title = element_blank())

ggsave("Output/Befolkning/007-Population-prognosis.png", mg7, width = 8, height = 5, units = "in")
