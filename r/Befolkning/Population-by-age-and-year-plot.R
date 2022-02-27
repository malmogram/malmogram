# ---
# Title: Population by age and year. Plotting
# Purpose: 
# Start date: 211031
# Comments:
# ---

# Packages ----
library(tidyverse)
library(gganimate)
library(patchwork)
library(ggrepel)

# Load data ----
# source("r/Befolkning/Population-by-age-and-year-setup.R", encoding = "UTF-8")
dat <- read_csv("Data/Befolkning/Data-Population-by-age.csv")
source("r/Teman/Themes.R", encoding = "UTF-8")

# Population pyramid in 2020, mg29 ----
dat_temp <- dat %>% 
  filter(År == 2020) %>% 
  group_by(Ålder, Kön) %>% 
  summarise(Antal = sum(Antal)) %>% 
  mutate(Antal_temp = ifelse(Kön == "Kvinnor", -Antal, Antal))

mg29 <- ggplot(dat_temp, aes(Ålder, Antal_temp, fill = Kön)) + 
  geom_bar(stat = "identity", width = 1) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(breaks = seq(-10000, 10000, 1000), labels = abs(seq(-10000, 10000, 1000))) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Malmö Stad, befolkning per ålder och kön, 2020",
       x = "Antal personer", y = "Ålder",
       caption = "Källa: SCB, http://www.scb.se/be0101
       Malmögram 29, 31 januari 2022") +
  theme_mg1()
mg29

ggsave("Output/Befolkning/Population-by-age.png", mg29, height = 10, width = 10)

# Population by age and sex, mg30 ----
dat_temp <- dat %>% 
  filter(År == 2020) %>% 
  group_by(Ålder, Kön) %>% 
  summarise(Antal = sum(Antal)) %>% 
  mutate(Antal_temp = ifelse(Kön == "Kvinnor", -Antal, Antal))

g1 <- ggplot(dat_temp, aes(Ålder, Antal, fill = Kön)) + 
  geom_bar(stat = "identity", width = 1) +
  facet_wrap( ~ Kön) +
  theme(legend.position = "none", axis.title.x = element_blank())

dat_temp <- dat_temp %>% 
  select(-Antal_temp) %>% 
  pivot_wider(names_from = Kön, values_from = Antal) %>% 
  mutate(Grund = pmin(Kvinnor, Män),
         Överskott = pmax(Kvinnor, Män) - Grund,
         Status = ifelse(Kvinnor > Män, "Fler kvinnor", 
                         ifelse(Kvinnor == Män, "Lika", "Fler män"))) %>% 
  select(-Kvinnor, -Män) %>% 
  pivot_longer(-c(Ålder, Status), names_to = "Typ", values_to = "Antal") %>% 
  mutate(Status = ifelse(Typ == "Grund", Typ, Status),
         Status = ordered(Status, c("Fler kvinnor", "Fler män", "Lika", "Grund")))

g2 <- ggplot(dat_temp, aes(Ålder, Antal, fill = Status)) + 
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = c("blue", "red", "white"))

mg30 <- g1 / g2 & 
  theme_mg1() & 
  theme(strip.background = element_blank(), strip.text = element_text(color = "white")) &
  scale_fill_manual(values = c("#3182BD", "#FC9272", "#DEEBF7")) &
  plot_annotation(title = "Malmö stads befolkning efter ålder, 2020", 
                  caption = "Källa: SCB, http://www.scb.se/be0101
       Malmögram 30, 3 februari 2022")
mg30

ggsave("Output/Befolkning/Population-by-age.png", mg30, height = 8, width = 10)

# Gender ratio (women to men) by age, mg31 ----
dat_temp <- dat %>% 
  filter(År == 2020) %>% 
  group_by(Ålder, Kön) %>% 
  summarise(Antal = sum(Antal)) %>% 
  pivot_wider(names_from = Kön, values_from = Antal) %>% 
  mutate(Kvot = Kvinnor / Män)

mg31 <- dat_temp %>% 
  ggplot(aes(Ålder, Kvot)) + 
  geom_line(col = "white") + 
  geom_point(col = "white") +
  geom_hline(yintercept = 1, col = "white") +
  labs(title = "Malmö Stad, könskvot per ålder, kvinnor / män, 2020",
       y = "Kvot", x = "Ålder",
       caption = "Källa: SCB, http://www.scb.se/be0101
       Malmögram 31, 5 februari 2022") +
  theme_mg1() +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  coord_cartesian(ylim = c(0, 2.2))
mg31

ggsave("Output/Befolkning/Population-gender-ratio-by-age.png", mg31, height = 5, width = 10)

mod <- lm(log(Kvot) ~ Ålder, dat_temp %>% filter(Ålder > 74))
dat_pred <- tibble(Ålder = 75:150)
dat_pred <- dat_pred %>% 
  mutate(Pred = exp(predict(mod, newdata = dat_pred)))

mg31b <- mg31 +
  geom_line(aes(Ålder, Pred), data = dat_pred, col = "red")
mg31b

# Proportional bars dividing age groups by year, mg32 ----
dat_temp <- dat %>% 
  group_by(År, Ålder, Kön) %>% 
  summarise(Antal = sum(Antal)) %>% 
  mutate(År = as.numeric(År)) %>% 
  mutate(Åldersgrupp = cut(Ålder, seq(0, 110, 10), include_lowest = T, right = F)) %>% 
  mutate(Åldersgrupp = ordered(Åldersgrupp, rev(levels(Åldersgrupp))))

mg32 <- dat_temp %>% 
  ggplot(aes(År, Antal, fill = Åldersgrupp)) +
  geom_bar(stat = "identity", position = position_fill(), width = 1, alpha = 0.9) +
  facet_wrap(~ Kön) +
  scale_fill_manual(values = colorRampPalette(c("#e6f5ff", "#005c99"))(11)) +
  scale_x_continuous(breaks = seq(1970, 2020, 10)) +
  labs(title = "Malmö Stad, befolkning i ålderklasser, 1968 - 2020",
       y = "Andel", x = "År",
       caption = "Källa: SCB, http://www.scb.se/be0101
       Malmögram 32, 5 februari 2022") +
  theme_mg1()
mg32

ggsave("Output/Befolkning/Population-by-age-and-year-classes.png", mg32, height = 5, width = 10)

# Lines per age-group, population count, mg33 ----
dat_temp <- dat %>% 
  group_by(År, Ålder, Kön) %>% 
  summarise(Antal = sum(Antal)) %>% 
  mutate(År = as.numeric(År)) %>% 
  ungroup() %>% group_by(År, Kön) %>% 
  arrange(Ålder) %>% 
  mutate(Kumulativt_antal = cumsum(Antal))

mg33 <- dat_temp %>% 
  ggplot(aes(År, Kumulativt_antal, group = Ålder)) +
  geom_line(color = "#005c99") +
  geom_line(data = dat_temp %>% filter(Ålder %in% seq(0, 100, 10)), color = "white") +
  geom_text(aes(label = paste(Ålder, "år")), data = dat_temp %>% filter(Ålder %in% seq(0, 90, 10), År == 2020), 
            color = "white", family = "Garamond", hjust = 0, nudge_x = 1) +
  facet_wrap(~ Kön) + 
  scale_x_continuous(limits = c(1968, 2025)) +
  labs(title = "Malmö Stad, total befolkning efter ålder, 1968 - 2020",
       y = "Antal  personer yngre än angiven ålder", x = "År",
       caption = "Källa: SCB, http://www.scb.se/be0101
       Malmögram 33, 5 februari 2022") +
  theme_mg1()
mg33

ggsave("Output/Befolkning/Population-by-age-and-year.png", mg33, height = 8, width = 10)

# Lines per age-group, population proportion, mg34 ----
dat_temp <- dat %>% 
  group_by(År, Ålder, Kön) %>% 
  summarise(Antal = sum(Antal)) %>% 
  mutate(År = as.numeric(År)) %>% 
  ungroup() %>% group_by(År, Kön) %>% 
  arrange(Ålder) %>% 
  mutate(Kumulativt_antal = cumsum(Antal),
         p = Kumulativt_antal / max(Kumulativt_antal))

mg34 <- dat_temp %>% 
  ggplot(aes(År, p, group = Ålder)) +
  geom_line(color = "#005c99") +
  geom_line(data = dat_temp %>% filter(Ålder %in% seq(0, 100, 10)), color = "white") +
  geom_text(aes(label = paste(Ålder, "år")), data = dat_temp %>% filter(Ålder %in% seq(0, 90, 10), År == 2020), 
            color = "white", family = "Garamond", hjust = 0, nudge_x = 1) +
  facet_wrap(~ Kön) + 
  scale_x_continuous(limits = c(1968, 2025)) +
  labs(title = "Malmö Stad, befolkningsandel efter ålder, 1968 - 2020",
       y = "Andel personer yngre än angiven ålder", x = "År",
       caption = "Källa: SCB, http://www.scb.se/be0101
       Malmögram 34, 5 februari 2022") +
  theme_mg1()
mg34

ggsave("Output/Befolkning/Population-by-age-and-year-proportions.png", mg34, height = 8, width = 10)

# Gender-ratio as path diagram, mg35 ----
dat_temp <- dat %>% 
  group_by(År, Kön) %>% 
  summarise(Antal = sum(Antal)) %>% 
  pivot_wider(names_from = Kön, values_from = Antal)

mg35 <- dat_temp %>% 
  ggplot(aes(Kvinnor, Män)) +
  geom_path() +
  geom_point(aes(fill = År), data = dat_temp %>% filter(År %in% seq(1970, 2020, 1)), 
             size = 5, shape = 21) +
  geom_abline(intercept = 0, slope = 1, col = "grey50") +
  coord_equal() +
  labs(title = "Malmö Stad, antal män och kvinnor över tid, 1968 - 2020",
       y = "Antal män", x = "Antal kvinnor",
       caption = "Källa: SCB, http://www.scb.se/be0101
       Malmögram 35, 5 februari 2022") +
  theme_mg1()
mg35

ggsave("Output/Befolkning/Population-gender-ratio-as-path.png", mg35, height = 8, width = 10)

# Marriage status over time, mg36 ----
mg36 <- dat %>% 
  filter(Ålder > 19) %>% 
  mutate(Civilstånd = tools::toTitleCase(Civilstånd)) %>% 
  ggplot(aes(År, Antal, fill = Civilstånd)) +
  geom_bar(stat = "identity", position = position_fill(), width = 1) +
  facet_wrap(~ Kön) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Malmö stad, befolkningens (över 20 år) civilstånd över tid, 1968 - 2020",
       y = "År", x = "Andel av kommunens befolkning",
       caption = "Källa: SCB, http://www.scb.se/be0101
       Malmögram 36, 5 februari 2022") +
  theme_mg1()
mg36

ggsave("Output/Befolkning/Population-marriage-status-over-time.png", mg36, height = 8, width = 10)

# Marriage status at 30, mg37 ----
mg37 <- dat %>% 
  filter(Ålder == 40,
          År %in% seq(1970, 2020, 10)) %>% 
  mutate(Civilstånd = tools::toTitleCase(Civilstånd)) %>% 
  ggplot(aes(0, Antal, fill = Civilstånd)) +
  geom_bar(stat = "identity", position = position_fill(), color = "black") +
  facet_grid(Kön ~ År, switch = "y") +
  xlim(-1.5,1) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Malmö stad, befolkningens civilstånd vid 30 års ålder",
       y = "", x = "",
       caption = "Källa: SCB, http://www.scb.se/be0101
       Malmögram 37, 5 februari 2022") +
  theme_mg1() +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank())
mg37

ggsave("Output/Befolkning/Population-marriage-status-as-pies.png", mg37, height = 4, width = 10)


# This could make an interesting animation

# Population pyramid in 1970 - 2020, mg39 ----
dat_temp <- dat %>% 
  filter(År %in% seq(1970, 2020, 10)) %>% 
  group_by(Ålder, Kön, År) %>% 
  summarise(Antal = sum(Antal)) %>% 
  mutate(Antal_temp = ifelse(Kön == "Kvinnor", -Antal, Antal))

mg39 <- ggplot(dat_temp, aes(Ålder, Antal_temp, fill = Kön)) + 
  geom_bar(stat = "identity", width = 1) +
  # geom_line() +
  coord_flip() +
  facet_wrap(~ År) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(breaks = seq(-10000, 10000, 2000), labels = abs(seq(-10000, 10000, 2000)),
                     minor_breaks = seq(-10000, 10000, 1000)) +
  labs(title = "Malmö stad, befolkningspyramid över tid",
       y = "Antal personer", x = "Ålder",
       caption = "Källa: SCB, http://www.scb.se/be0101
       Malmögram 39, 5 februari 2022") +
  theme_mg1() +
  theme(panel.grid.minor.x = element_line(color = "#333399"))
mg39

ggsave("Output/Befolkning/Population-pyramids-over-time.png", mg39, height = 8, width = 10)

# Population pyramid, animation, mg40----
dat_temp <- dat %>% 
  group_by(Ålder, Kön, År) %>% 
  summarise(Antal = sum(Antal)) %>% 
  mutate(Antal_temp = ifelse(Kön == "Kvinnor", -Antal, Antal),
         År = as.integer(År))

g1 <- ggplot(dat_temp, aes(Ålder, Antal_temp, fill = Kön)) + 
  geom_bar(stat = "identity", width = 1) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-10000, 10000, 2000), labels = abs(seq(-10000, 10000, 2000)),
                     minor_breaks = seq(-10000, 10000, 1000)) +
  transition_time(År) +
  scale_fill_brewer(palette = "Blues") +
  ease_aes("sine-in-out") +
  labs(title = "Malmö stad, befolkningspyramid, {frame_time}", x = "Ålder", y = "Antal") +
  theme_mg1() +
  theme(legend.title = element_blank())

animate(g1, nframes = 150, rewind = F, end_pause = 10, width = 800, height = 800, res = 100)
anim_save("Output/Befolkning/Population-pyramid-over-time.gif")

# Population pyramid but filled by marriage status, mg54 ----
mg54 <- dat %>% 
  filter(År == 2020, Ålder < 101) %>% 
  mutate(Antal_temp = ifelse(Kön == "Kvinnor", -Antal, Antal),
         Civilstånd = ordered(Civilstånd, c("änkor/änklingar", "skilda", "gifta", "ogifta"))) %>% 
  ggplot(aes(Ålder, Antal_temp, fill = Civilstånd)) + 
  geom_bar(stat = "identity", position = position_fill(), width = 1) +
  # geom_hline(yintercept = seq(-0.75,0.75,0.25), col = "grey80") +
  geom_vline(xintercept = seq(25,75,25)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(~ Kön, scales = "free_x") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_mg1()
mg54
