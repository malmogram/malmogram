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

# Population pyramid in 2020, mg29 ----
dat_temp <- dat %>% 
  filter(År == 2020) %>% 
  group_by(Ålder, Kön) %>% 
  summarise(Antal = sum(Antal)) %>% 
  mutate(Antal_temp = ifelse(Kön == "Kvinnor", -Antal, Antal))

ggplot(dat_temp, aes(Ålder, Antal_temp, fill = Kön)) + 
  geom_bar(stat = "identity", width = 1) +
  coord_flip()

# Population by age and sex, mg30 ----
dat_temp <- dat %>% 
  filter(År == 2020) %>% 
  group_by(Ålder, Kön) %>% 
  summarise(Antal = sum(Antal)) %>% 
  mutate(Antal_temp = ifelse(Kön == "Kvinnor", -Antal, Antal))

g1 <- ggplot(dat_temp, aes(Ålder, Antal, fill = Kön)) + 
  geom_bar(stat = "identity", width = 1) +
  facet_wrap( ~ Kön) +
  theme(legend.position = "none")

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

g1 / g2

# Gender ratio (women to men) by age, mg31 ----
dat_temp <- dat %>% 
  filter(År == 2020) %>% 
  group_by(Ålder, Kön) %>% 
  summarise(Antal = sum(Antal)) %>% 
  pivot_wider(names_from = Kön, values_from = Antal) %>% 
  mutate(Kvot = Kvinnor / Män)

mg31 <- dat_temp %>% 
  ggplot(aes(Ålder, Kvot)) + 
  geom_line() + 
  geom_point()
mg31

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

dat_temp %>% 
  ggplot(aes(År, Antal, fill = Åldersgrupp)) +
  geom_bar(stat = "identity", position = position_fill(), width = 1) +
  facet_wrap(~ Kön)


# Lines per age-group, population count, mg33 ----
dat_temp <- dat %>% 
  group_by(År, Ålder, Kön) %>% 
  summarise(Antal = sum(Antal)) %>% 
  mutate(År = as.numeric(År)) %>% 
  ungroup() %>% group_by(År, Kön) %>% 
  arrange(Ålder) %>% 
  mutate(Kumulativt_antal = cumsum(Antal))

dat_temp %>% 
  ggplot(aes(År, Kumulativt_antal, group = Ålder)) +
  geom_line(color = "grey70") +
  geom_line(data = dat_temp %>% filter(Ålder %in% seq(0, 100, 10))) +
  facet_wrap(~ Kön)

# Lines per age-group, population proportion, mg34 ----
dat_temp <- dat %>% 
  group_by(År, Ålder, Kön) %>% 
  summarise(Antal = sum(Antal)) %>% 
  mutate(År = as.numeric(År)) %>% 
  ungroup() %>% group_by(År, Kön) %>% 
  arrange(Ålder) %>% 
  mutate(Kumulativt_antal = cumsum(Antal),
         p = Kumulativt_antal / max(Kumulativt_antal))

dat_temp %>% 
  ggplot(aes(År, p, group = Ålder)) +
  geom_line(color = "grey70") +
  geom_line(data = dat_temp %>% filter(Ålder %in% seq(0, 100, 10))) +
  facet_wrap(~ Kön)

# Gender-ratio as path diagram, mg35 ----
dat_temp <- dat %>% 
  group_by(År, Kön) %>% 
  summarise(Antal = sum(Antal)) %>% 
  pivot_wider(names_from = Kön, values_from = Antal)

dat_temp %>% 
  ggplot(aes(Kvinnor, Män)) +
  geom_path() +
  geom_point(aes(fill = År), data = dat_temp %>% filter(År %in% seq(1970, 2020, 10)), 
             size = 3, shape = 21) +
  geom_abline(intercept = 0, slope = 1, col = "grey50") +
  coord_equal()

# Marriage status over time, mg36 ----
dat %>% 
  filter(Ålder > 19) %>% 
  ggplot(aes(År, Antal, fill = Civilstånd)) +
  geom_bar(stat = "identity", position = position_fill(), width = 1) +
  facet_wrap(~ Kön)

# Marriage status at 30, mg37 ----
dat %>% 
  filter(Ålder == 40,
          År %in% seq(1970, 2020, 10)) %>% 
  ggplot(aes(0, Antal, fill = Civilstånd)) +
  geom_bar(stat = "identity", position = position_fill(), color = "black") +
  facet_grid(Kön ~ År) +
  xlim(-1.5,1) +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.grid = element_blank())

# This would make an interesting animation