# ---
# Title: MFF in allsvenskan for men. Plots
# Purpose: Plotting of allsvenskan data
# Start date: 211003
# Comments:
# ---

# Packages ----
library(tidyverse)
library(lubridate)
library(ggrepel)
library(pammtools)
library(forcats)

# Data import
source("r/Idrott/Allsvenskan-herrar-data-setup.R", encoding = "UTF-8")
source("r/Teman/Themes.R", encoding = "UTF-8")

# Proportion wins by year. mg8 ----
dat_temp <- dat_long %>% 
  filter(Fokuslag == "Malmö FF") %>% 
  mutate(Utfall = ifelse(`Gjorda mål` > `Insläppta mål`, "Seger", "Oavgjort eller Förlust")) %>% 
  group_by(Säsong, Plats) %>%
  summarise(Segrar = sum(Utfall == "Seger"),
            Matcher = n()) %>% 
  mutate(`Andel segrar` = Segrar / Matcher,
         År = substring(Säsong, 1, 4) %>% as.numeric(),
         Plats = ordered(Plats, c("Hemma", "Borta")))
g_mg8 <- dat_temp %>% 
  ungroup() %>% group_by(Plats) %>% 
  mutate(Obruten_grupp = cumsum(!((År - 1) %in% dat_temp$År))) %>%
  ggplot(aes(År, `Andel segrar`, col = Plats, group = paste0(Plats, Obruten_grupp))) +
  geom_step(size = 2, direction = "mid") +
  scale_x_continuous(breaks = seq(1900, 2100, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0,1)) +
  scale_color_manual(values = mff_cols[c(3,4)]) +
  facet_wrap(~ Plats, ncol = 1) +
  labs(title = "Malmö FF herr. Vinstandel i Allsvenskan per säsong", 
       caption = "Källa: Svenska fotbollförbundet SvFF
       
       Malmögram 8
       16 januari 2022") +
  theme_mg2_mff() + 
  theme(legend.position = "none")
g_mg8

ggsave("Output/Idrott/008-MFF-men-win-proportion.png", g_mg8, height = 6, width = 10)

# All MFF games played in Allsvenskan, mg9 ----
mg9 <- dat_long %>% 
  filter(Fokuslag == "Malmö FF") %>% 
  mutate(År = year(Datum), Årsdag = yday(Datum) + wday(paste0(År, "-01-01")),
          Utfall = ordered(Utfall, c("Seger", "Oavgjort", "Förlust"))) %>% 
  ggplot(aes(Årsdag, År, fill = Utfall)) +
  geom_point(shape = 21, col = "black", size = 2) +
  scale_fill_manual(values = c(mff_cols[2], "white", mff_cols[4])) +
  scale_y_continuous(breaks = seq(1900, 2100, 10)) +
  labs(title = "Malmö FF herr. Matcher i Allsvenskan 1931-2021", 
       x = "Årsdag, justerat för att matcha veckodag över år",
       caption = "Källa: Svenska fotbollförbundet SvFF
       
       Malmögram 9
       16 januari 2022") +
  theme_mg2_mff() + 
  theme(panel.grid.major.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom", legend.title = element_blank())
mg9

ggsave("Output/Idrott/009-MFF-men-allsvenskan-games.png", mg9, height = 10, width = 6)

# MFF games in Allsvenskan 2008-2021, mg10 ----
dat_long %>% 
  filter(Fokuslag == "Malmö FF", Säsong %in% 2008:2021) %>% 
  mutate(Säsong = ordered(Säsong, 2021:2008)) %>% 
  group_by(Plats, Säsong) %>% 
  mutate(Matchnr = 1:n()) %>% 
  ggplot(aes(Matchnr, Säsong, fill = Utfall)) +
  geom_point(shape = 21, size = 6) +
  facet_wrap(~ Plats) +
  theme_mg2_mff()

mg10 <- dat_long %>% 
  filter(Fokuslag == "Malmö FF", Säsong %in% 2008:2021) %>% 
  mutate(Säsong = as.numeric(Säsong),
         Utfall = ordered(Utfall, c("Seger", "Oavgjort", "Förlust")),
         Plats = ordered(Plats, c("Hemma", "Borta"))) %>% 
  group_by(Plats, Säsong) %>% 
  mutate(Matchnr = 1:n()) %>% 
  ggplot(aes(Matchnr, -Säsong, shape = Utfall, col = Utfall)) +
  geom_point(size = 6) +
  scale_shape_manual(values = c("_", "/", "|")) +
  scale_y_continuous(breaks = -2008:-2021, labels = 2008:2021) +
  scale_x_continuous(breaks = seq(0,10,5)) +
  scale_color_manual(values = c(mff_cols[2], "white", mff_cols[4])) +
  facet_wrap(~ Plats) +
  labs(title = "Malmö FF herr i allsvenskan, 2008 - 2021",
       x = "Matchnummer", y = "Säsong",
       caption = "Källa: Svenska fotbollförbundet SvFF
       
       Malmögram 10
       16 januari 2022") +
  theme_mg2_mff() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(color = "black"),
        axis.title.y = element_blank(),
        legend.position = "bottom", legend.title = element_blank())
mg10
ggsave("Output/Idrott/010-MFF-men-allsvenskan-games-2008-2021.png", mg10, height = 6, width = 10)

# Cumulative results. Wins +1, ties 0, losses -1, mg11 ----
dat_temp <- dat_long %>% 
  filter(Fokuslag == "Malmö FF") %>% 
  group_by(Plats) %>% 
  mutate(Utfall_num = (Utfall == "Seger") - (Utfall == "Förlust"),
         Kumulativ_summa = cumsum(Utfall_num),
         Matchnr = 1:n()) %>%
  ungroup() %>% 
  mutate(År = year(Datum)) %>% 
  group_by(År, Plats) %>% 
  mutate(Omgång = 1:n(),
         Plats = ordered(Plats, c("Hemma", "Borta")))
dat_temp2 <- dat_temp %>% 
  filter(År %in% seq(1931, 2021, 10), Omgång == 1, Plats == "Hemma")

mg11 <- ggplot(dat_temp, aes(Matchnr, Kumulativ_summa, color = Plats)) +
  geom_step(size = 1.5) +
  geom_point(data = dat_temp2 %>% filter(År != 1931), size = 6, shape = "|", show.legend = F) +
  geom_text(aes(y = Kumulativ_summa + 30, label = År), data = dat_temp2, show.legend = F) +
  scale_color_manual(values = c(mff_cols[2], mff_cols[4])) +
  labs(title = "Malmö FF herr i allsvenska. Vunna minus förlorade mot antal spelade matcher, 1931 - 2021",
       x = "Spelade matcher", y = "Antal vunna - Antal förlorade",
       caption = "Källa: Svenska fotbollförbundet SvFF
       
       Malmögram 11
       16 januari 2022") +
  theme_mg2_mff()
mg11

ggsave("Output/Idrott/011-MFF-men-allsvenskan-cumulative-result.png", mg11, height = 6, width = 10)

# Scored and conceded goals. Mean per season, mg12 ----
dat_temp <- dat_long %>% 
  filter(Fokuslag == "Malmö FF") %>% 
  group_by(Säsong, Plats) %>% 
  summarise(`Gjorda mål, medel` = mean(`Gjorda mål`),
            `Insläppta mål, medel` = mean(`Insläppta mål`)) %>% 
  pivot_longer(-c(Säsong, Plats), names_to = "Måltyp", values_to = "Medelantal") %>% 
  mutate(År = substring(Säsong, 1, 4) %>% as.numeric(),
          År = ifelse(År < 1958, År + 1, År),
          Medelantal = ifelse(Måltyp == "Insläppta mål, medel", -Medelantal, Medelantal),
          Plats = ordered(Plats, c("Hemma", "Borta")))
mg12 <- dat_temp %>% 
  ungroup() %>% group_by(Plats, Måltyp) %>% 
  mutate(Obruten_grupp = cumsum(!((År - 1) %in% dat_temp$År))) %>%
  ggplot(aes(År, Medelantal, col = Plats, 
              group = paste0(Plats, Måltyp, Obruten_grupp))) +
  geom_step(direction = "mid", size = 1.2, alpha = 0.75) +
  scale_x_continuous(breaks = seq(1900, 2100, 10)) +
  scale_color_manual(values = c(mff_cols[2], mff_cols[4])) +
  labs(title = "Malmö FF herr i Allsvenskan. Gjorda och insläppa mål, hemma och borta, 1931 - 2021",
       y = "Insläppta mål  |  Gjorda mål",
       caption = "Källa: Svenska fotbollförbundet SvFF
       
       Malmögram 12
       16 januari 2022") +
  theme_mg2_mff() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        axis.title.y = element_text(hjust = 0.47)) +
  coord_cartesian(ylim = c(-4,4))
mg12

ggsave("Output/Idrott/012-MFF-men-allsvenskan-scored-and-conceded-over-time.png", mg12, height = 6, width = 10)

# Scored and conceded goals scattergram, mg13 ----
dat_temp <- dat_long %>% 
  filter(Fokuslag == "Malmö FF") %>% 
  group_by(Säsong, Plats) %>% 
  summarise(`Gjorda mål, medel` = mean(`Gjorda mål`),
            `Insläppta mål, medel` = mean(`Insläppta mål`)) %>% 
  mutate(År = substring(Säsong, 1, 4) %>% as.numeric(),
          År = ifelse(År < 1958, År + 1, År),
          `Allsvensk seger` = (År %in% allsvenska_segrar),
          Plats = ordered(Plats, c("Hemma", "Borta")))

dat_temp2 <- dat_temp %>% 
  mutate(Kvot = `Gjorda mål, medel` / `Insläppta mål, medel`) %>% 
  mutate(Intressant = (`Gjorda mål, medel` > 3.8) | 
           (`Insläppta mål, medel` < 0.3) |
           (`Insläppta mål, medel` > 2.5) |
           (`Gjorda mål, medel` > 2.9 & Plats == "Borta") |
           (Kvot < 1 & `Allsvensk seger`) |
           (`Insläppta mål, medel` > 1.79 & Plats == "Hemma") |
           (`Gjorda mål, medel` > 2 & Plats == "Borta" & `Insläppta mål, medel` < 1)) %>% 
  filter(Intressant) %>% 
  mutate(adjust_h = ifelse(`Gjorda mål, medel` > 2.9 | Säsong == 1986, 0, 1),
         nudge_x = ifelse(adjust_h == 0, 0.05, -0.05))

mg13 <- ggplot(dat_temp, aes(`Gjorda mål, medel`, `Insläppta mål, medel`)) + 
  annotate("segment", x = 0, y = 0, xend = 5, yend = 5, col = "white") +
  geom_point(shape = 21, col = "black", fill = mff_cols[2], size = 2) +
  geom_point(shape = 21, col = "black", fill = "#d4af37", size = 2, 
             data = dat_temp %>% filter(`Allsvensk seger`)) +
  geom_text(aes(label = Säsong), data = dat_temp2, col = mff_cols[3], 
            family = "Garamond", hjust = dat_temp2$adjust_h, size = 3, nudge_x = dat_temp2$nudge_x) +
  xlim(0,5) + ylim(0,5) +
  facet_wrap(~ Plats) +
  labs(title = "Malmö FF herr i allsvenskan. Gjorda mot insläppa mål, 1931 - 2021",
       x = "Gjorda mål, medel", y = "Insläppta mål, medel",
       caption = "Källa: Svenska fotbollförbundet SvFF
       
       Malmögram 13
       16 januari 2022") +
  theme_mg2_mff() +
  theme(legend.position = "none")
mg13

ggsave("Output/Idrott/013-MFF-men-allsvenskan-scored-conceded-scattergram.png", mg13, height = 8, width = 12)

# Results against specific opponents, mg14 ----
dat_temp <- dat_long %>% 
  select(-c(Publik, Domare, Arena, `Gjorda mål`, `Insläppta mål`)) %>% 
  filter(Fokuslag == "Malmö FF") %>% 
  group_by(Motståndare) %>% mutate(Antal_matcher = n()) %>% 
  filter(Antal_matcher >= 50) %>% 
  mutate(År = substring(Säsong, 1, 4) %>% as.numeric(),
          År = ifelse(År < 1958, År + 1, År),
          Utfall_num = (Utfall == "Seger") - (Utfall == "Förlust")) %>% 
  group_by(Motståndare, Plats) %>% 
  mutate(Kumulativ_resultat = cumsum(Utfall_num),
         Matchnr = 1:n()) %>% 
  mutate(gr = cumsum(Kumulativ_resultat == 0))

dat_temp <- dat_temp %>% 
  bind_rows(dat_temp %>% 
              filter(Kumulativ_resultat == 0) %>% 
              mutate(gr = gr - 1)) %>% 
  ungroup() %>% group_by(Motståndare, Plats, gr) %>% 
  mutate(Pos_neg = as.character(sign(mean(Kumulativ_resultat))))

ggplot(dat_temp, aes(Matchnr, ymin = 0, 
                     ymax = Kumulativ_resultat, group = gr, fill = Pos_neg)) +
  geom_stepribbon() +
  geom_step(aes(y = Kumulativ_resultat)) +
  # geom_hline(yintercept = 0) +
  facet_grid(Motståndare ~ Plats)

# Results against specific opponents, bar version, mg14 ----
limit_no_of_matches <- 50

dat_temp <- dat_long %>% 
  select(-c(Publik, Domare, Arena, `Gjorda mål`, `Insläppta mål`)) %>% 
  filter(Fokuslag == "Malmö FF") %>% 
  group_by(Motståndare) %>% 
  mutate(Antal_matcher = n()) %>% 
  filter(Antal_matcher >= limit_no_of_matches) %>% 
  mutate(År = substring(Säsong, 1, 4) %>% as.numeric(),
          År = ifelse(År < 1958, År + 1, År),
          Utfall_num = (Utfall == "Seger") - (Utfall == "Förlust")) %>% 
  group_by(Motståndare, Plats) %>% 
  mutate(Kumulativt_resultat = cumsum(Utfall_num),
         Matchnr = 1:n()) %>% 
  ungroup() %>% 
  mutate(Motståndare = fct_reorder(Motståndare, Motståndare, length, .desc = T),
         Plats = ordered(Plats, c("Hemma", "Borta"))) %>% 
  group_by(Motståndare, Plats)
#  mutate(Matchnr = Matchnr + 50 - n() / 2) # FIX LATER

dat_temp2 <- dat_long %>% 
  select(-c(Publik, Domare, Arena, `Gjorda mål`, `Insläppta mål`)) %>% 
  filter(Fokuslag == "Malmö FF") %>% 
  count(Motståndare, Plats, Utfall) %>% 
  pivot_wider(values_from = n, names_from = Utfall, values_fill = 0) %>% 
  filter(Motståndare %in% dat_temp$Motståndare) %>% 
  mutate_at(3:5, ~ ifelse(.x < 10, paste0(" ", .x), .x)) %>% 
  mutate(Resultat = paste(Seger, Oavgjort, Förlust, sep = " - ")) %>% 
  mutate(Motståndare = fct_reorder(Motståndare, Motståndare, length, .desc = T),
         Plats = ordered(Plats, c("Hemma", "Borta")))

mg14 <- ggplot(dat_temp, aes(ifelse(Plats == "Hemma", -Matchnr, Matchnr), 
                     y = Kumulativt_resultat, fill = as.character(sign(Kumulativt_resultat)))) +
  geom_bar(stat = "identity", width = 1) +
  geom_segment(aes(ifelse(Plats == "Hemma", -Matchnr - 0.5, Matchnr + 0.5), 0, xend = 0, yend = 0)) +
  geom_label(aes(x = ifelse(Plats == "Hemma", -100, 100), y = 0, 
                label = Resultat, hjust = ifelse(Plats == "Hemma", 0, 1)), 
            inherit.aes = F, data = dat_temp2, vjust = 0.2,
            family = "Garamond", col = mff_cols[3], fill = "transparent",
            label.size = NA, size = 3) +
  facet_grid(Motståndare ~ Plats, scale = "free_x", switch = "y") +
  scale_fill_manual(values = c("red", "black", "darkgreen")) +
  scale_x_continuous(expand = c(0,0), breaks = seq(-100, 100, 25), 
                     labels = abs(seq(-100, 100, 25))) +
  ylim(-max(abs(dat_temp$Kumulativt_resultat)), max(abs(dat_temp$Kumulativt_resultat))) +
  labs(title = "Malmö FF herr i allsvenskan. Aggregerat resultat, vinster - förluster, 1931 - 2021", 
       x = "Antal matcher. Text anger vunna - oavgjorda - förluster.", y = "Aggregerat resultat, vinster - förluster",
       caption = "Källa: Svenska fotbollförbundet SvFF
       
       Malmögram 14
       16 januari 2022") +
  theme_mg2_mff() +
  theme(legend.position = "none",
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text.y.left = element_text(angle = 0))
mg14

ggsave("Output/Idrott/014-MFF-men-allsvenskan-cumulative-result-by-opponent.png", mg14, height = 14, width = 10)
