# ---
# Title: Weather. Temperatur. Plots
# Purpose: Create plots for weather data - temperature
# Start date: 211024
# Comments:
# ---

# Packages ----
library(tidyverse)
library(patchwork)
library(cowplot)

# Import
source("R/Klimat/Weather-station-setup.R", encoding = "UTF-8")

# Mean temperature by time, mg22 ----
g1 <- dat_weatherstation %>% 
  mutate(År = year(Datum)) %>% 
  group_by(Station, Datum, År) %>% 
  summarise(Lufttemperatur = mean(Lufttemperatur)) %>%  
  ungroup() %>% 
  group_by(År, Station) %>% 
  summarise(Lufttemperatur = mean(Lufttemperatur), n = n()) %>% 
  filter(!(Station == "a" & År %in% c(1917, 1964)),
         !(Station == "b" & År %in% 1926),
         !(Station == "c" & År %in% 2021)) %>% 
  ggplot(aes(År, Lufttemperatur, col = Station, linetype = Station, shape = Station)) + 
  geom_point(size = 3) + 
  geom_line(size = 0.5) +
  scale_color_manual(values = c("black", "#ff3333" ,"#669933")) +
  # scale_color_brewer(palette = "Greens") +
  theme_mg4()

g2 <- ggplot() + 
  geom_sf(aes(fill = stadsdel), data = dat_adm_delomr, alpha = 0.25) +
  geom_sf_text(aes(label = Station), data = dat_meta_station) +
  scale_fill_discrete(guide = "none") +
  theme_mg4() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "#ffffff"))

mg22 <- g1 + g2 + guide_area() + 
  plot_layout(design = c(area(1,1,4,4), area(1,5,2,5), area(3,5,4,5)), 
              guides = "collect") & theme(plot.background = element_rect("#ff5555", color = "#ff5555"))
mg22

# g1 + inset_element(g2, left = 0.7, bottom = 0.5, right = 1, top = 1)

# Max and min temperature by time, mg23 ----
g1 <- dat_weatherstation %>% 
  mutate(År = year(Datum)) %>% 
  group_by(År, Station) %>% 
  summarise(Max = max(Lufttemperatur),
            Min = min(Lufttemperatur)) %>% 
  filter(!(Station == "a" & År %in% c(1917, 1964)),
         !(Station == "b" & År %in% 1926:1940),
         !(Station == "c" & År %in% 2021)) %>%
  pivot_longer(-c(År, Station), names_to = "Min/Max", values_to = "Lufttemperatur") %>% 
  ggplot(aes(År, Lufttemperatur, col = Station, shape = `Min/Max`, linetype = Station)) + 
  geom_point(size = 2) + 
  geom_line(size = 1) +
  scale_linetype_discrete(guide = "none") +
  scale_color_manual(values = c("black", "#ff3333" ,"#669933")) +
  theme_mg4()

mg23 <- g1 + g2 + guide_area() + 
  plot_layout(design = c(area(1,1,4,4), area(1,5,2,5), area(3,5,4,5)), 
              guides = "collect") & theme(plot.background = element_rect("#ff5555", color = "#ff5555"))
mg23

# Temperature by day 1960s and 2010s, mg24 ----
dat_temp <- dat_weatherstation %>% 
  filter(Station != "a") %>% 
  mutate(Årsdag = yday(Datum), År = year(Datum), Timma = hour(`Tid (UTC)`)) %>%
  filter(Timma == 12) %>% 
  filter(År %in% c(1960:1969, 2010:2019)) %>% 
  filter(Årsdag < 366) %>% 
  group_by(Årsdag, År, Timma) %>% 
  summarise(Lufttemperatur = mean(Lufttemperatur)) %>% 
  mutate(Årtionde = paste0(as.character(floor(År / 10) * 10), "-talet")) %>% 
  ungroup() %>% 
  mutate(Årsdag = as.Date(Årsdag - 1, origin = "2000-01-01"))

mg24 <- dat_temp %>% 
  ggplot(aes(Årsdag, Lufttemperatur, group = År, col = Årtionde)) + 
  geom_line(alpha = 0.10) +
  geom_line(data = dat_temp %>% group_by(Årtionde, Årsdag) %>% summarise_all(mean), 
            size = 1.2) +
  scale_x_date(date_breaks = "2 month", date_labels = "%B", expand = c(0,0)) +
  scale_color_manual(values = c("#ff3333", "#669933")) +
  theme_mg4() +
  theme(panel.grid.major.x = element_line(color = "grey80"),
        legend.text = element_text(size = 15))
mg24

# Temperature difference per date, 1960s and 2010, mg25 ----
dat_temp <- dat_temp %>% 
  group_by(Årtionde, Årsdag) %>% 
  summarise_all(mean) %>%
  select(-År, -Timma) %>% 
  pivot_wider(names_from = Årtionde, values_from = Lufttemperatur)

dat_temp %>% 
  mutate(Differens = `2010-talet` - `1960-talet`) %>% 
  pivot_longer(-Årsdag) %>% 
  group_by(name) %>% 
  summarise(Medelvärde = mean(value), Median = median(value))

mg25 <- dat_temp %>% 
  ggplot(aes(Årsdag, `2010-talet` - `1960-talet`)) + 
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = median(dat_temp$`2010-talet` - dat_temp$`1960-talet`), col = "red", size = 5, alpha = 0.25) +
  scale_x_date(date_breaks = "2 month", date_labels = "%B", expand = c(0,0)) +
  theme_mg4() +
  theme(panel.grid.major.x = element_line(color = "grey80"),
        legend.text = element_text(size = 15)) +
  labs(y = "Medellufttemperatur 2010-tal minus medellufttemperatur 1960-tal")
mg25

# Temperature during the day and year, mean 2010s, mg26 ----
mg26 <- dat_weatherstation %>% 
  filter(Station == "c") %>% 
  mutate(Timme = hour(`Tid (UTC)`),
         Årsdag = yday(Datum),
         År = year(Datum)) %>% 
  filter(År %in% 2010:2019) %>% 
  group_by(Årsdag, Timme) %>% 
  summarise(Lufttemperatur = mean(Lufttemperatur, na.rm = T)) %>% 
  mutate(Lufttemperatur_bins = cut(Lufttemperatur, seq(-50, 50, 5))) %>% 
  ggplot(aes(ymin = Årsdag, xmin = Timme, ymax = Årsdag + 1, xmax = Timme + 1, fill = Lufttemperatur_bins)) +
  geom_rect() +
  geom_vline(xintercept = seq(0,24,1)) +
  scale_x_continuous(breaks = seq(0, 24, 6), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c("#eff3ff", RColorBrewer::brewer.pal(5, "Reds"))) +
  theme_mg4() +
  theme(legend.text = element_text(size = 10))
mg26

# Temperatur per hour and month, mg27 ----
mg27 <- dat_weatherstation %>% 
  mutate(Timme = hour(`Tid (UTC)`),
         Årsdag = yday(Datum),
         Månad = month(Datum, label = T, abbr = F),
         År = year(Datum)) %>% 
  mutate(Månad = forcats::fct_relabel(Månad, tools::toTitleCase)) %>% 
  filter(År %in% 2000:2020) %>% 
  group_by(Månad) %>% 
  mutate(Medeltemperatur_månad = mean(Lufttemperatur)) %>% 
  group_by(Timme, Månad) %>% 
  summarise_at(c("Lufttemperatur", "Medeltemperatur_månad"), mean) %>% 
  ggplot(aes(Timme, Lufttemperatur)) + 
  geom_line() +
  geom_line(aes(y = Medeltemperatur_månad)) +
  facet_wrap(~ Månad, ncol = 12) +
  scale_x_continuous(breaks = seq(6, 18, 6), expand = c(0,0)) +
  labs(title = "Medelufttemperatur i Malmö per timme och månad, 2000 - 2020",
       caption = "Heldragen linje anger medeltemperatur per timme för de angivna åren.
       Horisontell linje anger medeltemperatur per månad.
       Källa: ") +
  theme_mg4() +
  theme(panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color = "grey80"),
        strip.background = element_rect(fill = "#ffbbbb"),
        axis.title.x = element_blank())
mg27

# Temperatur per day and year, mg28 ----
dat_temp <- dat_weatherstation %>% 
  mutate(Årsdag = yday(Datum),
         År = year(Datum),
         Timme = hour(`Tid (UTC)`)) %>%
  filter(Timme == 12) %>% 
  filter(Årsdag <= 365) %>% 
  filter(År >= 1950, År < 2011) %>% 
  group_by(Årsdag, År) %>% 
  summarise(Lufttemperatur = mean(Lufttemperatur)) %>% 
  ungroup() %>% group_by(Årsdag) %>% 
  mutate(Mediandagstemperatur = median(Lufttemperatur),
         Årtionde = paste0(floor(År / 10) * 10, "-talet"),
         År_mod_10 = År %% 10,
         Status_mot_medel = ifelse(Lufttemperatur > Mediandagstemperatur, "Varmare än median", "Kallare än median"))

mg28 <- dat_temp %>% 
  ggplot(aes(Årsdag, Lufttemperatur, fill = Status_mot_medel)) +
  geom_bar(stat = "identity", width = 1) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("#5555ff", "#ff5555")) +
  scale_y_continuous(breaks = seq(-15,15,15)) +
  labs(title = "Lufttemperatur mot årsdag. Malmö, kl 12, 1950 - 2021",
       caption = "Färg anger om dagens temperatur är högre eller lägre än den genomsnittliga temperaturen för den årsdagen över samtliga år.") +
  facet_grid(Årtionde ~ År_mod_10) +
  theme_mg4() +
  theme(legend.text = element_text(size = 13),
        legend.position = "bottom", 
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        strip.background.y = element_blank())
mg28
