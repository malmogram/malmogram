# ---
# Title: Oresund bridge -plots
# Purpose: 
# Start date: 211217
# Comments:
# ---

# Packages ----
library(tidyverse)
library(scales)

# Read setup
source("R/Transporter och kommunikationer/Oresund-bridge-setup.R", encoding = "UTF-8")
source("R/Teman/Themes.R", encoding = "UTF-8")

names(dat_ore)[3:7] <- c("MC", "Bil", "Bil med trailer,\ncampingvagn, eller minibus", "Buss", "Lastbil över 6 m")

# Yearly totals, mg81 ----
mg81 <- dat_ore %>% 
  group_by(Year) %>% 
  summarise(Total = sum(`In total`)) %>% 
  filter(Year < 2022) %>% 
  ggplot(aes(Year, Total)) +
  geom_col(width = 0.5) +
  scale_y_continuous(labels = comma_format(big.mark = " "), limits = c(0, 8e6)) +
  labs(title = "Motortrafik över Öresundsbron, 2000 - 2021",
       x = "År", y = "Totalt antal fordon",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 81
       12 juni 2022") +
  theme_bridge()
mg81
# ggsave("Output/Transporter och kommunikationer/081-Motortrafik-Oresundsbron.png", mg81, width = 6, height = 4)

# Total timeline, mg82 ----
mg82 <- dat_ore %>% 
  mutate(Year_month = lubridate::ym(paste0(Year, "-", Month))) %>% 
  ggplot(aes(Year_month, `In total`)) +
  geom_line() +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  labs(title = "Motortrafik över Öresundsbron, 2000 - 2021",
       x = "", y = "Antal per månad",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 82
       12 juni 2022") +
  theme_bridge()
mg82
# ggsave("Output/Transporter och kommunikationer/082-Motortrafik-Oresundsbron-per-månad.png", mg82, width = 6, height = 4)

# Total timeline 2017 - 2022, mg83 ----
mg83 <- dat_ore %>% 
  mutate(Year_month = lubridate::ymd(paste0(Year, "-", Month, "-", 15))) %>%
  filter(Year %in% 2017:2022) %>% 
  ggplot(aes(Year_month, `In total`)) +
  geom_line() +
  annotate("point", y = 0, x = lubridate::ymd("20220101"), color = NA) +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  labs(title = "Motortrafik över Öresundsbron, 2017 - 2022",
       x = "År", y = "Antal per månad",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 83
       29 juni 2022") +
  theme_bridge()
mg83
ggsave("Output/Transporter och kommunikationer/083-Motortrafik-Oresundsbron-från-2017.png", mg83, width = 6, height = 4)

# Total timeline as heatmap, mg84 ----
mg84 <- dat_ore %>% 
  ggplot(aes(Month, -Year, fill = `In total`)) +
  geom_tile() +
  geom_vline(xintercept = seq(0.5,12.5, 3)) +
  geom_hline(yintercept = seq(-1999.5,-2019.5,-5)) +
  scale_x_continuous(breaks = 1:12, labels = month.abb, expand = c(0,0)) +
  scale_y_continuous(labels = seq(2020, 2000, -5), expand = c(0,0)) +
  scale_fill_gradient2(low = "white", high = "black", labels = comma_format(big.mark = " ")) +
  labs(title = "Motortrafik över Öresundsbron, 2000 - 2021",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 84
       12 juni 2022") +
  theme_trees() +
  theme(panel.grid = element_blank())
mg84

# Yearly totals by vehicle type, mg85 ----
mg85 <- dat_ore %>% 
  select(-`In total`, -stat_id) %>% 
  pivot_longer(-c(Year, Month), names_to = "Vehicle", values_to = "Count") %>% 
  mutate(Year_month = lubridate::ymd(paste0(Year, "-", Month, "-", 15))) %>%
  count(Year, Vehicle, wt = Count) %>% 
  filter(Year < 2022) %>% 
  ggplot(aes(Year, n, fill = Vehicle)) +
  geom_col(color = "black", width = 0.5) +
  scale_fill_grey() +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  labs(title = "Motortrafik över Öresundsbron, 2000 - 2021",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 85
       12 juni 2022") +
  theme_trees()
mg85

# Yearly totals by vehicle type, separate facets, mg86 ----
mg86 <- dat_ore %>% 
  select(-`In total`, -stat_id) %>% 
  pivot_longer(-c(Year, Month), names_to = "Vehicle", values_to = "Count") %>% 
  mutate(Year_month = lubridate::ymd(paste0(Year, "-", Month, "-", 15))) %>%
  count(Year, Vehicle, wt = Count) %>%
  filter(Year < 2022) %>% 
  ggplot(aes(Year, n, fill = Vehicle)) +
  geom_line() +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  facet_wrap(~ Vehicle, scales = "free_y", nrow = 1) +
  labs(title = "Motortrafik över Öresundsbron, 2000 - 2021",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 86
       12 juni 2022") +
  theme_trees()
mg86

# Monthly variation by vehicle type, mg87 ----
mg87 <- dat_ore %>% 
  select(-stat_id, -`In total`) %>% 
  pivot_longer(-c(Year, Month)) %>% 
  group_by(Year, name) %>% 
  mutate(Yearly_mean = mean(value),
         Ratio = value / Yearly_mean,
         log2_ratio = log2(Ratio)) %>% 
  filter(Year %in% 2010:2019) %>% 
  ggplot(aes(Month, log2_ratio, col = name)) +
  geom_line(aes(group = paste0(Year, name)), alpha = 0.25) +
  geom_smooth(aes(group = name), se = F) +
  labs(title = "Motortrafik över Öresundsbron, 2000 - 2021",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 87
       12 juni 2022") +
  theme_trees()
mg87

# Monthly variation by vehicle type,separate facets, mg88 ----
mg88 <- dat_ore %>% 
  select(-stat_id, -`In total`) %>% 
  pivot_longer(-c(Year, Month)) %>% 
  filter(Year %in% 2001:2019) %>% 
  ggplot(aes(Month, value)) +
  geom_line(aes(group = paste0(Year, name)), alpha = 0.75) +
  facet_wrap(~ name, scales = "free_y", ncol = 5) +
  geom_text(aes(x, y, label = text), col = "black", size = 2, hjust = 0, nudge_x = 0.2,
            data = data.frame(x = 6, y = 13885, text = "Juni 2014: 13 885 bussar!?", name = "Buss")) +
  scale_x_continuous(breaks = c(4,7,10), labels = c("Apr", "Juli", "Okt")) +
  labs(title = "Motortrafik över Öresundsbron, 2000 - 2021",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 88
       12 juni 2022") +
  theme_trees()
mg88

# Year travels by train, mg89 ----
mg89 <- dat_ore_tag %>% 
  ggplot(aes(År, Resor)) +
  geom_line() +
  labs(title = "Motortrafik över Öresundsbron, 2000 - 2021",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 89
       12 juni 2022") +
  theme_trees()
mg89

# Trains to and from Sweden, mg90 ----
mg90 <- dat_ore_tag %>% 
  select(År, `Dan - Sve, tåg`, `Sve - Dan, tåg`) %>% 
  pivot_longer(-År, names_to = "Riktning", values_to = "Antal") %>% 
  mutate(Riktning = ifelse(Riktning == "Dan - Sve, tåg", "Till Malmö", "Från Malmö")) %>% 
  filter(År %in% 2001:2019) %>% 
  ggplot(aes(År, Antal, col = Riktning)) +
  geom_line(size = 2) +
  scale_color_grey() +
  labs(title = "Motortrafik över Öresundsbron, 2000 - 2021",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 90
       12 juni 2022") +
  theme_trees()
mg90

# Trains to and from Sweden, cumulative numbers, mg91 ----
mg91 <- dat_ore_tag %>% 
  select(År, `Dan - Sve, tåg`, `Sve - Dan, tåg`) %>% 
  pivot_longer(-År, names_to = "Riktning", values_to = "Antal") %>% 
  mutate(Riktning = ifelse(Riktning == "Dan - Sve, tåg", "Till Malmö", "Från Malmö")) %>% 
  filter(År %in% 2000:2020) %>%
  group_by(Riktning) %>% 
  mutate(`Kumulativt antal` = cumsum(Antal)) %>% 
  ggplot(aes(År, `Kumulativt antal`, col = Riktning)) +
  geom_line() +
  labs(title = "Motortrafik över Öresundsbron, 2000 - 2021",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 91
       12 juni 2022") +
  theme_trees()
mg91

# Trains to and from Sweden, cumulative difference, mg92 ----
mg92 <- dat_ore_tag %>% 
  select(År, `Dan - Sve, tåg`, `Sve - Dan, tåg`) %>% 
  pivot_longer(-År, names_to = "Riktning", values_to = "Antal") %>% 
  mutate(Riktning = ifelse(Riktning == "Dan - Sve, tåg", "Till Malmö", "Från Malmö")) %>% 
  filter(År %in% 2000:2020) %>%
  group_by(Riktning) %>% 
  mutate(`Kumulativt antal` = cumsum(Antal)) %>%
  select(-Antal) %>% 
  pivot_wider(names_from = Riktning, values_from = `Kumulativt antal`) %>% 
  ggplot(aes(År, `Från Malmö` - `Till Malmö`)) +
  geom_line() +
  labs(title = "Motortrafik över Öresundsbron, 2000 - 2021",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 92
       12 juni 2022") +
  theme_trees()
mg92  

# Cars against trucks, path graph, mg102 ----
mg102 <- dat_ore %>%
  filter(Year %in% 2016:2021) %>% 
  ggplot(aes(Bil, `Lastbil över 6 m`, size = lubridate::ymd(paste(Year, "-", Month, "-", 15)))) +
  geom_point() +
  geom_path() +
  scale_size_date(range = c(0.2,2)) +
  labs(title = "Motortrafik över Öresundsbron, 2000 - 2021",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 102
       12 juni 2022") +
  theme_bridge() +
  theme(legend.position = "none")
mg102

# Bubble chart of all vehicle types, mg103 ----
dat_ore %>% 
  mutate(Date = lubridate::ymd(paste(Year, Month, 15, sep = "-"))) %>% 
  select(Date, Year, 3:7) %>% 
  filter(Year > 2015) %>% 
  pivot_longer(-c(1,2), names_to = "Vehicle", values_to = "Count") %>% 
  group_by(Vehicle) %>% 
  mutate(Count = Count/mean(Count)) %>% 
  ggplot(aes(y = Vehicle, x = Date, size = Count, color = Count)) +
  geom_point(shape = "|") +
  scale_size_continuous(range = c(1,15)) +
  scale_color_gradient2(low = "darkgreen", high = "red") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  labs(title = "Motortrafik över Öresundsbron, 2000 - 2021",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 103
       12 juni 2022") +
  theme_bridge() +
  theme(legend.position = "none")

mg103 <- dat_ore %>% 
  mutate(Date = lubridate::ymd(paste(Year, Month, 15, sep = "-"))) %>% 
  select(Date, Year, 3:7) %>% 
  filter(Year > 2015) %>% 
  pivot_longer(-c(1,2), names_to = "Vehicle", values_to = "Count") %>% 
  mutate("Count" = Count / 1000,
         Vehicle = ordered(Vehicle, unique(Vehicle)[c(2,3,1,4,5)])) %>% 
  ggplot(aes(y = Count, x = Date)) +
  geom_line() +
  annotate("point", x = lubridate::ymd("2022-01-01"), y = 0, color = NA) +
  facet_wrap(~ Vehicle, scales = "free_y", ncol = 1, strip.position = "left") +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  labs(title = "Motortrafik över Öresundsbron per fordonstyp, 2016 - 2022",
       x = "", y = "Antal per månad (tusental)",
       caption = "Källa: Öresundsbro konsortiet
       https://www.oresundsbron.com/en/traffic-stats
       
       Malmögram 103
       1 juli 2022") +
  theme_bridge() +
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1))
mg103
ggsave("Output/Transporter och kommunikationer/103-Motortrafik-Oresundsbron-per-typ.png", mg103, width = 8, height = 9)
