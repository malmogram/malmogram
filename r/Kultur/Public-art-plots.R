# ---
# Title: Public art. Malmö art map - plots
# Purpose: 
# Start date: 211210
# Comments:
# ---

library(tidyverse)

# Read setup file ----
source("R/Kultur/Public-art-setup.R", encoding = "UTF-8")
source("R/Geografi/Parts-of-the-City-setup.R", encoding = "UTF-8")
source("R/Teman/Themes.R")
dat_namn <- read_csv("Data/Allmänt/Tilltalsnamn-statistik-SCB.csv")

# Re-used functions ----
split_at_lower_case_upper_case <- function(text){
  parts <- strsplit(text, "[[:lower:]][[:upper:]]")[[1]]
  if (length(parts) > 1) {
    extracts <- stringr::str_extract_all(text, "[[:lower:]][[:upper:]]")[[1]]
    for(i in 1:(length(parts) - 1)) parts[i] <- paste0(parts[i], substring(extracts[i], 1, 1))
    for(i in 2:(length(parts))) parts[i] <- paste0(substring(extracts[i - 1], 2, 2), parts[i])
  }
  parts
}

# Art on the map, mg69 ----
mg69 <- dat_adm_delomr %>% 
  st_transform(3008) %>% 
  ggplot() + 
  geom_sf() +
  geom_point(aes(X, Y), color = "black", data = dat_art)
mg69

# Art by material, Malmö stads konstkarta, mg70 ----
dat_temp <- dat_art %>% 
  mutate(material_list = map(material, ~ strsplit(.x, "/")[[1]])) %>%  
  unnest(material_list) %>% 
  mutate(Material = tools::toTitleCase(material_list))

dat_correct <- tibble(Current = c("Vatte", "Va", "Rostfritt Stå", "Stålvajra", "Målad s", "Konstrgräs", "Kalkste", "Grani", "Glasfiberarmerad Pla", "Betongm", "Belys", '\"Hål\"'),
                      Fixed = c("Vatten", "Vatten", "Rostfritt Stål", "Stålvajrar", "Målad sten", "Konstgräs", "Kalksten", "Granit", "Glasfiberarmerad Plast", "Betong", "Belysning", "Hål"))

dat_temp <- dat_temp %>% 
  left_join(dat_correct %>% rename("Material" = "Current")) %>% 
  mutate(Material = ifelse(is.na(Fixed), Material, Fixed))

dat_temp %>% 
  count(Material, sort = T) %>% 
  drop_na(Material) %>% 
  filter(n > 1) %>% 
  mutate(Material = tolower(Material),
         Material = paste0(toupper(substring(Material, 1, 1)), substring(Material, 2, nchar(Material)))) %>% 
  mutate(Material = ordered(Material, Material)) %>% 
  ggplot(aes(n, Material)) + 
  geom_col()

# Art by year, Malmö stads konstkarta, mg71 ----
dat_art %>% 
  mutate(avtackningsar = as.numeric(avtackningsar)) %>% 
  drop_na(avtackningsar) %>% 
  group_by(avtackningsar) %>% 
  mutate(nr = 1:n()) %>% 
  ggplot(aes(avtackningsar, nr)) +
  geom_point() +
  geom_text(aes(label = namn), data = . %>% filter(avtackningsar < 1910), 
            angle = 90, hjust = 0, nudge_y = 0.5, nudge_x = -0.5) +
  scale_x_continuous(breaks = seq(1860, 2020, 20))

# Art on the map by decade, Malmö stads konstkarta, mg72 ----
dat_temp <- dat_adm_delomr %>% 
  st_transform(3008)

mg72 <- ggplot() +
  geom_sf(aes(fill = stadsdel), alpha = 0.2, data = dat_temp) +
  geom_sf(data = dat_art_geo, alpha = 0.04) +
  geom_sf(data = dat_art_geo %>% 
            mutate(Årtionde = floor(as.numeric(avtackningsar) / 10) * 10) %>% 
            drop_na(Årtionde), col = "red") +
  facet_wrap(~ Årtionde, ncol = 7) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid = element_blank())
mg72

# Art by artist, Malmö stads konstkarta, mg73 ----
mg73 <- dat_art %>% 
  count(konstnar, sort = T) %>% 
  filter(!is.na(konstnar), konstnar != "Okänd", n > 1) %>% 
  arrange(n, desc(konstnar)) %>% 
  mutate(konstnar = ordered(konstnar, konstnar)) %>% 
  ggplot(aes(n, konstnar)) + 
  geom_col(fill = "lightblue", color = "black", width = 0.5) +
  scale_x_continuous(breaks = 0:4, limits = c(0,4))
mg73

# Art works by gender (based on name), mg74 ----
dat_temp <- dat_art %>% 
  separate(konstnar, "Förnamn", " ", remove = F) %>% 
  mutate(Förnamn = tolower(Förnamn)) %>% 
  left_join(dat_namn %>% 
              mutate(Förnamn = tolower(Namn), Total = K + M, Andel_kvinnor = K / Total) %>% 
              select(Förnamn, Total, Andel_kvinnor), 
            by = "Förnamn") %>% 
  mutate(Kön = ifelse(Andel_kvinnor > 0.5, "K", "M"))

mg74 <- dat_temp %>% 
  ggplot(aes(0, fill = Kön)) +
  geom_bar(position = position_fill(), color = "black") +
  scale_x_continuous(limits = c(-2,1)) +
  coord_polar(theta = "y")
mg74

# Art by year, Wiki data, mg75 ----
mg75 <- dat_wiki_art %>% 
  mutate(Årtal = as.numeric(Årtal)) %>% 
  group_by(Årtal) %>% 
  mutate(nr = 1:n()) %>% 
  filter(Årtal >1800) %>% 
  ggplot(aes(Årtal, nr)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(1800, 2100, 10))
mg75

# Art on map by decade, Wiki data, mg76 ----
dat_temp <- dat_wiki_art_geo %>% 
  mutate(Årtionde = floor(as.numeric(År) / 10) * 10) %>% 
  filter(Årtionde > 1920) %>% 
  drop_na(Årtionde)

mg76 <- ggplot() +
  geom_sf(aes(fill = stadsdel), alpha = 0.2, data = dat_adm_delomr) +
  geom_sf(data = dat_temp %>% select(-Årtionde), alpha = 0.04) +
  geom_sf(data = dat_temp, col = "red") +
  facet_wrap(~ Årtionde, ncol = 3) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid = element_blank())
mg76

# Art by gender (based on name), Wiki data, mg77 ----
dat_temp <- dat_wiki_art %>% 
  mutate(Konstnärer = map(`Konstnär(er)`, split_at_lower_case_upper_case)) %>% 
  unnest(`Konstnärer`) %>% 
  separate(`Konstnärer`, "Förnamn", " ", remove = F) %>% 
  mutate(Förnamn = tolower(Förnamn)) %>% 
  left_join(dat_namn %>% 
              mutate(Förnamn = tolower(Namn), Total = K + M, Andel_kvinnor = K / Total) %>% 
              select(Förnamn, Total, Andel_kvinnor), 
            by = "Förnamn") %>% 
  mutate(Kön = ifelse(Andel_kvinnor > 0.5, "K", "M"))

mg77 <- dat_temp %>% 
  ggplot(aes(0, fill = Kön)) +
  geom_bar(position = position_fill(), color = "black") +
  scale_x_continuous(limits = c(-2,1)) +
  coord_polar(theta = "y")
mg77

# Art by gender (based on name) split by decade, Wiki data, mg78 ----
dat_temp <- dat_wiki_art %>% 
  mutate(Konstnärer = map(`Konstnär(er)`, split_at_lower_case_upper_case)) %>% 
  unnest(`Konstnärer`) %>% 
  separate(`Konstnärer`, "Förnamn", " ", remove = F) %>% 
  mutate(Förnamn = tolower(Förnamn)) %>% 
  left_join(dat_namn %>% 
              mutate(Förnamn = tolower(Namn), Total = K + M, Andel_kvinnor = K / Total) %>% 
              select(Förnamn, Total, Andel_kvinnor), 
            by = "Förnamn") %>% 
  mutate(Kön = ifelse(Andel_kvinnor > 0.5, "K", "M")) %>% 
  mutate(Årtionde = floor(as.numeric(Årtal) / 10) * 10) %>% 
  drop_na(Kön, Årtionde) %>% 
  filter(Årtionde >= 1920)

mg78 <- dat_temp %>% 
  ggplot(aes(0, fill = Kön)) +
  geom_bar(position = position_fill(), color = "black") +
  geom_text(aes(x, y, label = n), . %>% count(Årtionde, Kön) %>% 
              mutate(x = 1, y = ifelse(Kön == "K", 0.9, 0.1)),
            inherit.aes = F) +
  # scale_x_continuous(limits = c(-2,1)) +
  facet_wrap(~ Årtionde, ncol = 5) +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(), panel.grid = element_blank())
mg78

# Art by artist, Wiki data, mg79 ----
mg79 <- dat_wiki_art %>% 
  mutate(Konstnärer = map(`Konstnär(er)`, split_at_lower_case_upper_case)) %>% 
  unnest(`Konstnärer`) %>% 
  count(Konstnärer, sort = T) %>% 
  filter(Konstnärer != "Okänd") %>% 
  filter(n > 3) %>% 
  arrange(n, desc(Konstnärer)) %>% 
  mutate(Konstnärer = ordered(Konstnärer, Konstnärer)) %>% 
  ggplot(aes(n, Konstnärer)) +
  geom_col(width = 0.5)
mg79

# Art on map by gender, Wiki data, mg80 ----
dat_temp <- dat_wiki_art_geo %>% 
  separate(`Konstnär`, "Förnamn", " ", remove = F) %>% 
  mutate(Förnamn = tolower(Förnamn)) %>% 
  left_join(dat_namn %>% 
              mutate(Förnamn = tolower(Namn), Total = K + M, Andel_kvinnor = K / Total) %>% 
              select(Förnamn, Total, Andel_kvinnor), 
            by = "Förnamn") %>% 
  mutate(Kön = ifelse(Andel_kvinnor > 0.5, "K", "M")) %>% 
  drop_na(Kön)

mg80 <- ggplot() +
  geom_sf(aes(fill = stadsdel), alpha = 0.2, data = dat_adm_delomr) +
  geom_sf(aes(color = Kön), data = dat_temp, size = 2) +
  facet_wrap(~ Kön, ncol = 2) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid = element_blank()) +
  guides(color = "none")
mg80
