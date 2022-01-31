# ---
# Title: Malmö municipality tree map - plotting 
# Purpose: 
# Start date: 211203
# Comments:
# ---

# Packages ----
library(tidyverse)
library(patchwork)

# Read setup file ----
source("R/Natur/Trees-setup.R", encoding = "UTF-8")
source("R/Geografi/Parts-of-the-City-setup.R", encoding = "UTF-8")
source("r/Teman/Themes.R", encoding = "UTF-8")

# Tree map reproduction, mg55 ----
mg55 <- dat_adm_delomr %>% 
  st_transform(3008) %>% 
  ggplot() + 
  # geom_point(aes(X, Y), size = 0.1, shape = ".", data = dat_trees %>% filter(kategori == "Lövträd"), color = "black") +
  # geom_point(aes(X, Y), size = 0.1, shape = ".", data = dat_trees %>% filter(kategori == "Barrträd"), color = "red") +
  geom_point(aes(X, Y), size = 0.1, data = dat_trees, color = "#ff4444") +
  geom_sf(color = "white", fill = "NA") +
  labs(title = "Träd från Malmö stads trädkarta",
       caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 55
       23 januari 2022") +
  theme_trees() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.background = element_blank(), plot.caption = element_text(size = 10))
mg55

ggsave("Output/Natur/Tree-map.png", mg55, height = 10, width = 10)

# Tree map split by type and origin, mg56 ----
mg56 <- dat_adm_delomr %>% 
  st_transform(3008) %>% 
  ggplot() + 
  geom_point(aes(X, Y), size = 0.1, data = dat_trees %>% drop_na(ursprung), color = "#ff4444") +
  geom_sf(color = "white", fill = "NA") +
  facet_grid(ursprung ~ kategori) +
  labs(title = "Träd från Malmö stads trädkarta, uppdelade efter ursprung och barr-/lövträd",
       caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 56
       23 januari 2022") +
  theme_trees() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.background = element_blank(), plot.caption = element_text(size = 10))

mg56

ggsave("Output/Natur/Tree-map-by-type-and-origin.png", mg56, height = 12, width = 12)

# Tree map by decade, mg57 ----
mg57 <- dat_adm_delomr %>% 
  st_transform(3008) %>% 
  ggplot() + 
  geom_point(aes(X, Y), size = 0.1, data = dat_trees %>% filter(Årtionde != 0), color = "#ff4444") +
  geom_sf(color = "white", fill = "NA") +
  facet_wrap(~ Årtionde, ncol = 6) +
  labs(title = "Träd från Malmö stads trädkarta, uppdelade efter planteringsårtionde",
       caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 57
       23 januari 2022") +
  theme_trees() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.background = element_blank(), plot.caption = element_text(size = 10))
mg57

ggsave("Output/Natur/Tree-map-by-decade.png", mg57, height = 10, width = 16)

# Treemap, density, mg58 ----
mg58 <- ggplot(dat_trees %>% filter(Årtionde != 0)) + 
  geom_density_2d_filled(aes(X, Y)) +
  geom_sf(data = dat_adm_mpol %>% st_transform(3008), fill = NA, color = "white") +
  scale_fill_manual(values = c(colorRampPalette(c("#004d00", "#ff4444"))(12))) +
  labs(title = "Malmö stads trädkarta, trädtäthet",
       caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 58
       23 januari 2022") +
  theme_trees() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.background = element_blank(), plot.caption = element_text(size = 10), legend.position = "none")

mg58

ggsave("Output/Natur/Tree-map-density.png", mg58, height = 14, width = 14)

# Number of trees per decade, fill by categori, mg59 ----
g1 <- dat_trees %>% 
  filter(Årtionde > 0) %>% 
  ggplot(aes(Årtionde, fill = kategori)) + 
  geom_bar(width = 10, col = "black") +
  labs(x = "Årtionde", y = "Antal") +
  scale_fill_manual(values = c("#ccffcc", "#22aa22"))
g2 <- dat_trees %>% 
  filter(Årtionde > 0) %>% 
  ggplot(aes(Årtionde, fill = kategori)) + 
  geom_bar(position = position_fill(), col = "black", width = 10) +
  labs(x = "Årtionde", y = "Andel") +
  scale_fill_manual(values = c("#ccffcc", "#22aa22"))
mg59 <- g1 + g2 + 
  plot_layout(guides = "collect") +
  plot_annotation(title = "Malmös trädkarta. Antal träd per årtionde",
                  caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 59
       23 januari 2022") &
  theme_trees()
mg59

ggsave("Output/Natur/Tree-map-trees-per-decade.png", mg59, height = 6, width = 10)

# Number of trees per decade, fill by categori and origin, mg60 ----
g1 <- dat_trees %>% 
  filter(Årtionde > 0) %>% 
  drop_na(ursprung) %>% 
  ggplot(aes(Årtionde, fill = paste(kategori, ursprung))) + 
  geom_bar(width = 10, col = "black") +
  ylab("Antal") +
  scale_fill_manual(values = c("#d2a679", "#ecd9c6", "#ccffcc", "#22aa22"))
g2 <- dat_trees %>% 
  filter(Årtionde > 0) %>% 
  drop_na(ursprung) %>%  
  ggplot(aes(Årtionde, fill = paste(kategori, ursprung))) + 
  geom_bar(position = position_fill(), col = "black", width = 10) +
  ylab("Andel") +
  scale_fill_manual(values = c("#d2a679", "#ecd9c6", "#ccffcc", "#22aa22"))
mg60 <- g1 + g2 + 
  plot_layout(guides = "collect") +
  plot_annotation(title = "Malmös trädkarta. Antal träd per årtionde, uppdelat efter sort och ursprung",
                  caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 56
       23 januari 2022") &
  theme_trees()
mg60

ggsave("Output/Natur/Tree-map-trees-per-decade-by-type-and-origin.png", mg60, height = 6, width = 10)

# Number of trees per year after 2000, mg61 ----
mg61 <- dat_trees %>% 
  filter(Årtionde > 1999, planteringsar != 2021) %>% 
  drop_na(ursprung) %>% 
  ggplot(aes(planteringsar, fill = paste(ursprung, kategori))) + 
  geom_bar(col = "black", width = 1) +
  scale_fill_manual(values = c("#d2a679", "#ecd9c6", "#ccffcc", "#22aa22")) +
  labs(title = "Malmös trädkarta. Antal träd per år, uppdelat efter sort och ursprung",
       caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 61
       25 januari 2022",
       y = "Antal", x = "År") +
  theme_trees() +
  theme(text = element_text(size = 12))
mg61

ggsave("Output/Natur/Tree-map-trees-per-year-by-type-and-origin.png", mg61, height = 6, width = 8)

# Tree species top 20, mg62 ----
mg62 <- dat_trees %>% 
  count(svenskt_namn, kategori, ursprung, sort = T) %>% 
  slice(1:20) %>% 
  mutate(svenskt_namn = fct_reorder(svenskt_namn, n)) %>% 
  ggplot(aes(n, svenskt_namn, fill = paste(kategori, ursprung))) +
  geom_col(col = "black", width = 0.8) +
  scale_fill_manual(values = c("#d2a679", "#ecd9c6", "#22aa22")) +
  labs(title = "Malmö stads vanligaste träd",
       x = "Antal", y = "Art",
       caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 62
       23 januari 2022") +
  theme_trees() +
  theme(panel.grid.major.x = element_line(color = "white"), axis.ticks = element_blank(), text = element_text(size = 10))
mg62

ggsave("Output/Natur/Tree-map-most-common-trees.png", mg62, height = 6, width = 8)

# Tree species top 10 map, mg63 ----
mg63 <- dat_trees %>%
  filter(svenskt_namn %in% (dat_trees %>% 
                              count(svenskt_namn, sort = T) %>% slice(1:10) %>% pull(svenskt_namn))) %>% 
  ggplot(aes(X, Y, col = svenskt_namn)) +
  geom_point(size = 0.1) +
  geom_sf(data = dat_adm_mpol %>% st_transform(3008), fill = NA, inherit.aes = F) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  labs(title = "Malmö Stads trädkarta för tio vanliga arter",
       caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 63
       29 januari 2022") +
  theme_trees() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
mg63

ggsave("Output/Natur/Tree-map-most-common-trees-on-map.png", mg63, height = 10, width = 10)

# Bar chart, Kungs- och slottsparken, mg64 ----
dat_temp <- dat_trees_geo %>% 
  st_intersection(dat_adm_mpol %>% 
                    st_transform(3008) %>% 
                    filter(name == "Malmö Hus") %>% 
                    select(name)) %>%
  mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2]) %>% 
  group_by(svenskt_namn) %>% 
  mutate(n = n(),
         Art = ifelse(n > 50, svenskt_namn, "Övriga"),
         Familj = map_chr(vetenskapligt_namn, ~ strsplit(.x, " ")[[1]][1]))

mg64 <- dat_temp %>% 
  as_tibble() %>% 
  group_by(X, Y) %>% slice(1) %>% ungroup() %>% 
  count(svenskt_namn, vetenskapligt_namn, kategori, ursprung, sort = T) %>% 
  mutate(Rang = rank(-n),
         Art = ifelse(Rang > 20, "Övriga", svenskt_namn)) %>% 
  count(Art, kategori, ursprung, wt = n, sort = T) %>% 
  mutate(Sort_n = ifelse(Art == "Övriga", -1, n),
         Art = fct_reorder(Art, Sort_n)) %>% 
  drop_na(ursprung) %>% 
  ggplot(aes(n, Art, fill = paste(kategori, ursprung))) + 
  geom_col(color = "black") +
  geom_text(aes(label = n), data = . %>% filter(Art != "Övriga"), 
            size = 4, hjust = 0, nudge_x = 20, color = "white", family = "Garamond") +
  scale_fill_manual(values = c("#d2a679", "#ecd9c6", "#ccffcc", "#22aa22")) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  labs(title = "Malmö Stads trädkarta. Antal träd i Kungs- och Slottsparken, per art",
       x = "Antal",
       caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 64
       23 januari 2022") +
  theme_trees() +
  theme(text = element_text(size = 10), plot.title.position = "plot", axis.ticks = element_blank())
mg64

ggsave("Output/Natur/Tree-map-most-common-trees-Slottsparken.png", mg64, height = 10, width = 10)

# Tree map, Kungs- och slottsparken, mg65 ----
dat_temp <- dat_trees_geo %>% 
  st_intersection(dat_adm_mpol %>% 
                    st_transform(3008) %>% 
                    filter(name == "Malmö Hus") %>% 
                    select(name)) %>%
  group_by(svenskt_namn) %>% 
  mutate(n = n(),
         Art = ifelse(n > 229, svenskt_namn, "Övriga"),
         Familj = map_chr(vetenskapligt_namn, ~ strsplit(.x, " ")[[1]][1]))

dat_water1 <- opq(dat_adm_mpol %>% filter(name == "Malmö Hus") %>% st_bbox()) %>%
  add_osm_feature(key="water") %>%
  osmdata_sf()

dat_water2 <- opq(dat_adm_mpol %>% filter(name == "Malmö Hus") %>% st_bbox()) %>%
  add_osm_feature(key="water", value = "canal") %>%
  osmdata_sf()


mg65 <- ggplot() +
  geom_sf(data = dat_temp %>% filter(Art == "Övriga"), alpha = 0.1) +
  geom_sf(aes(color = Art), data = dat_temp %>% filter(Art != "Övriga")) +
  geom_sf(data = dat_water1$osm_polygons, fill = NA, color = "white") + 
  geom_sf(data = dat_water2$osm_lines, color = "white") +
  coord_sf(xlim = c(117356, 118191), ylim = c(6164034, 6164819)) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Malmö Stads trädkarta. Vanliga träd i Kungs- och Slottsparken",
       caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 65
       29 januari 2022") +
  theme_trees() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
mg65

ggsave("Output/Natur/Tree-map-most-common-trees-on-map-Slottsparken.png", mg65, height = 10, width = 10)

# Bar chart, Pildammsparken, mg66 ----
dat_temp <- dat_trees_geo %>% 
  st_intersection(dat_adm_mpol %>% 
                    st_transform(3008) %>% 
                    filter(name == "Pildammsparken") %>% 
                    select(name)) %>%
  mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2]) %>% 
  group_by(svenskt_namn) %>% 
  mutate(n = n(),
         Art = ifelse(n > 50, svenskt_namn, "Övriga"),
         Familj = map_chr(vetenskapligt_namn, ~ strsplit(.x, " ")[[1]][1]))

mg66 <- dat_temp %>% 
  as_tibble() %>% 
  group_by(X, Y) %>% slice(1) %>% ungroup() %>% 
  count(svenskt_namn, vetenskapligt_namn, kategori, ursprung, sort = T) %>% 
  mutate(Rang = rank(-n),
         Art = ifelse(Rang > 20, "Övriga", svenskt_namn)) %>% 
  count(Art, kategori, ursprung, wt = n, sort = T) %>% 
  mutate(Sort_n = ifelse(Art == "Övriga", -1, n),
         Art = fct_reorder(Art, Sort_n)) %>% 
  drop_na(ursprung) %>% 
  ggplot(aes(n, Art, fill = paste(kategori, ursprung))) + 
  geom_col(color = "black") +
  geom_text(aes(label = n), data = . %>% filter(Art != "Övriga"), 
            size = 3, hjust = 0, nudge_x = 50, color = "white", family = "Garamond")  +
  scale_fill_manual(values = c("#d2a679", "#ecd9c6", "#ccffcc", "#22aa22")) +
  labs(title = "Malmö Stads trädkarta. Antal träd i Pildammsparken per art",
       x = "Antal",
       caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 66
       23 januari 2022") +
  theme_trees() +
  theme(text = element_text(size = 10), plot.title.position = "plot", axis.ticks = element_blank())
mg66

ggsave("Output/Natur/Tree-map-most-common-trees-Pildammssparken.png", mg66, height = 10, width = 10)

# Tree map, Pildammsparken, mg67 ----
dat_temp <- dat_trees_geo %>% 
  st_intersection(dat_adm_mpol %>% 
                    st_transform(3008) %>% 
                    filter(name == "Pildammsparken") %>% 
                    select(name)) %>%
  group_by(svenskt_namn) %>% 
  mutate(n = n(),
         Art = ifelse(n > 99, svenskt_namn, "Övriga"),
         Familj = map_chr(vetenskapligt_namn, ~ strsplit(.x, " ")[[1]][1]))

mg67 <- ggplot() +
  geom_sf(data = dat_temp %>% filter(Art == "Övriga"), alpha = 0.1) +
  geom_sf(aes(color = Art), data = dat_temp %>% filter(Art != "Övriga")) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Malmö Stads trädkarta. Vanliga träd i Pildammsparken",
       caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 67
       29 januari 2022") +
  theme_trees() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        text = element_text(size = 12))
mg67

ggsave("Output/Natur/Tree-map-most-common-trees-on-map-Pildammsparken.png", mg67, height = 10, width = 10)

# Tree map, Pildammsparken, mg68 ----
dat_temp <- dat_trees_geo %>% 
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>% 
  st_intersection(dat_adm_mpol %>% 
                    st_transform(3008) %>% 
                    filter(name == "Pildammsparken") %>% 
                    select(name)) %>% 
  group_by(planteringsar) %>% 
  mutate(n = n()) %>% 
  filter(n > 50) %>% 
  as_tibble()

mg68 <- ggplot(dat_temp, aes(X, Y)) +
  geom_point(data = dat_temp %>% select(-planteringsar), alpha = 0.1, size = 0.1) +
  geom_point(col = "#ff4444", size = 0.1) +
  facet_wrap(~ planteringsar, ncol = 5) +
  coord_equal() +
  labs(title = "Träd i Pildammsparken efter planteringsår",
       caption = "Källa: Malmö Stads trädkarta, 
       https://malmo.se/Stadsutveckling/Tema/Bebyggelse-och-utemiljoer/Trad-i-Malmo.html
       
       Malmögram 68
       29 januari 2022") +
  theme_trees() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        text = element_text(size = 12), panel.background = element_rect(color = NA), 
        strip.background = element_blank())
mg68

ggsave("Output/Natur/Tree-map-Pildammsparken-by-year.png", mg68, height = 12, width = 15)