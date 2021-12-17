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

# Tree map reproduction, mg55 ----
mg55 <- dat_adm_delomr %>% 
  st_transform(3008) %>% 
  ggplot() + 
  geom_sf() +
  geom_point(aes(X, Y), size = 0.1, data = dat_trees %>% filter(kategori == "Lövträd"), color = "black") +
  geom_point(aes(X, Y), size = 0.1, data = dat_trees %>% filter(kategori == "Barrträd"), color = "red")
mg55

# Tree map split by type and origin, mg56 ----
mg56 <- dat_adm_delomr %>% 
  st_transform(3008) %>% 
  ggplot() + 
  geom_sf() +
  geom_point(aes(X, Y), size = 0.1, data = dat_trees %>% drop_na(ursprung)) +
  facet_grid(ursprung ~ kategori)
mg56

# Tree map by decade, mg57 ----
mg57 <- dat_adm_delomr %>% 
  st_transform(3008) %>% 
  ggplot() + 
  geom_point(aes(X, Y), size = 0.1, data = dat_trees %>% filter(Årtionde != 0)) +
  geom_sf(fill = NA) +
  facet_wrap(~ Årtionde)
mg57

# Treemap, density, mg58 ----
mg58 <- ggplot(dat_trees %>% filter(Årtionde != 0)) + 
  geom_density_2d_filled(aes(X, Y)) +
  geom_sf(data = dat_adm_mpol %>% st_transform(3008), fill = NA)
mg58

# Number of trees per decade, fill by categori, mg59 ----
g1 <- dat_trees %>% 
  filter(Årtionde > 0) %>% 
  ggplot(aes(Årtionde, fill = kategori)) + 
  geom_bar(width = 10, col = "black")
g2 <- dat_trees %>% 
  filter(Årtionde > 0) %>% 
  ggplot(aes(Årtionde, fill = kategori)) + 
  geom_bar(position = position_fill(), col = "black", width = 10)
mg59 <- g1 + g2
mg59

# Number of trees per decade, fill by categori and origin, mg60 ----
g1 <- dat_trees %>% 
  filter(Årtionde > 0) %>% 
  drop_na(ursprung) %>% 
  ggplot(aes(Årtionde, fill = paste(kategori, ursprung))) + 
  geom_bar(width = 10, col = "black")
g2 <- dat_trees %>% 
  filter(Årtionde > 0) %>% 
  drop_na(ursprung) %>%  
  ggplot(aes(Årtionde, fill = paste(kategori, ursprung))) + 
  geom_bar(position = position_fill(), col = "black", width = 10)
mg60 <- g1 + g2
mg60

# Number of trees per year after 2000, mg61 ----
mg61 <- dat_trees %>% 
  filter(Årtionde > 1999, planteringsar != 2021) %>% 
  drop_na(ursprung) %>% 
  ggplot(aes(planteringsar, fill = paste(ursprung, kategori))) + 
  geom_bar(col = "black", width = 1)
mg61

# Tree species top 20, mg62 ----
mg61 <- dat_trees %>% 
  count(svenskt_namn, kategori, ursprung, sort = T) %>% 
  slice(1:20) %>% 
  mutate(svenskt_namn = fct_reorder(svenskt_namn, n)) %>% 
  ggplot(aes(n, svenskt_namn, fill = paste(kategori, ursprung))) +
  geom_col(col = "black")
mg61

# Tree species top 10 map, mg63 ----
mg62 <- dat_trees %>%
  filter(svenskt_namn %in% (dat_trees %>% 
           count(svenskt_namn, sort = T) %>% slice(1:10) %>% pull(svenskt_namn))) %>% 
  ggplot(aes(X, Y, col = svenskt_namn)) +
  geom_point(size = 0.1) +
  geom_sf(data = dat_adm_mpol %>% st_transform(3008), fill = NA, inherit.aes = F) +
  guides(colour = guide_legend(override.aes = list(size=3)))
mg62

# Bar chart, Kungs- och slottsparken, mg64 ----
dat_temp <- dat_trees_geo %>% 
  st_intersection(dat_adm_mpol %>% 
                    st_transform(3008) %>% 
                    filter(name == "Malmö Hus") %>% 
                    select(name)) %>%
  group_by(svenskt_namn) %>% 
  mutate(n = n(),
         Art = ifelse(n > 50, svenskt_namn, "Övriga"),
         Familj = map_chr(vetenskapligt_namn, ~ strsplit(.x, " ")[[1]][1]))

dat_temp %>% 
  as_tibble() %>% 
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
            size = 4, hjust = 0, nudge_x = 20) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

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

ggplot() +
  geom_sf(data = dat_temp %>% filter(Art == "Övriga"), alpha = 0.1) +
  geom_sf(aes(color = Art), data = dat_temp %>% filter(Art != "Övriga"))

# Bar chart, Pildammsparken, mg66 ----
dat_temp <- dat_trees_geo %>% 
  st_intersection(dat_adm_mpol %>% 
                    st_transform(3008) %>% 
                    filter(name == "Pildammsparken") %>% 
                    select(name)) %>%
  group_by(svenskt_namn) %>% 
  mutate(n = n(),
         Art = ifelse(n > 50, svenskt_namn, "Övriga"),
         Familj = map_chr(vetenskapligt_namn, ~ strsplit(.x, " ")[[1]][1]))

mg66 <- dat_temp %>% 
  as_tibble() %>% 
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
            size = 3, hjust = 0, nudge_x = 50) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
mg66

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
  geom_sf(aes(color = Art), data = dat_temp %>% filter(Art != "Övriga"))
mg67

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
  geom_point(col = "red", size = 0.1) +
  facet_wrap(~ planteringsar, ncol = 3) +
  coord_equal()
mg68
