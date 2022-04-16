# ---
# Title: Riksdag elections - plots
# Purpose: 
# Start date: 
# Comments:
# ---

# Packages ----
library(tidyverse)
library(gganimate)

# Import data
source("r/Politik/Riksdagsval-setup.R", encoding = "UTF-8")
source("r/Teman/Themes.R", encoding = "UTF-8")

# Malmö, piecharts over time, mg41 ----
mg41 <- dat %>% 
  filter(Region == "Malmö") %>% 
  ggplot(aes(0, Antal, fill = Parti)) +
  geom_bar(stat = "identity", position = position_fill(), col = "black", alpha = 1.0) +
  facet_wrap(~ År, ncol = 7) +
  xlim(-2,1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = dat_parties$Färg) +
  labs(title = "\nMalmö stad. Utfall i riksdagsval 1973 - 2018",
       caption = "Källa: SCBs valstatistik, http://www.scb.se/me0104 
       Malmögram 41
       23 januari 2022") +
  theme_mg5() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
mg41

ggsave("Output/Politik/041-Riksdag-circles-by-year.png", mg41, height = 4, width = 10)

# Malmö, time series, mg42 ----
dat_temp <- dat %>% 
  filter(Region == "Malmö") %>% 
  group_by(År) %>% 
  mutate(Totalt_antal = sum(Antal, na.rm = T),
         p = Antal / Totalt_antal)
dat_temp2 <- dat_temp %>% rename(Parti1 = Parti)

mg42 <- dat_temp %>% 
  ggplot(aes(År, p, col = Parti)) +
  geom_line(aes(col = NA, group = Parti1), data = dat_temp2, col = "#ffeeee", alpha = 0.5) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = seq(1980, 2010, 10)) +
  facet_wrap(~ Parti) +
  scale_color_manual(values = dat_parties$Färg) +
  labs(title = "Malmö stad. Utfall i riksdagsval 1973 - 2018",
       x = "År", y = "Andel röster",
       caption = "Källa: SCBs valstatistik, http://www.scb.se/me0104 
       Malmögram 42
       11 februari 2022") +
  theme_bw() +
  theme_mg5() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#550000"))
mg42

ggsave("Output/Politik/042-Riksdag-by-year-per-party.png", mg42, height = 8, width = 10)

# Path-graph, Malmö and Sweden, mg43 ----
dat_temp <- dat %>% 
  filter(Region == "Malmö") %>% 
  bind_rows(dat_agg) %>% 
  group_by(År, Region) %>% 
  mutate(Totalt_antal = sum(Antal, na.rm = T),
         p = Antal / Totalt_antal) %>% 
  ungroup() %>% 
  select(Region, Parti, År, p) %>% 
  pivot_wider(values_from = p, names_from = Region) %>% 
  mutate(Sverige = ifelse(is.na(Malmö), NA, Sverige))

mg43 <- dat_temp %>% 
  ggplot(aes(Sverige, Malmö, label = År, col = År)) + 
  geom_abline(intercept = 0, slope = 1, col = "white") +
  geom_path(size = 1) +
  geom_point(size = 3) +
  scale_color_gradient(low = "white", high = "black") +
  facet_wrap(~ Parti) +
  labs(title = "Sverige mot Malmö. Utfall i riksdagsval 1973 - 2018", 
       x = "Andel röster i Sverige", y = "Andel röster i Malmö",
       caption = "Källa: SCBs valstatistik, http://www.scb.se/me0104 
       Malmögram 43
       11 februari 2022") +
  theme_mg5() +
  theme(panel.grid = element_line(color = "#550000"),
        panel.grid.minor = element_blank())
mg43

ggsave("Output/Politik/043-Riksdag-paths-per-party.png", mg43, height = 8, width = 10)

# Point-line-comparison plot 2018, mg44 ----
dat_temp <- dat %>% 
  filter(Region == "Malmö") %>% 
  bind_rows(dat_agg) %>% 
  group_by(År, Region) %>% 
  mutate(Totalt_antal = sum(Antal, na.rm = T),
         p = Antal / Totalt_antal) %>% 
  ungroup() %>% 
  filter(År == 2018) %>% 
  group_by(Parti) %>% 
  mutate(Differens = sum(p) - 2 * sum(p * (Region == "Sverige")),
         p_Malmö = sum(p * (Region == "Malmö")))

mg44 <- ggplot(dat_temp, aes(p, fct_reorder(Parti, p_Malmö, mean))) +
  geom_line(aes(group = Parti, col = ifelse(Region == "Malmö", "Sverige", "Malmö")), size = 1, show.legend = F) +
  geom_point(aes(fill = Region), size = 4, shape = 21) +
  scale_color_manual(values = c("white", "black")) +
  scale_fill_manual(values = c("white", "black")) +
  labs(title = "Malmö och Sverige. Utfall i riksdagsval 2018", 
       subtitle = "Malmöbor röstar i mycket mindre utsträckning än övriga landet.", 
       x = "Andel röster", y = "Parti",
       caption = "Källa: SCBs valstatistik, http://www.scb.se/me0104 
       Malmögram 44
       11 februari 2022") +
  theme_mg5() +
  theme(panel.grid = element_line(color = "#550000"),
        panel.grid.major.y = element_blank())
mg44

ggsave("Output/Politik/044-Riksdag-Sweden-to-Malmoe-comparison-2018.png", mg44, height = 6, width = 8)

# Point-line-comparison centered on proportion for Sweden, mg45 ----
dat_temp <- dat %>% 
  filter(Region == "Malmö") %>% 
  bind_rows(dat_agg) %>% 
  group_by(År, Region) %>% 
  mutate(Totalt_antal = sum(Antal, na.rm = T),
         p = Antal / Totalt_antal) %>% 
  ungroup() %>% 
  filter(År == 2018) %>% 
  group_by(Parti) %>% 
  mutate(Differens = sum(p) - 2 * sum(p * (Region == "Sverige")),
         Relativt_störst = ifelse(Differens < 0, "Sverige", "Malmö")) %>% 
  ungroup() %>% 
  mutate(Parti = fct_reorder(Parti, Differens, mean))

mg45 <- ggplot(dat_temp, aes(Differens, Parti)) +
  geom_point(size = 3, col = "white") +
  geom_segment(aes(yend = Parti, xend = 0), size = 1, col = "white") +
  scale_x_continuous(breaks = seq(-0.1, 0.1, 0.02), limits = c(-0.06, 0.06), labels = seq(-10, 10, 2)) +
  labs(title = "Malmö minus Sverige. Utfall i riksdagsval 2018", 
       x = "Procentenheter skillnad, Malmö - Sverige", y = "Parti",
       caption = "Källa: SCBs valstatistik, http://www.scb.se/me0104 
       Malmögram 45
       28 februari 2022") +
  theme_mg5() +
  theme(panel.grid = element_line(color = "#550000"),
        panel.grid.major.y = element_blank(), legend.position = "none")
mg45

ggsave("Output/Politik/045-Riksdag-Sweden-to-Malmoe-comparison-2018-centered.png", mg45, height = 6, width = 8)

# Point-line-comparison centered on proportion for Sweden, relative comparison (log-scale) mg46 ----
dat_temp <- dat %>% 
  filter(Region == "Malmö") %>% 
  bind_rows(dat_agg) %>% 
  group_by(År, Region) %>% 
  mutate(Totalt_antal = sum(Antal, na.rm = T),
         p = Antal / Totalt_antal) %>% 
  ungroup() %>% 
  filter(År == 2018) %>% 
  select(Region, Parti, p) %>% 
  pivot_wider(values_from = p, names_from = Region) %>% 
  mutate(Kvot = Malmö / Sverige,
         log_kvot = log(Kvot))

mg46 <- ggplot(dat_temp, aes(Kvot, fct_reorder(Parti, Kvot), col = Kvot > 1)) +
  geom_point(size = 4) +
  geom_segment(aes(yend = Parti, xend = 1), size = 2) +
  scale_x_continuous(breaks = seq(0.5, 1.5, 0.25), labels = c("-50 %", "-25 %", "+/- 0 %", "+25 %", "+50%"),
                     limits = c(0.5, 1.5)) +
  theme_mg5() +
  theme(panel.grid = element_line(color = "grey20"),
        legend.position = "none")
mg46

# Malmö, stacked bars over time, mg47 ----
valår <- c(unique(dat$År), 2022)
foo <- function(x) min(valår[valår > x])

mg47 <- dat %>% 
  filter(Region == "Malmö") %>% 
  mutate(Slutår = map_dbl(År, foo),
         Antal = ifelse(is.na(Antal), 0, Antal)) %>% 
  group_by(År) %>% 
  mutate(p = Antal / sum(Antal),
         p_cs = cumsum(p),
         p_cs_lower = lag(p_cs, default = 0)) %>% 
  ggplot(aes(xmin = År, xmax = Slutår, ymin = p_cs_lower, ymax = p_cs, fill = Parti)) +
  geom_rect(col = "black", alpha = 0.85) +
  geom_hline(yintercept = seq(0, 1, 0.25), col = "white", alpha = 0.5) +
  geom_vline(xintercept = c(1973, 2022), col = "white") +
  scale_fill_manual(values = dat_parties$Färg) +
  scale_x_continuous(name = "År", breaks = valår, expand = c(0,0)) +
  scale_y_continuous(name = "Andel", expand = c(0,0)) +
  labs(title = "Malmö i riksdagsvalen 1973 - 2018", 
       x = "Andel röster", y = "Parti",
       caption = "Källa: SCBs valstatistik, http://www.scb.se/me0104 
       Malmögram 47
       11 februari 2022") +
  theme(axis.ticks = element_blank()) +
  theme_mg5() +
  theme(panel.grid = element_blank(), axis.ticks = element_line(color = "white"))
mg47

ggsave("Output/Politik/047-Riksdag-Malmoe-bar-by-year.png", mg47, height = 6, width = 8)

# Malmö, stacked bars over time, excluding non-voters, mg47b ----
valår <- c(unique(dat$År), 2022)
foo <- function(x) min(valår[valår > x])

mg47b <- dat %>% 
  filter(Region == "Malmö", !(Parti %in% c("övriga partier", "ogiltiga valsedlar", "ej röstande"))) %>% 
  mutate(Slutår = map_dbl(År, foo),
         Antal = ifelse(is.na(Antal), 0, Antal)) %>% 
  group_by(År) %>% 
  mutate(p = Antal / sum(Antal),
         p_cs = cumsum(p),
         p_cs_lower = lag(p_cs, default = 0)) %>% 
  ggplot(aes(xmin = År, xmax = Slutår, ymin = p_cs_lower, ymax = p_cs, fill = Parti)) +
  geom_rect(col = "black", alpha = 0.85) +
  geom_hline(yintercept = seq(0, 1, 0.25), col = "white", alpha = 0.5) +
  geom_vline(xintercept = c(1973, 2022), col = "white") +
  scale_fill_manual(values = dat_parties$Färg) +
  scale_x_continuous(name = "År", breaks = valår, expand = c(0,0)) +
  scale_y_continuous(name = "Andel", expand = c(0,0)) +
  labs(title = "Malmö i riksdagsvalen 1973 - 2018",
       subtitle = "Enbart riksdagspartier (frånräknad icke-röstande, ogilitiga röster, övriga partier)",
       x = "Andel röster", y = "Parti",
       caption = "Källa: SCBs valstatistik, http://www.scb.se/me0104 
       Malmögram 47
       11 februari 2022") +
  theme(axis.ticks = element_blank()) +
  theme_mg5() +
  theme(panel.grid = element_blank(), axis.ticks = element_line(color = "white"))
mg47b

ggsave("Output/Politik/047b-Riksdag-Malmoe-bar-by-year-only-parties.png", mg47b, height = 6, width = 8)

# Malmö, piecharts over time, animated, mg48 ----
mg48 <- dat %>% 
  filter(Region == "Malmö") %>% 
  mutate(Antal = ifelse(is.na(Antal), 1, Antal)) %>% 
  ggplot(aes(0, Antal, fill = Parti)) +
  geom_bar(stat = "identity", position = position_fill(), col = "black") +
  scale_fill_manual(values = dat_parties$Färg) +
  xlim(-2,1) +
  coord_polar(theta = "y") +
  theme_mg5() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  ease_aes() +
  transition_states(År, transition_length = 2, state_length = 4) +
  labs(title = "Malmö i riksdagsvalet {closest_state}")

animate(mg48, nframes = 400, width = 500, height = 400, fps = 40)
anim_save("Output/Politik/048-Riksdag-Malmoe-pie-over-time.gif")

# Malmö and neighbours, mg49 ----
mg49 <- dat %>% 
  filter(Region %in% c("Malmö", "Vellinge", "Lomma", "Burlöv", "Staffanstorp", "Svedala", "Lund"),
         År == 2018) %>%
  group_by(År, Region) %>% 
  mutate(p = Antal / sum(Antal)) %>% 
  ungroup() %>% arrange(Region) %>% 
  mutate(Region = fct_reorder(Region, p * (Parti == "Socialdemokraterna"), sum, .desc = T)) %>% 
  ggplot(aes(Region, p, col = Parti, group = Parti)) +
  geom_point(size = 4) +
  geom_line(size = 1) +
  scale_color_manual(values = dat_parties$Färg) +
  scale_y_continuous(breaks = seq(0,0.4,0.1), labels = seq(0, 40, 10), limits = c(0, 0.4)) +
  labs(title = "Malmö och några grannar i riksdagsvalet 2018",
       x = "", y = "Andel röster (procent)",
       caption = "Källa: SCBs valstatistik, http://www.scb.se/me0104 
       Malmögram 49
       4 mars 2022") +
  theme_mg5() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.border = element_blank())
mg49

ggsave("Output/Politik/049-Riksdag-Malmoe-and-neighbours.png", mg49, height = 6, width = 8)

# Malmö against all other municipalities, mg50 ----
dat_temp <- dat %>% 
  filter(År == 2018, Region != "Bara") %>% 
  group_by(År, Region) %>% 
  mutate(p = Antal / sum(Antal)) %>% 
  ungroup() %>% group_by(Parti, År) %>% 
  mutate(Rank = rank(p),
         Text = paste0(ifelse((Rank - 1) > (290 - Rank), "Högre", "Lägre"), " andel i ", 
                       pmin(Rank - 1, 290 - Rank), " kommuner"))
mg50 <- dat_temp %>% 
  ggplot(aes(p, fct_rev(Parti))) +
  geom_boxplot(col = "white", fill = NA, width = 0.2) +
  geom_point(data = . %>% filter(Region == "Malmö"), fill = "red", shape = 21, size = 3) +
  geom_text(aes(x = 0.5, label = Text), data = . %>% filter(Region == "Malmö"), col = "white", size = 3) +
  labs(title = "Sveriges kommuner i riksdagsvalet 2018",
       subtitle = "Malmö som röd punkt. Text anger antalet kommuner med högre (eller lägre) andel för partiet.",
       x = "Andel röster", y = "Parti",
       caption = "Källa: SCBs valstatistik, http://www.scb.se/me0104 
       Malmögram 50
       28 februari 2022") +
  xlim(0, 0.6) +
  theme_mg5() +
  theme(panel.grid = element_line(color = "#880000"),
        panel.grid.major.y = element_blank())
mg50

ggsave("Output/Politik/050-Riksdag-Malmoe-relation-to-other-municipalities.png", mg50, height = 6, width = 8)

# Malmö multiple scatterplots, mg51 ----
dat_temp <- dat %>% 
  filter(Region == "Malmö") %>% 
  group_by(År, Region) %>% 
  mutate(p = Antal / sum(Antal, na.rm = T)) %>%
  select(Region, År, Parti, p) %>% 
  pivot_wider(names_from = Parti, values_from = p) %>% 
  ungroup()

dat_temp2 <- tibble(Parti = unique(names(dat_temp)[-(1:2)])) %>% 
  mutate(dat = map(Parti, ~ dat_temp %>% 
                     pivot_longer(-c(År, Region, all_of(.x)), names_to = "Parti2", values_to = "p2") %>% 
                     rename(p1 = .x))) %>% 
  unnest(dat) %>% 
  arrange(År) %>% 
  mutate(Parti = ordered(Parti, dat_parties$Parti),
         Parti2 = ordered(Parti2, dat_parties$Parti))

mg51 <- dat_temp2 %>% 
  ggplot(aes(p1, p2, col = År)) +
  geom_point() +
  geom_path() +
  facet_grid(Parti2 ~ Parti, scales = "free") +
  theme_mg5() +
  theme(panel.grid = element_line(color = "grey20"))
mg51

# GGally::ggpairs(dat_temp, columns = 3:13)

# Sweden 2018 multiple scatterplots (scatter matrix), mg52 ----
dat_temp <- dat %>% 
  filter(År == 2018) %>% 
  group_by(År, Region) %>% 
  mutate(p = Antal / sum(Antal, na.rm = T)) %>%
  select(Region, År, Parti, p) %>% 
  pivot_wider(names_from = Parti, values_from = p) %>% 
  ungroup()

dat_temp2 <- tibble(Parti = unique(names(dat_temp)[-(1:2)])) %>% 
  mutate(dat = map(Parti, ~ dat_temp %>% 
                     pivot_longer(-c(År, Region, .x), names_to = "Parti2", values_to = "p2") %>% 
                     rename(p1 = .x))) %>% 
  unnest(dat) %>% 
  arrange(År) %>% 
  mutate(Parti = ordered(Parti, dat_parties$Parti),
         Parti2 = ordered(Parti2, dat_parties$Parti))

mg52 <- dat_temp2 %>% 
  ggplot(aes(p1, p2)) +
  geom_point(alpha = 0.2, col = "white") +
  geom_smooth(method = lm, se = F, col = "#ff3333") +
  geom_point(data = . %>% filter(Region == "Malmö"), col = "red") +
  facet_grid(Parti2 ~ Parti, scales = "free") +
  theme_mg5() +
  theme(panel.grid = element_line(color = "grey20"),
        panel.grid.minor = element_blank())
mg52

# Sweden 2018, score plot from PCA, mg53
library(ggrepel)
dat_temp <- dat %>% 
  filter(År == 2018, Region != "Bara") %>% 
  group_by(År, Region) %>% 
  mutate(p = Antal / sum(Antal, na.rm = T)) %>%
  select(Region, År, Parti, p) %>% 
  pivot_wider(names_from = Parti, values_from = p) %>% 
  ungroup()

mod <- prcomp(dat_temp[, -(1:2)], scale. = T)
# summary(mod)

loading_mult <- 6
dat_loadings <- mod$rotation %>% as.data.frame() %>% as_tibble() %>% 
  mutate(Parti = rownames(mod$rotation)) %>% select(PC1, PC2, Parti) %>% 
  mutate(length = sqrt(PC1^2 + PC2^2),
         PC1 = PC1 / length, PC2 = PC2 / length,
         length2 = sqrt(PC1^2 + PC2^2),
         adj = ifelse(PC1 < 0, 1.1, -0.1),
         angle = 180 / pi * atan(PC2 / PC1)) %>% 
  mutate_at(vars(PC1, PC2), ~ loading_mult * .x)

mg53 <- dat_temp %>% 
  mutate(PC1 = mod$x[,1], PC2 = mod$x[,2]) %>% 
  ggplot(aes(PC1, PC2)) +
  geom_point(col = "white") +
  geom_point(data = . %>% filter(Region == "Malmö"), col = "red", size = 3) +
  geom_text_repel(aes(label = Region), data = . %>% filter(abs(PC1) > 2 | abs(PC2) > 2), 
                  size = 2, max.overlaps = 21, col = "white") +
  geom_text(aes(PC1, PC2, label = Parti, hjust = adj, angle = angle),
            data = dat_loadings, col = "red") +
  geom_point(aes(PC1, PC2), data = dat_loadings, col = "red") +
  geom_segment(aes(xend = PC1, yend = PC2, x = 0, y = 0), 
               data = dat_loadings, col = "red") +
  geom_polygon(aes(x, y), 
               data = tibble(t = seq(0, 2*pi, 0.05), x = loading_mult * sin(t), y = loading_mult * cos(t)), 
               col = "red", fill = NA, inherit.aes = F) +
  theme_mg5() +
  theme(panel.grid = element_line(color = "grey20")) +
  xlim(-8,8) + ylim(-8,8) +
  coord_equal()
mg53
