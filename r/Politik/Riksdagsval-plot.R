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
  geom_bar(stat = "identity", position = position_fill(), col = "black") +
  facet_wrap(~ År, ncol = 7) +
  xlim(-2,1) +
  coord_polar(theta = "y") +
  theme_mg5() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
mg41

# Malmö, time series, mg42 ----
dat_temp <- dat %>% 
  filter(Region == "Malmö") %>% 
  group_by(År) %>% 
  mutate(Totalt_antal = sum(Antal, na.rm = T),
         p = Antal / Totalt_antal)
dat_temp2 <- dat_temp %>% rename(Parti1 = Parti)

mg42 <- dat_temp %>% 
  ggplot(aes(År, p, col = Parti)) +
  geom_line(aes(col = NA, group = Parti1), data = dat_temp2, col = "grey80") +
  geom_line(size = 2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = seq(1980, 2010, 10)) +
  facet_wrap(~ Parti) +
  theme_bw() +
  theme_mg5() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey10"))
mg42

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
  geom_point(size = 3) +
  geom_path(size = 1) +
  facet_wrap(~ Parti) +
  theme_mg5() +
  theme(panel.grid = element_line(color = "grey10"))
mg43

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
  mutate(Differens = sum(p) - 2 * sum(p * (Region == "Sverige")))

mg44 <- ggplot(dat_temp, aes(p, fct_reorder(Parti, p, sum), col = Region)) +
  geom_line(aes(group = Parti), size = 2) +
  geom_point(size = 4) +
  theme_mg5() +
  theme(panel.grid = element_line(color = "grey50"))
mg44

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

mg45 <- ggplot(dat_temp, aes(Differens, Parti, col = Relativt_störst)) +
  geom_point() +
  geom_segment(aes(yend = Parti, xend = 0)) +
  xlim(-0.075,0.075) +
  theme_mg5() +
  theme(panel.grid = element_line(color = "grey20"), 
        legend.position = "none")
mg45

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
  filter(Region == "Malmö", Parti != "ej röstande") %>% 
  mutate(Slutår = map_dbl(År, foo),
         Antal = ifelse(is.na(Antal), 0, Antal)) %>% 
  group_by(År) %>% 
  mutate(p = Antal / sum(Antal),
         p_cs = cumsum(p),
         p_cs_lower = lag(p_cs, default = 0)) %>% 
  ggplot(aes(xmin = År, xmax = Slutår, ymin = p_cs_lower, ymax = p_cs, fill = Parti)) +
  geom_rect(col = "black") +
  geom_hline(yintercept = seq(0, 1, 0.25), col = "white", alpha = 0.5) +
  geom_vline(xintercept = c(1973, 2022), col = "white") +
  scale_x_continuous(name = "År", breaks = valår, expand = c(0,0)) +
  scale_y_continuous(name = "Andel", expand = c(0,0)) +
  theme(axis.ticks = element_blank()) +
  theme_mg5()
mg47

# Malmö, piecharts over time, animated, mg48 ----
mg48 <- dat %>% 
  filter(Region == "Malmö") %>% 
  mutate(Antal = ifelse(is.na(Antal), 1, Antal)) %>% 
  ggplot(aes(0, Antal, fill = Parti)) +
  geom_bar(stat = "identity", position = position_fill(), col = "black") +
  xlim(-2,1) +
  coord_polar(theta = "y") +
  theme_mg5() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  ease_aes() +
  transition_time(År)
mg48

# Malmö and neighbours, mg49 ----
mg49 <- dat %>% 
  filter(Region %in% c("Malmö", "Vellinge", "Lomma", "Burlöv", "Staffanstorp", "Svedala"),
         År == 2018) %>%
  group_by(År, Region) %>% 
  mutate(p = Antal / sum(Antal)) %>% 
  ungroup() %>% arrange(Region) %>% 
  mutate(Region = fct_reorder(Region, p * (Parti == "Socialdemokraterna"), sum, .desc = T)) %>% 
  ggplot(aes(Region, p, col = Parti, group = Parti)) +
  geom_point(size = 3) +
  geom_line() +
  theme_mg5() +
  theme(panel.grid = element_line(color = "grey40"))
mg49

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
  ggplot(aes(p, Parti)) +
  geom_boxplot(col = "white", fill = "grey60") +
  geom_point(data = . %>% filter(Region == "Malmö"), fill = "red", shape = 21, size = 3) +
  geom_text(aes(x = 0.5, label = Text), data = . %>% filter(Region == "Malmö"), col = "white") +
  theme_mg5() +
  theme(panel.grid = element_line(color = "grey30"))
mg50

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
