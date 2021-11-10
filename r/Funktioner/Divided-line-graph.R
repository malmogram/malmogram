# Linjediagram med ribbon till nollan (eller något annat värde) med en färg om linjen är under noll 
# och en annan om linjen är över noll.
library(tidyverse)
library(sf)

t_max <- 1000
dat <- tibble(t = 1:t_max) %>% 
  mutate(x = (1 - t / t_max) * cumsum(rnorm(n())) + t / t_max * rev(cumsum(rnorm(n()))))

xy_minmax <- dat %>% 
  summarise_all(c(min, max)) %>% 
  unlist()

dat_low <- bind_rows(tibble(t = xy_minmax[1], x = xy_minmax[2] - 1),
                     dat,
                     tibble(t = c(xy_minmax[3], xy_minmax[1]), x = c(xy_minmax[2] - 1, xy_minmax[2] - 1)))

dat_high <- bind_rows(tibble(t = xy_minmax[1], x = xy_minmax[4] + 1),
                      dat,
                      tibble(t = c(xy_minmax[3], xy_minmax[1]), x = c(xy_minmax[4] + 1, xy_minmax[4] + 1)))

ggplot() + 
  geom_polygon(aes(t, x), data =dat_low, fill = "#ddddff", col = "black") + 
  geom_polygon(aes(t, x), data = dat_high, fill = "#ffdddd", col = "black")

dat_low <- st_polygon(list(as.matrix(dat_low))) %>%
  st_sfc() %>%
  st_sf()
dat_high <- st_polygon(list(as.matrix(dat_high))) %>%
  st_sfc() %>%
  st_sf()

dat_rect <- tibble(x = xy_minmax[c(1,1,3,3,1,1,1,3,3,1)],
                   y = c(xy_minmax[2] - 1,0,0,xy_minmax[2] - 1,xy_minmax[2] - 1,
                         0,xy_minmax[4] + 1,xy_minmax[4] + 1,0,0),
                   obj = letters[c(1,1,1,1,1,2,2,2,2,2)])

dat_rect <- dat_rect %>%
  nest(data = c(x,y)) %>%
  mutate(geo = map(data, ~st_polygon(list(as.matrix(.x)))) %>% st_sfc()) %>%
  st_sf() %>%
  select(-data) %>%
  ungroup()

dat_temp1 <- dat_low %>%
  st_difference(dat_rect) %>%
  slice(1) %>%
  st_cast("POLYGON") %>%
  mutate(id = paste0("up", 1:n()))

dat_temp2 <- dat_high %>%
  st_difference(dat_rect) %>%
  slice(2) %>%
  st_cast("POLYGON") %>%
  mutate(id = paste0("down", 1:n()))

ggplot() + geom_sf(data = dat_temp1, fill = "red") +
  geom_sf(data = dat_temp2, fill = "green")

foo <- . %>%  
  mutate(coord = map(geometry, ~ st_coordinates(.x) %>% as.data.frame() %>% as_tibble())) %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  unnest(coord)

dat_t <- bind_rows(foo(dat_temp1), foo(dat_temp2)) %>% 
  mutate(type = substring(id, 1, 1))

ggplot(dat_t, aes(X, Y, group = id, fill = type)) +
  geom_polygon() +
  coord_polar()

# Alternative solution. Plot background then add negative space ----
library(cowplot)
collection <- list()
for(i in 1:12){
  dat <- tibble(t = 1:200, x = cumsum(rnorm(200)))
  
  dat2 <- bind_rows(tibble(t = 1, x = 5),
                    dat,
                    tibble(t = c(200, 1), x = c(5, 5)))
  
  dat3 <- tibble(x = c(1,1,200,200,1,1,1,200,200,1),
                 y = c(-5,0,0,-5,-5,0,5,5,0,0),
                 obj = letters[c(1,1,1,1,1,2,2,2,2,2)])
  
  g <- ggplot() +
    geom_polygon(aes(x, y, fill = obj), data = dat3) +
    geom_polygon(aes(t, x), data = dat2, fill = "#000080", color = "#000080") +
    theme_nothing() +
    coord_polar()
  collection[[i]] <- g
}

library(patchwork)

call <- paste0(paste0("collection[[", 1:12, "]]"), collapse = " + ")
eval(parse(text = call))

collection[[1]] + collection[[2]] + collection[[3]] + 
  collection[[4]] + collection[[5]] + collection[[6]] + 
  collection[[7]] + collection[[8]] + collection[[9]] + 
  collection[[10]] + collection[[11]] + collection[[12]] & 
  theme(plot.background = element_rect(fill = "#000080", color = "#000080"))
