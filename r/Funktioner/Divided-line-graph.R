# Linjediagram med ribbon till nollan (eller något annat värde) med en för om linjen är under noll 
# och en annan om linjen är över noll.
library(sf)

dat <- tibble(t = 1:200, x = rnorm(200))

xy_minmax <- dat %>% summarise_all(c(min, max)) %>% unlist()

dat2 <- bind_rows(tibble(t = 1, x = -5),
                  dat,
                  tibble(t = c(200, 1), x = c(-5, -5)))


a <- st_polygon(list(as.matrix(dat2))) %>%
  st_sfc() %>%
  st_sf()

dat <- tibble(x = c(1,1,200,200,1,1,1,200,200,1),
              y = c(-5,0,0,-5,-5,0,5,5,0,0),
              obj = letters[c(1,1,1,1,1,2,2,2,2,2)])

dat3 <- dat %>%
  nest(data = c(x,y)) %>%
  mutate(geo = map(data, ~st_polygon(list(as.matrix(.x)))) %>% st_sfc()) %>%
  st_sf() %>%
  select(-data) %>%
  ungroup()

dat_temp <- a %>%
  st_difference(dat3) %>%
  slice(1) %>%
  st_cast("POLYGON") %>%
  mutate(id = paste0("up", 1:n())) %>%
  group_by(id)

dat_temp2 <- a %>%
  st_difference(dat3) %>%
  slice(2) %>%
  st_cast("POLYGON") %>%
  st_coordinates() %>%
  as_tibble()

st_coordinates(dat_temp) %>%
  as_tibble() %>%
  ggplot(aes(X, Y, group = L2)) +
  geom_polygon(fill = "darkgreen") +
  geom_polygon(data = dat_temp2, fill = "red") +
  coord_polar()


dat3 %>% st_difference(a) %>% plot()


# Alternative solution. Plot background then add negative space ----
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
