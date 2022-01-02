# ---
# Title: Malmö University plots
# Purpose: 
# Start date: 211228
# Comments:
# ---

# Packages ----
library(tidyverse)

# Import ----
source("r/Utbildning/University-courses-setup.R", encoding = "UTF-8")
terms <- expand_grid(y = c("09", 10:21), x = c("VT", "ST", "HT")) %>% mutate(z = paste0(x, y)) %>% pull(z)

# Number of courses per term, mg93 ----
dat_uni_wide %>% 
  count(Termin) %>%
  mutate(Semester = substring(Termin, 1, 2),
         År = as.numeric(paste0("20", substring(Termin, 3, 4)))) %>% 
  ggplot(aes(År, n)) + 
  geom_col() +
  facet_wrap(~ Semester) +
  labs(y = "Antal kurser")

# Number of (first) applications per term, mg94 ----
dat_uni_wide %>% 
  count(Termin, wt = sok.sökande) %>% 
  mutate(Semester = substring(Termin, 1, 2),
         År = as.numeric(paste0("20", substring(Termin, 3, 4)))) %>% 
  ggplot(aes(År, n)) + 
  geom_col() +
  facet_wrap(~ Semester) +
  labs(y = "Antal ansökningar")

# Number of accepted applications per term, mg95 ----
dat_uni_wide %>% 
  count(Termin, wt = urval2.antagna) %>% 
  mutate(Semester = substring(Termin, 1, 2),
         År = as.numeric(paste0("20", substring(Termin, 3, 4)))) %>% 
  ggplot(aes(År, n)) + 
  geom_col() +
  facet_wrap(~ Semester) +
  labs(y = "Antal antagna kursplatser")

# Number of courses per subject at MaU, mg96 ----
dat_uni_wide %>% 
  filter(Termin %in% c("HT21", "VT21", "ST21"), ämnesgrupp != "EJ ANGIVEN") %>% 
  count(ämnesgrupp, sort = T) %>% 
  drop_na(ämnesgrupp) %>% 
  slice(1:30) %>% 
  ggplot(aes(n, fct_reorder(ämnesgrupp, n))) + 
  geom_col() +
  labs(y = "Ämne", x = "Antal kurser", title = "Antal kurser per ämne. Malmö universitet VT21, ST21 och HT21") +
  theme(plot.title.position = "plot")

# Applications per program at MaU, mg97 ----
dat_uni_wide %>% 
  filter(Termin %in% c("HT21"), utbildningstyp == "Program") %>% 
  group_by(anmälningsalternativ) %>% 
  summarise_at(vars(sok.kvinnligaFörstahandsökande, sok.manligaFörstahandsökande), sum) %>% 
  mutate(Total = sok.kvinnligaFörstahandsökande + sok.manligaFörstahandsökande) %>% 
  arrange(-Total) %>% 
  slice(1:20) %>% 
  select(-Total) %>% 
  pivot_longer(-anmälningsalternativ) %>% 
  mutate(name = ifelse(name == "sok.kvinnligaFörstahandsökande", "Kvinnor", "Män"),
         anmälningsalternativ = ifelse(anmälningsalternativ == "Media and Communication Studies: Culture, Collaborative Media, and Creative Industries, Master's pro", "Media and Communication Studies", anmälningsalternativ)) %>% 
  ggplot(aes(value, fct_reorder(anmälningsalternativ, value, sum), fill = name)) +
  geom_col(col = "black", width = 0.45) +
  labs(x = "Antal sökande", y = "Program", title = "Antal sökande per program. Malmö universitet HT21") +
  theme(legend.title = element_blank())

# Accepted per program at MaU, mg98 ----
dat_uni_wide %>% 
  filter(Termin %in% c("HT21"), utbildningstyp == "Program") %>% 
  group_by(anmälningsalternativ) %>% 
  summarise_at(vars(urval2.kvinnligaAntagna, urval2.manligaAntagna), sum) %>% 
  mutate(Total = urval2.kvinnligaAntagna + urval2.manligaAntagna) %>% 
  arrange(-Total) %>% 
  slice(1:20) %>% 
  select(-Total) %>% 
  pivot_longer(-anmälningsalternativ) %>% 
  mutate(name = ifelse(name == "urval2.kvinnligaAntagna", "Kvinnor", "Män"),
         anmälningsalternativ = ifelse(anmälningsalternativ == "Media and Communication Studies: Culture, Collaborative Media, and Creative Industries, Master's pro", "Media and Communication Studies", anmälningsalternativ)) %>% 
  group_by(anmälningsalternativ) %>% 
  mutate(Kvinnor = sum(value * (name == "Kvinnor")), Män = sum(value * (name == "Män"))) %>% 
  ggplot(aes(value, fct_reorder(anmälningsalternativ, value, sum), fill = name)) +
  geom_col(col = "black", width = 0.45) +
  geom_text(aes(x = Kvinnor + Män, label = paste(Män, "/", Kvinnor)), . %>% filter(name == "Kvinnor"), hjust = 0, size = 2.5, nudge_x = 5, nudge_y = 0.1) +
  labs(x = "Antal sökande", y = "Program", title = "Antal antagna per program. Malmö universitet HT21") +
  theme(legend.title = element_blank())
