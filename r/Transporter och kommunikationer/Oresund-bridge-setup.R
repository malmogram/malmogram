# ---
# Title: Oresund bridge - setup
# Purpose: 
# Start date: 
# Comments:
# ---

# Packages ----
library(tidyverse)

# Import ----
dat_ore <- read_csv("Data/Transporter och kommunikationer/Oresund-bridge-traffic-statistics-2000-2021-nov.csv")
dat_ore_tag <- read_csv("Data/Transporter och kommunikationer/Oresund-bridge-train-traffic-statistics.csv")
