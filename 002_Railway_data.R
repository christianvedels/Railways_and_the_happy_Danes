# Railways
#
# Date updated:   2023-11-17
# Auhtor:         Christian Vedel 
# Purpose:        This script adds a functions to calculate stats for railways
#                 and parishes given two shape files: Parish shapes and railways
#                 shapes. 

# ==== Libraries ====
library(tidyverse)
source("000_functions.R")
library(sf)
library(ggspatial)
library(foreach)

# ==== Read data ====
shape = read_sf("../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp")
shape_parishes = read_sf("Data/sogne_shape/sogne.shp")

# test function
test = calc_rail( # Comes from functions
  shape,
  shape_parishes,
  verbose = TRUE,
  plots = TRUE,
  id = "test",
  years = 1846:1856
)

# ==== Run it ====
railways_panel = calc_rail(
  shape,
  shape_parishes,
  verbose = TRUE,
  plots = TRUE,
  id = "actual_railways",
  years = 1846:2020
)

# ==== Simple summary stats ====
railways_panel %>% 
  group_by(Year) %>% 
  summarise(
    Connected_rail = mean(Connected_rail)
  ) %>% 
  ggplot(aes(Year, Connected_rail)) + geom_line() + 
  theme_bw()

# ==== Saving data ====
railways_panel %>% 
  write_csv2("Data/Panel_of_railways_in_parishes.csv")

