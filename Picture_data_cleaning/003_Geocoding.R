# Data cleaning
#
# Date updated:   2024-07-04
# Author:         Tom Görges, Christian Vedel
# Purpose:        This script performs geocoding of archive locations

# ==== Libraries ====
library(tidyverse)
library(tidygeocoder)
source("Picture_data_cleaning/000_Functions.R")

# ==== Read data ====
data0 = read_rds("../Data not redistributable/Tmp_data/Tmp_picture_data.rds")


# ==== Geocode archives ====
data0 = data0 %>% geocode(address = location, method = "osm", custom_query = list(countrycodes = 'dk'))

data0 = data0 %>% 
  rename(
    long_archive = long,
    lat_archive = lat
  )

# ==== Handle coordinates from different sources ====
data0 = data0 %>% 
  mutate(
    long = ifelse(is.na(coords_long), centroid_long, coords_long),
    lat = ifelse(is.na(coords_lat), centroid_lat, coords_lat)
  ) %>% 
  mutate(
    long = ifelse(is.na(long), long_archive, long), # Archive coordinate
    lat = ifelse(is.na(lat), lat_archive, lat)
  )

# ==== Save results ====
saveRDS(data0, "../Data not redistributable/Tmp_data/Tmp_picture_data.rds")

x = data0 %>% drop_na(`Se paa kort`)
