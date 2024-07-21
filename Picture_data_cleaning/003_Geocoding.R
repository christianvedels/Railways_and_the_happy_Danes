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

data0 = data0 %>% geocode(address = location, method = "osm", custom_query = list(countrycodes = 'dk'))

# ==== Save results ====
saveRDS(data0, "../Data not redistributable/Tmp_data/Tmp_picture_data.rds")



x = data0 %>% drop_na(`Se paa kort`)
