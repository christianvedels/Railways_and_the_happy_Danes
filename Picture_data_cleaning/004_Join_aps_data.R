# Data cleaning
#
# Date updated:   2024-07-31
# Author:         Tom Görges, Christian Vedel
# Purpose:        This script loads and cleans the data created by archival picture sentiments

# ==== Libraries ====
library(tidyverse)
source("Picture_data_cleaning/000_Functions.R")

# ==== Read data ====
# Read aps data
df = read_aps_data("archival_picture_sentiments/fer_results.csv")

# Metadata
metadata = read_rds("../Data not redistributable/Tmp_data/Tmp_picture_data.rds")

# Geographical features
path = "https://raw.githubusercontent.com/christianvedels/A_perfect_storm_replication/main/Data/Geo.csv"
geo = read_csv2(path, guess_max = 2000)

# ==== Join data ====
df = df %>% # Joining on metadata
  mutate(
    id = gsub(".png","", Image)
  ) %>% 
  full_join(metadata, by = "id")

# Time periods
df = df %>% 
  mutate(
    decade = round0(midpoint_year, 10)
  )


# ==== Save data ====
saveRDS(df, "../Data not redistributable/Tmp_data/Tmp_picture_data.rds")
