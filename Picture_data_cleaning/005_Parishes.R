# Parishes to pictures
#
# Date updated:   2024-10-15
# Author:         Tom Görges, Christian Vedel
# Purpose:        This scripts assigns 1820 parish borders to pictures

# ==== Libraries ====
library(tidyverse)
library(sf)
library(foreach)
library(progress)
source("Picture_data_cleaning/000_Functions.R")

# ==== Setup ====
progress_bar_format = "[:bar] :elapsedfull -- :current of :total -- :percent eta: :eta"

# ==== Read data ====
# Read aps data
df = read_rds("../Data not redistributable/Tmp_data/Tmp_picture_data.rds")
shape_parishes = read_sf("Data/sogne_shape/sogne.shp")
crs0 = st_crs(shape_parishes)

misc = read_sf("../Data not redistributable/Railways Fertner enriched/") # Take CRS from here
shape_parishes = st_transform(shape_parishes, st_crs(misc))

# ==== Distinct coordinates ====
df_coords = df %>% 
  distinct(long, lat) %>% 
  drop_na() %>% 
  mutate(
    tmp_id = 1:n()
  )

# ==== Check if coords in parish ====
# Convert the coordinates to an sf object
df_coords_sf = st_as_sf(df_coords, coords = c("long", "lat"), crs = crs0)
df_coords_sf = st_transform(df_coords_sf, crs = st_crs(shape_parishes))

# Spatial join to check which coordinates fall within which parish
parish_in_coords = st_join(df_coords_sf, shape_parishes, join = st_intersects)

# Drop irrelevant
parish_in_coords = parish_in_coords %>%
  st_drop_geometry() %>% 
  select(tmp_id, GIS_ID)

df_coords = df_coords %>% 
  left_join(
    parish_in_coords, by = "tmp_id"
  )

# ==== Calculate distance to closest parish centroid for all points ====
# Calculate parish centroids
shape_parishes_centroids = st_centroid(shape_parishes)

# Initialize progress bar
pb = progress_bar$new(
  total = nrow(df_coords_sf),
  format = paste0("Calculating distances: ", progress_bar_format)
)

res_distance = foreach(i = seq_len(nrow(df_coords_sf)), .combine = "bind_rows") %do% {
  pb$tick()
  
  # Get the current coordinate
  df_coord_row = df_coords_sf[i, ]
  
  # Calculate the nearest parish centroid
  distances = st_distance(df_coord_row, shape_parishes_centroids) %>% as.numeric()
  closest_parish = shape_parishes_centroids[which.min(distances),]$GIS_ID
  distance = distances[which.min(distances)]
  
  if((length(closest_parish)>1) | (length(as.numeric(distance)) > 1)){
    stop(closest_parish, distance)
  }
  
  if((length(closest_parish) == 0) | (length(as.numeric(distance)) == 0)){
    stop(closest_parish, distance)
  }
  
  # Store the results
  data.frame(
    GIS_ID_nearest = closest_parish,
    distance_nearest_parish = as.numeric(distance) / 1000, # to km
    tmp_id = df_coord_row$tmp_id
  )
  
}

df_coords = df_coords %>% 
  left_join(res_distance, by = "tmp_id",
            relationship = "many-to-many")

# Assign nearest parish if within a threshold distance (e.g., 100 meters)
threshold_distance = 5 # adjust as needed
df_coords = df_coords %>%
  mutate(
    GIS_ID_w_closest = ifelse(is.na(GIS_ID) & distance_nearest_parish <= threshold_distance, GIS_ID_nearest, GIS_ID)
  )

# ==== Add parish information back to the original dataframe ====
df_with_parishes = df %>%
  left_join(df_coords, by = c("long", "lat"))

# ==== Save ====
saveRDS(df_with_parishes, "../Data not redistributable/Tmp_data/Tmp_picture_data.rds")
