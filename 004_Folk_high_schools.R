# Folk High School (Højskoler)
#
# Date updated:   2023-10-02
# Auhtor:         Christian Vedel 
# Purpose:        Cleans data on high schools# ==== Libraries ====

# ==== Libraries ====
library(tidyverse)
library(sf)
library(ggspatial)
library(foreach)

# ==== Load data ====
load("../Data not redistributable/All_raw_data_for_project.Rdata")
shape_parishes = read_sf("Data/sogne_shape/sogne.shp")

# Load railways to be able to cast to this format
shape = read_sf("../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp")

# ==== High school data ====
geo_tmp = hoejskoler %>% 
  distinct(id, lat, long)

geo_tmp_sf = st_as_sf(
  geo_tmp, coords = c("long", "lat"),
  crs = st_crs(shape_parishes)
)

shape_parishes = st_transform(shape_parishes, st_crs(shape))
geo_tmp_sf = st_transform(geo_tmp_sf, st_crs(shape))

gis_id = st_intersects(geo_tmp_sf, shape_parishes) %>% 
  as.character() %>% 
  unique()

ggplot() + 
  layer_spatial(
    size = 0,
    data = shape_parishes
  ) + 
  layer_spatial(
    geo_tmp_sf
  ) + 
  theme_bw()


