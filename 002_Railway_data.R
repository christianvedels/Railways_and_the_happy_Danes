# Data clean
#
# Date updated:   2023-09-15
# Auhtor:         Christian Vedel 
# Purpose:        Cleans data

# ==== Libraries ====
library(tidyverse)
source("00_functions.R")
library(sf)
library(rgdal)
library(ggspatial)
library(raster)
library(foreach)

# ==== Functions ====
ratio_funky = function(x){
  ex = extent(x)
  y_len = ex@ymax - ex@ymin
  x_len = ex@xmax - ex@xmin
  res = y_len / x_len
  return(res)
}

# ==== Read data ====
shape = readOGR("../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp")
shape_parishes = readOGR("Data/sogne_shape/sogne.shp")
geo_data = shape_parishes@data

shape = spTransform(shape, "+proj=longlat +zone=32 +ellps=GRS80")
shape_parishes = spTransform(shape_parishes, "+proj=longlat +zone=32 +ellps=GRS80")


# ==== Railway panel for parishes ====
railways_panel = foreach(y = 1846:2020, .combine = "bind_rows") %do% {
  shape_y = shape %>% subset(y >= opened & y <= fake_close)
  
  if(NROW(shape_y)==0){
    return(
      data.frame(
        GIS_ID = shape_parishes$GIS_ID,
        Connected_rail = 0,
        Year = y
      )
    )
  }
  
  connected_y = shape_parishes %>% 
    over(shape_y) %>% 
    dplyr::select(label2) %>% 
    transmute(
      Connected_rail = ifelse(is.na(label2), 0, 1)
    ) %>% 
    mutate(
      GIS_ID = shape_parishes$GIS_ID
    )
  
  result_y = geo_data %>% dplyr::select(GIS_ID) %>% 
    left_join(connected_y, by = "GIS_ID") %>% 
    mutate(
      Connected_rail = ifelse(is.na(Connected_rail), 0, Connected_rail)
    )
  
  if(any(!result_y$Connected_rail %in% c(0,1))){
    stop("This should not be possible")
  }
  
  shape_parishes@data$tmp = ifelse(result_y$Connected_rail==1, "Yes", "No")
  
  parishes_covered = sum(result_y$Connected_rail)
  
  p1 = ggplot() +
    layer_spatial(
      aes(fill = tmp, col = tmp),
      size = 0,
      data = shape_parishes
    ) +
    layer_spatial(data = shape_y) +
    theme_bw() +
    labs(
      title = paste0("Year ",y),
      subtitle = paste0(
        parishes_covered, " of ",
        NROW(shape_parishes@data),
        " parishes covered (",
        pretty_pct(parishes_covered/NROW(shape_parishes@data)),")"
      ),
      caption = "Data from Fertner (2013)",
      col = "Parish connected:",
      fill = "Parish connected:"
    ) +
    theme(
      legend.position = "bottom"
    )
  
  fname = paste0("Plots/Plots_parish_connected/N_Y",y,".png")
  w = 6
  ggsave(fname, plot = p1, width = w, height = 0.9*w)
  
  cat(y,"          \r")
  
  # Results to df
  result_y = result_y %>% 
    mutate(Year = y)
  return(result_y)
}

# ==== Simple summary stats ====
railways_panel %>% 
  group_by(Year) %>% 
  summarise(
    Connected_rail = mean(Connected_rail)
  ) %>% 
  ggplot(aes(Year, Connected_rail)) + geom_line()

railways_panel %>% 
  filter(Year <= 1901) %>% 
  left_join(geo_data, by = "GIS_ID") %>% 
  filter(distance_oce < 5000) %>% 
  group_by(Year, limfjord_placement) %>% 
  summarise(
    Connected_rail = mean(Connected_rail)
  ) %>% 
  ggplot(aes(Year, Connected_rail, col = limfjord_placement)) + 
  geom_line()

