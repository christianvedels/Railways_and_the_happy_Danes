# Data clean
#
# Date updated:   2023-10-02
# Auhtor:         Christian Vedel 
# Purpose:        Cleans data

# ==== Libraries ====
library(tidyverse)
source("000_functions.R")
library(sf)
library(ggspatial)
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
shape = read_sf("../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp")
shape_parishes = read_sf("Data/sogne_shape/sogne.shp")

# shape = st_transform(shape, "+proj=longlat +zone=32 +ellps=GRS80")
shape_parishes = st_transform(shape_parishes, st_crs(shape))


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
    st_intersects(shape_y)%>% 
    as.character() %>% 
    data.frame(
      Connected_rail = .
    ) %>% 
    mutate(
      Connected_rail = ifelse(Connected_rail == "integer(0)", 0, 1)
    ) %>%
    mutate(
      GIS_ID = shape_parishes$GIS_ID
    )

  result_y = data.frame(GIS_ID = shape_parishes$GIS_ID) %>% 
    left_join(connected_y, by = "GIS_ID") %>%
    mutate(
      Connected_rail = ifelse(is.na(Connected_rail), 0, Connected_rail)
    )

  if(any(!result_y$Connected_rail %in% c(0,1))){
    stop("This should not be possible")
  }

  shape_parishes$tmp = ifelse(result_y$Connected_rail==1, "Yes", "No")

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
        NROW(shape_parishes),
        " parishes covered (",
        pretty_pct(parishes_covered/NROW(shape_parishes)),")"
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
  ggplot(aes(Year, Connected_rail)) + geom_line() + 
  theme_bw()

# ==== Saving data ====
railways_panel %>% 
  write_csv2("Data/Panel_of_railways_in_parishes.csv")

