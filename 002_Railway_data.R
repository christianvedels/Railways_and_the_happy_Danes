# Railways
#
# Date updated:   2023-10-02
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

# ==== Functions ====
ratio_funky = function(x){
  ex = extent(x)
  y_len = ex@ymax - ex@ymin
  x_len = ex@xmax - ex@xmin
  res = y_len / x_len
  return(res)
}

# ==== Calculate distance ====
calculate_distance = function(parish_geometry, railways) {
  # Compute parish centroid
  suppressWarnings({ # to avoid 'st_centroid assumes attributes are constant over geometries'
    parish_centroid = st_centroid(parish_geometry)
  })
  # Compute distance
  distances = st_distance(parish_centroid, railways)
  min_distance = min(distances, na.rm = TRUE)
  return(min_distance)
}

# ==== Read data ====
shape = read_sf("../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp")
shape_parishes = read_sf("Data/sogne_shape/sogne.shp")

# ==== Railway panel for parishes ====
# shape:          Shape of railways (or instrumented railways)
# shape_parishes: Shape of parishes 
# verbose:        Should updates be printed?
# plots:          Should plots of each instance be constructed?
# id:             A descriptive identifying string
# years:          Which years should this run for?

calc_rail = function(shape,
                     shape_parishes,
                     verbose = TRUE,
                     plots = TRUE,
                     id = "placeholder",
                     years = 1846:2020) {
  
  # Convert to same coordinate system
  shape_parishes = st_transform(shape_parishes, st_crs(shape))
  
  # Run loop
  railways_panel = foreach(y = years, .combine = "bind_rows") %do% {
    
    # Subset to period
    shape_y = shape %>% subset(y >= opened & y <= fake_close)
    
    # Initiatilize empty df
    if (NROW(shape_y) == 0) {
      return(data.frame(
        GIS_ID = shape_parishes$GIS_ID,
        Connected_rail = 0,
        Year = y
      ))
    }
    
    # Test conncection
    connected_y = shape_parishes %>%
      st_intersects(shape_y) %>%
      as.character() %>%
      data.frame(Connected_rail = .) %>%
      mutate(Connected_rail = ifelse(Connected_rail == "integer(0)", 0, 1)) %>%
      mutate(GIS_ID = shape_parishes$GIS_ID)
    
    # Clean result
    result_y = data.frame(GIS_ID = shape_parishes$GIS_ID) %>%
      left_join(connected_y, by = "GIS_ID") %>%
      mutate(Connected_rail = ifelse(is.na(Connected_rail), 0, Connected_rail))
    
    # Test validity
    if (any(!result_y$Connected_rail %in% c(0, 1))) {
      stop("This should not be possible")
    }
    
    # Calculate the distances to the nearest railway using foreach
    if(verbose){
      cat("\nCalculating distances\n")
    }
    
    distances = foreach(i = 1:NROW(shape_parishes), .combine = "c") %do% {
      dist_i = calculate_distance(shape_parishes[i, ], shape_y)
      dist_i = as.numeric(dist_i) / 1000 # Convert to km
      if(verbose){
        cat(paste0(i," is ", round(dist_i, 2), "m from rail in ", y, "                               \r"))
      }
      
      return(dist_i)
    }
    cat("\n")
    result_y$Distance_to_nearest_railway = distances
    
    parishes_covered = sum(result_y$Connected_rail)
    
    
    # Make plot
    if(plots){
      # Convert to 'yes' / 'no' to plot
      shape_parishes$tmp = ifelse(result_y$Connected_rail == 1, "Yes", "No")
      
      # Distance 
      shape_parishes$Dist = result_y$Distance_to_nearest_railway
      
      # Connected plots
      p1 = ggplot() +
        layer_spatial(aes(fill = tmp, col = tmp),
                      size = 0,
                      data = shape_parishes) +
        layer_spatial(data = shape_y) +
        theme_bw() +
        labs(
          title = paste0("Year ", y),
          subtitle = paste0(
            parishes_covered,
            " of ",
            NROW(shape_parishes),
            " parishes covered (",
            pretty_pct(parishes_covered / NROW(shape_parishes)),
            ")"
          ),
          caption = "Data from Fertner (2013)",
          col = "Parish connected:",
          fill = "Parish connected:"
        ) +
        theme(legend.position = "bottom")
      
      # Distance to rail
      p2 = ggplot() +
        layer_spatial(aes(fill = Dist, col = Dist),
                      size = 0,
                      data = shape_parishes) +
        layer_spatial(data = shape_y) +
        theme_bw() +
        labs(
          title = paste0("Year ", y),
          subtitle = paste0(
            parishes_covered,
            " of ",
            NROW(shape_parishes),
            " parishes covered (",
            pretty_pct(parishes_covered / NROW(shape_parishes)),
            ")"
          ),
          caption = "Data from Fertner (2013)",
          col = "Distance to rail (km):",
          fill = "Distance to rail (km):"
        ) +
        theme(legend.position = "bottom") + 
        scale_fill_gradient(
          low = "#273a8f",
          high = "#DE7500"
        ) + 
        scale_color_gradient(
          low = "#273a8f",
          high = "#DE7500"
        )
      
      # Save plots
      fname1 = paste0("Plots/Plots_parish_connected/", id ,"N_Y", y, ".png")
      fname2 = paste0("Plots/Plots_distance_to_rail/", id ,"N_Y", y, ".png")
      w = 6
      ggsave(fname1,
             plot = p1,
             width = w,
             height = 0.9 * w)
      ggsave(fname2,
             plot = p2,
             width = w,
             height = 0.9 * w)
        
    }
    
    cat(y, "          \r")
    
    
    # Results to df
    result_y = result_y %>%
      mutate(Year = y)
    return(result_y)
  }
  
  
  return(railways_panel)
}


# test
test = calc_rail(
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

