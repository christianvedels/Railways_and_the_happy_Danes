# Evaluating instrument performanc
#
# Date updated:   2023-12-08
# Auhtor:         Tom Görges, Christian Vedel
# Purpose:        Reads shape files of predicted railways based on 
#                 least cost paths

# ==== Libraries ====
library(tidyverse)
library(foreach)
library(ggspatial)
library(sf)
library(permute)
library(foreach)
library(raster)
library(elevatr)

source("000_Functions.R") # Contains calc_rail()

# ==== Load and merge instruments + finding instrment with best fit (RMSE) ====
# Read files
real_rail = read_csv2("Data/Panel_of_railways_in_parishes.csv") %>%
  filter(Year < 1878) # filter because intstrument only up to 1877

generated_rails = list.files("Data/Instruments")
instruments = foreach(f = generated_rails) %do% {
  data_f = read_csv2(paste0("Data/Instruments/", f)) %>%
    filter(Year < 1878) %>% 
    rename(
      Connected_rail_pred = Connected_rail,
      Distance_to_nearest_railway_pred = Distance_to_nearest_railway
    )
  
  # Join with real rail
  data_f = data_f %>% 
    left_join(real_rail, by = c("GIS_ID", "Year"))
  
  return(data_f)
}

# ==== Evaluate performance ====
# RMSE/Accuracy 
foreach(data_f = instruments, .combine = "bind_rows") %do% {
  data_f %>% 
    summarise(
      Accuracy = mean(Connected_rail_pred == Connected_rail, na.rm = TRUE),
      RMSE = sqrt(mean(Distance_to_nearest_railway_pred - Distance_to_nearest_railway, na.rm = TRUE)^2),
      parameter = parameter[1]
    )
}

# ==== Confusion matrix ====
foreach(data_f = instruments) %do% {
  conf_mat = confusion_matrix(data_f)
  
  p1 = conf_mat %>% 
    mutate(
      Truth = relevel(factor(Truth), ref = "TRUE")
    ) %>% 
    ggplot(aes(x = Predicted, y = Truth, fill = Pct, label = label)) +
    geom_tile(color = "white") +
    geom_text(vjust = 1) +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_bw() + 
    theme(
      legend.position = "bottom"
    ) + 
    labs(
      fill = "Pct of support", 
      title = "Confusion Matrix",
      subtitle = paste0("Parameter: ", unique(conf_mat$parameter)),
      x = "Predicted to be connected",
      y = "Actually connected"
    )
  
  f = paste0("Plots/Instrument_confusion_matrix/Param_",unique(conf_mat$parameter), ".png")
  
  ggsave(f, plot = p1, width = 8, height = 6)
  
  return(1)
}

# Confusion matrix every year
foreach(i = seq(length(instruments))) %do% {
  data_f = instruments[[i]]
  # Create folder if it does not exist
  dir_for_plots = paste0("Plots/Instrument_confusion_matrix/By_year/", unique(data_f$parameter))
  if(!dir.exists(dir_for_plots)){
    dir.create(dir_for_plots)
  }
  
  foreach(y = unique(data_f$Year)) %do% {
    data_fy = data_f %>% filter(Year == y)
    conf_mat = confusion_matrix(data_fy)
    
    p1 = conf_mat %>% 
      mutate(
        Truth = relevel(factor(Truth), ref = "TRUE")
      ) %>% 
      ggplot(aes(x = Predicted, y = Truth, fill = Pct, label = label)) +
      geom_tile(color = "white") +
      geom_text(vjust = 1) +
      scale_fill_gradient(low = "white", high = "blue") +
      theme_bw() + 
      theme(
        legend.position = "bottom"
      ) + 
      labs(
        fill = "Pct of support", 
        title = paste("Confusion Matrix", y),
        subtitle = paste0("Parameter: ", unique(conf_mat$parameter)),
        x = "Predicted to be connected",
        y = "Actually connected"
      )
    
    f = paste0(dir_for_plots, "/Year", y ,"_",unique(conf_mat$parameter), ".png")
    
    ggsave(f, plot = p1, width = 8, height = 6)
    
    return(1)
  }
  
  return(1)
}

# ==== Predicted versus actual distance to railway ====
foreach(i = seq(length(instruments))) %do% {
  data_f = instruments[[i]]
  param_f = data_f$parameter %>% unique()
  
  # Create basice plot
  p1 = data_f %>% 
    ggplot(
      aes(Distance_to_nearest_railway_pred, Distance_to_nearest_railway)
    ) + 
    geom_point() + 
    theme_bw() +
    labs(
      y = "Distance to rail\n(Actual)",
      x = "Distance to rail\n(Predicted)"
    ) + 
    geom_smooth(method = "lm", se = FALSE)
  
  # RMSE
  RMSE = data_f %>% 
    summarise(
      RMSE = sqrt(mean(Distance_to_nearest_railway_pred - Distance_to_nearest_railway, na.rm = TRUE)^2)
    ) %>% unlist() %>% signif(3)
  
  # Rsq
  mod_tmp = lm(Distance_to_nearest_railway ~ Distance_to_nearest_railway_pred, data = data_f)
  Rsq = summary(mod_tmp)$r.squared %>% signif(3)
  
  # Add info to plot
  p1 = p1 + 
    labs(
      title = "Distance to rail versus predicted distance to rail",
      subtitle = paste0("Parameter: ", param_f, "   RMSE: ", RMSE, ";   R^2: ", Rsq)
    )
  
  f = paste0("Plots/Instrument_pred_vs_actual_dist_to_rail/Param_",param_f, ".png")
  
  ggsave(f, plot = p1, width = 8, height = 6)
}

# THE FOLLOWING DOES NOT WORK IN A FRESH ENVIRONMENT
# # === Plot preparations ===
# shape_data <- st_read("../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp") %>% st_transform(4326)
# outline_dk <- st_read("../Data not redistributable/Outline DK/DNK_adm0.shp")  %>% st_transform(4326)
# 
# # Obtain elevation raster (from OpenStreetMap)
# denmark_elev <- get_elev_raster(outline_dk, z = 9, source = "osm", clip = "locations") # z(oom) = 9 also used by package "movecost"
# plot(denmark_elev)
# 
# # Create slope raster: %
# slope_raster <- terrain(denmark_elev, opt='slope', unit='tangent')*100
# 
# hypo4 <- st_read("Data/lcp_shape_files/LCP_scrit4.shp") %>% st_transform(crs = 32632)
# hypo16 <- st_read("Data/lcp_shape_files/LCP_scrit16.shp") %>% st_transform(crs = 32632)
# 
# 
# # Convert the raster to a data frame
# slope_df <- as.data.frame(rasterToPoints(slope_raster), stringsAsFactors = FALSE)
# 
# # Rename columns for clarity
# colnames(slope_df) <- c("long", "lat", "slope")
# 
# # Applying a log transformation (for illustration purposes only)
# slope_df$slope_log <- log1p(slope_df$slope)  # log1p to avoid log(0)
# 
# # === Plot ===
# 
# # Plot slope raster with log-transformed slope values
# plot_gg <- ggplot() +
#   geom_tile(data = slope_df, aes(x = long, y = lat, fill = slope_log)) +
#   scale_fill_gradient(low = "grey95", high = "black") +
#   theme_minimal() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         panel.background = element_rect(fill = "white", colour = "white"),
#         plot.background = element_rect(fill = "white", colour = "white")) +
#   guides(fill = "none")  # This line removes the legend for slope values
# 
# 
# # Adding cities (names)
# plot_gg <- plot_gg + geom_point(data = subset_market_towns_df, aes(x = long, y = lat), color = "black", size = 2, shape = 4) + geom_text_repel(data = subset_market_towns_df, aes(x = long, y = lat, label = Market_town), size = 2.5)
# 
# # Adding railway lines opened <= 1877
# plot_gg <- plot_gg + geom_sf(data = subset(shape_data, opened <= 1877), size = 0.5, inherit.aes = FALSE, alpha = 1)
# 
# # Adding LCP('s)
# plot_gg <- plot_gg + geom_sf(data = hypo4, size = 1, inherit.aes = FALSE, col = "firebrick1")
# 
# # Adding LCP('s)
# plot_gg <- plot_gg + geom_sf(data = hypo16, size = 1, inherit.aes = FALSE, col = "dodgerblue")
# 
# # Change legend position
# #plot_gg <- plot_gg + theme(legend.position = c(0.75, 0.7), legend.text = element_text(size = 12), legend.title = element_text(size = 14), legend.key.size = unit(4, "mm"))
# 
# 
# # Plot
# plot_gg
# 
# 
# #####
# ################################################################################
# # Save plot
# #ggsave(filename = "../Railways_and_the_happy_Danes/Plots/hypo_rail_scrit_4_16.png", plot = plot_gg, width = 8, dpi = 600)
# ################################################################################