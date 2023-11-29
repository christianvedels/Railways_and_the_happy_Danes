# Least Cost Path
#
# Date updated:   2023-11-29
# Author:         Tom Görges 
# Purpose:        Creation of Hypothetical Railway Networks based on Slope (%)


# Clear workspace
rm(list=ls())


# ==== Libraries ====
library(sf)
library(ggplot2)
library(elevatr)
library(viridis)
library(raster)
library(gdistance)
library(sp)
library(ggrepel)
library(rgdal)
library(tidygeocoder)
library(readr)
library(dplyr)

# ==== Load data (Railway shape data and Outline of Denmark) ===
shape_data <- st_read("../Data not redistributable/Railways Fertner/") %>% st_transform(4326)
outline_dk <- st_read("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Data/Raw/Outline DK/DNK_adm0.shp") %>% st_transform(4326)


# Obtain elevation raster
denmark_elev <- get_elev_raster(outline_dk, z = 9, source = "osm", clip = "locations") # z(oom) = 9 also used by package "movecost"
plot(denmark_elev)

# Create slope raster: %
slope_raster <- terrain(denmark_elev, opt='slope', unit='tangent')*100

plot(slope_raster)


# ==== Create and save transitions ===

# critical slope value
median_slope <- median(values(slope_raster), na.rm = T)

s_crit <- median_slope

# base cost + slope (Herzog and Costaz-Fernandez) + geocorrection
# Maybe increase directions = 16 if computational power permits

### changing critical slope value
#transitions <- transition(slope_raster, transitionFunction = function(x) (1 + (x / s_crit)^2) , directions = 8) %>% geoCorrection(type="c")
#transitions_scrit2 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 2)^2) , directions = 8) %>% geoCorrection(type="c")
#transitions_scrit3 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 3)^2) , directions = 8) %>% geoCorrection(type="c")
#transitions_scrit4 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 4)^2) , directions = 8) %>% geoCorrection(type="c")
#transitions_scrit5 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 5)^2) , directions = 8) %>% geoCorrection(type="c")
#transitions_scrit6 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 6)^2) , directions = 8) %>% geoCorrection(type="c")
#transitions_scrit7 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 7)^2) , directions = 8) %>% geoCorrection(type="c")
#transitions_scrit8 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 8)^2) , directions = 8) %>% geoCorrection(type="c")

### SAVE & LOAD ################################################################
#save(transitions, file = "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions.RData")
#save(transitions_scrit2, file = "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit2.RData")
#save(transitions_scrit3, file = "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit3.RData")
#save(transitions_scrit4, file = "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit4.RData")
#save(transitions_scrit5, file = "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit5.RData")
#save(transitions_scrit6, file = "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit6.RData")
#save(transitions_scrit7, file = "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit7.RData")
#save(transitions_scrit8, file = "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit8.RData")


# === Load Transitions ===
load("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions.RData") #  # 1 + (s/median)^2 Costaz-Fernandet et al. 2020
load("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit2.RData") # 1 + (s/2)^2
load("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit3.RData") # 1 + (s/3)^2
load("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit4.RData") # 1 + (s/4)^2
load("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit5.RData") # 1 + (s/5)^2
load("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit6.RData") # 1 + (s/6)^2
load("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit7.RData") # 1 + (s/7)^2
load("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions_scrit8.RData") # 1 + (s/8)^2: Herzog (2016)


# === Nodes / Market towns ===


# Reading in market towns
market_towns <- read_delim("C:/Users/Win7ADM/Desktop/Market_towns.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


# Calculate the median of the Pop1801 column
median_pop1801 <- median(market_towns$Pop1801, na.rm = TRUE)

# Subset the data frame, sort and select
subset_market_towns <- subset(market_towns, Pop1801 > median_pop1801 & Coastal == 1) %>%
  arrange(desc(Pop1801)) %>%
  select(Market_town, Pop1801)


### Add nodes
esbjerg <- data.frame(Market_town = "Esbjerg", Pop1801 = NA)
struer <- data.frame(Market_town = "Struer", Pop1801 = NA)
skive <- data.frame(Market_town = "Skive", Pop1801 = NA)
ringkobing <- data.frame(Market_town = "Ringkobing", Pop1801 = NA)
middelfart <- data.frame(Market_town = "Middelfart", Pop1801 = NA)
frederikshavn <- data.frame(Market_town = "Frederikshavn", Pop1801 = NA)
vordingborg <- data.frame(Market_town = "Masnedsund", Pop1801 = NA)
norresundby <- data.frame(Market_town = "Norresundby", Pop1801 = NA)

subset_market_towns <- rbind(subset_market_towns, esbjerg, skive, struer, ringkobing, middelfart, frederikshavn, vordingborg, norresundby)


### Get coordinates

### Change names for OpenStreetMap
subset_market_towns$Market_town[subset_market_towns$Market_town == "Koebenhavn"] <- "Copenhagen"
subset_market_towns$Market_town[subset_market_towns$Market_town == "Soenderborg"] <- "Sonderborg" 
subset_market_towns$Market_town[subset_market_towns$Market_town == "Aeroeskoebing"] <- "Ærøskøbing"
subset_market_towns$Market_town[subset_market_towns$Market_town == "Neksoe"] <- "Nexø"
subset_market_towns$Market_town[subset_market_towns$Market_town == "Rudkoebing"] <- "Rudkøbing"
subset_market_towns$Market_town[subset_market_towns$Market_town == "Roenne"] <- "Rønne"

# Geocode
subset_market_towns <- subset_market_towns %>% geocode(Market_town, method = 'osm')

# correct coordinates
subset_market_towns$lat[subset_market_towns$Market_town == "Skive"] <- 56.56171662979727
subset_market_towns$long[subset_market_towns$Market_town == "Skive"] <- 9.026041387513118

subset_market_towns$lat[subset_market_towns$Market_town == "Middelfart"] <- 55.49697028894646
subset_market_towns$long[subset_market_towns$Market_town == "Middelfart"] <- 9.746609766176393

# keep df
subset_market_towns_df <- subset_market_towns

# Convert the data frame to an sf object
subset_market_towns_sf <- st_as_sf(subset_market_towns, coords = c("long", "lat"), crs = 4326)

### roughly check
plot(outline_dk$geometry)
plot(subset_market_towns_sf$geometry, add = T, col = "blue")


### Make data frame a Spatial points df
coordinates(subset_market_towns) <- ~long+lat
proj4string(subset_market_towns) <- CRS("+proj=longlat +datum=WGS84")



# Define market town pairs
town_pairs <- matrix(c("Copenhagen", "Roskilde",
                       "Roskilde", "Korsoer", 
                       "Roskilde", "Koege", 
                       "Roskilde", "Holbaek", 
                       "Holbaek", "Kalundborg", 
                       "Copenhagen", "Helsingoer", 
                       "Koege", "Naestved", 
                       "Nyborg", "Odense", 
                       "Aarhus", "Randers", 
                       "Randers", "Aalborg",
                       "Aarhus", "Horsens",
                       "Horsens", "Vejle",
                       "Vejle", "Fredericia",
                       "Esbjerg", "Kolding",
                       "Fredericia", "Kolding",
                       "Odense", "Middelfart",
                       "Naestved", "Masnedsund",
                       "Norresundby", "Frederikshavn",
                       "Aarhus", "Skive",
                       "Esbjerg", "Ringkobing",
                       "Struer", "Ringkobing",
                       "Struer", "Skive"),
                     ncol = 2, byrow = TRUE)

# === CRITICAL SLOPE VALUE: MEDIAN ===

# Initialize paths list
paths <- list()

# Loop through each town pair to calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  
  # Create dynamic name for each path
  path_name <- paste(start_town, end_town, sep = "_")
  
  # Calculate shortest path
  paths[[path_name]] <- st_as_sf(shortestPath(
    transitions, 
    coordinates(subset_market_towns[subset_market_towns$Market_town == start_town, ]), 
    coordinates(subset_market_towns[subset_market_towns$Market_town == end_town, ]), 
    output = "SpatialLines"
  ))
}

# Add an identifier to each sf object in the list
names(paths) <- gsub("paths\\$", "", names(paths))

for (name in names(paths)) {
  paths[[name]]$route <- name
}

all_paths <- bind_rows(paths)
all_paths <- st_set_crs(all_paths, 4326)

# Save the sf object as a shapefile
#st_write(all_paths, "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_median.shp", driver = "ESRI Shapefile")

# === CRITICAL SLOPE VALUE: 2 ===

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  
  # Create dynamic name for each path
  path_name <- paste(start_town, end_town, sep = "_")
  
  # Calculate shortest path
  paths[[path_name]] <- st_as_sf(shortestPath(
    transitions_scrit2, ### Changed transitions
    coordinates(subset_market_towns[subset_market_towns$Market_town == start_town, ]), 
    coordinates(subset_market_towns[subset_market_towns$Market_town == end_town, ]), 
    output = "SpatialLines"
  ))
}

# Add an identifier to each sf object in the list
names(paths) <- gsub("paths\\$", "", names(paths))

for (name in names(paths)) {
  paths[[name]]$route <- name
}

all_paths <- bind_rows(paths)
all_paths <- st_set_crs(all_paths, 4326)

# Save the sf object as a shapefile
#st_write(all_paths, "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit2.shp", driver = "ESRI Shapefile")

# === CRITICAL SLOPE VALUE: 3 ===

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  
  # Create dynamic name for each path
  path_name <- paste(start_town, end_town, sep = "_")
  
  # Calculate shortest path
  paths[[path_name]] <- st_as_sf(shortestPath(
    transitions_scrit3, ### Changed transitions
    coordinates(subset_market_towns[subset_market_towns$Market_town == start_town, ]), 
    coordinates(subset_market_towns[subset_market_towns$Market_town == end_town, ]), 
    output = "SpatialLines"
  ))
}

# Add an identifier to each sf object in the list
names(paths) <- gsub("paths\\$", "", names(paths))

for (name in names(paths)) {
  paths[[name]]$route <- name
}

all_paths <- bind_rows(paths)
all_paths <- st_set_crs(all_paths, 4326)

# Save the sf object as a shapefile
#st_write(all_paths, "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit3.shp", driver = "ESRI Shapefile")

# === CRITICAL SLOPE VALUE: 4 ===

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  
  # Create dynamic name for each path
  path_name <- paste(start_town, end_town, sep = "_")
  
  # Calculate shortest path
  paths[[path_name]] <- st_as_sf(shortestPath(
    transitions_scrit4, ### Changed transitions
    coordinates(subset_market_towns[subset_market_towns$Market_town == start_town, ]), 
    coordinates(subset_market_towns[subset_market_towns$Market_town == end_town, ]), 
    output = "SpatialLines"
  ))
}

# Add an identifier to each sf object in the list
names(paths) <- gsub("paths\\$", "", names(paths))

for (name in names(paths)) {
  paths[[name]]$route <- name
}

all_paths <- bind_rows(paths)
all_paths <- st_set_crs(all_paths, 4326)

# Save the sf object as a shapefile
#st_write(all_paths, "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit4.shp", driver = "ESRI Shapefile")

# === CRITICAL SLOPE VALUE: 5 ===

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  
  # Create dynamic name for each path
  path_name <- paste(start_town, end_town, sep = "_")
  
  # Calculate shortest path
  paths[[path_name]] <- st_as_sf(shortestPath(
    transitions_scrit5, ### Changed transitions
    coordinates(subset_market_towns[subset_market_towns$Market_town == start_town, ]), 
    coordinates(subset_market_towns[subset_market_towns$Market_town == end_town, ]), 
    output = "SpatialLines"
  ))
}

# Add an identifier to each sf object in the list
names(paths) <- gsub("paths\\$", "", names(paths))

for (name in names(paths)) {
  paths[[name]]$route <- name
}

all_paths <- bind_rows(paths)
all_paths <- st_set_crs(all_paths, 4326)

# Save the sf object as a shapefile
#st_write(all_paths, "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit5.shp", driver = "ESRI Shapefile")


# === CRITICAL SLOPE VALUE: 6 ===

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  
  # Create dynamic name for each path
  path_name <- paste(start_town, end_town, sep = "_")
  
  # Calculate shortest path
  paths[[path_name]] <- st_as_sf(shortestPath(
    transitions_scrit6, ### Changed transitions
    coordinates(subset_market_towns[subset_market_towns$Market_town == start_town, ]), 
    coordinates(subset_market_towns[subset_market_towns$Market_town == end_town, ]), 
    output = "SpatialLines"
  ))
}

# Add an identifier to each sf object in the list
names(paths) <- gsub("paths\\$", "", names(paths))

for (name in names(paths)) {
  paths[[name]]$route <- name
}

all_paths <- bind_rows(paths)
all_paths <- st_set_crs(all_paths, 4326)

# Save the sf object as a shapefile
#st_write(all_paths, "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit6.shp", driver = "ESRI Shapefile")

# === CRITICAL SLOPE VALUE: 7 ===

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  
  # Create dynamic name for each path
  path_name <- paste(start_town, end_town, sep = "_")
  
  # Calculate shortest path
  paths[[path_name]] <- st_as_sf(shortestPath(
    transitions_scrit7, ### Changed transitions
    coordinates(subset_market_towns[subset_market_towns$Market_town == start_town, ]), 
    coordinates(subset_market_towns[subset_market_towns$Market_town == end_town, ]), 
    output = "SpatialLines"
  ))
}

# Add an identifier to each sf object in the list
names(paths) <- gsub("paths\\$", "", names(paths))

for (name in names(paths)) {
  paths[[name]]$route <- name
}

all_paths <- bind_rows(paths)
all_paths <- st_set_crs(all_paths, 4326)

# Save the sf object as a shapefile
#st_write(all_paths, "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit7.shp", driver = "ESRI Shapefile")

# === CRITICAL SLOPE VALUE: 8 ===

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  
  # Create dynamic name for each path
  path_name <- paste(start_town, end_town, sep = "_")
  
  # Calculate shortest path
  paths[[path_name]] <- st_as_sf(shortestPath(
    transitions_scrit8, ### Changed transitions
    coordinates(subset_market_towns[subset_market_towns$Market_town == start_town, ]), 
    coordinates(subset_market_towns[subset_market_towns$Market_town == end_town, ]), 
    output = "SpatialLines"
  ))
}

# Add an identifier to each sf object in the list
names(paths) <- gsub("paths\\$", "", names(paths))

for (name in names(paths)) {
  paths[[name]]$route <- name
}

all_paths <- bind_rows(paths)
all_paths <- st_set_crs(all_paths, 4326)

# Save the sf object as a shapefile
#st_write(all_paths, "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit8.shp", driver = "ESRI Shapefile")




# === RMSE: Find shape file with the best fit ===


### Load shape files (Actual railway and hypothetical networks)
real <- st_read("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Data/Raw/Railroads/Fertner 2012/jernbane_historisk.shp") %>% st_transform(crs = 32632)
hypo1 <- st_read("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_median.shp") %>% st_transform(crs = 32632)
hypo2 <- st_read("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit2.shp") %>% st_transform(crs = 32632)
hypo3 <- st_read("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit3.shp") %>% st_transform(crs = 32632)
hypo4 <- st_read("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit4.shp") %>% st_transform(crs = 32632)
hypo5 <- st_read("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit5.shp") %>% st_transform(crs = 32632)
hypo6 <- st_read("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit6.shp") %>% st_transform(crs = 32632)
hypo7 <- st_read("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit7.shp") %>% st_transform(crs = 32632)
hypo8 <- st_read("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Shape files/IV/LCP_scrit8.shp") %>% st_transform(crs = 32632)


### Subset
real <- real[real$opened <= 1875, ]

calculate_rmse <- function(hypo_network, real_network, spacing) {
  # Transform to sf object
  hypo_network <- st_as_sf(hypo_network)
  # Calculate line length and number of points
  line_length <- as.numeric(st_length(hypo_network))
  num_points <- ceiling(line_length / spacing)
  # Sample points
  hypo_points <- st_sample(hypo_network, num_points, type = "regular")
  # Calculate distances
  distances <- st_distance(hypo_points, real_network)
  min_distances <- apply(distances, 1, min)
  # Calculate and return RMSE
  sqrt(mean(min_distances^2))
}

rmse1 <- calculate_rmse(hypo1, real, 100)
rmse2 <- calculate_rmse(hypo2, real, 100)
rmse3 <- calculate_rmse(hypo3, real, 100)
rmse4 <- calculate_rmse(hypo4, real, 100)
rmse5 <- calculate_rmse(hypo5, real, 100)
rmse6 <- calculate_rmse(hypo6, real, 100)
rmse7 <- calculate_rmse(hypo7, real, 100)
rmse8 <- calculate_rmse(hypo8, real, 100)


rmse_values <- c(rmse1, rmse2, rmse3, rmse4, rmse5, rmse6, rmse7, rmse8)
names(rmse_values) <- c("hypo1", "hypo2", "hypo3", "hypo4", "hypo5", "hypo6", "hypo7", "hypo8")

# Find the hypothetical network with the smallest RMSE
best_fit <- which.min(rmse_values)
print(paste("Best fit is:", names(rmse_values)[best_fit]))


plot(real$geometry)
plot(hypo2, add = T, col = "blue")



# === Plot preparations ===

# Convert the raster to a data frame
slope_df <- as.data.frame(rasterToPoints(slope_raster), stringsAsFactors = FALSE)

# Rename columns for clarity
colnames(slope_df) <- c("long", "lat", "slope")

# Applying a log transformation (for illustration purposes only)
slope_df$slope_log <- log1p(slope_df$slope)  # log1p to avoid log(0)

# === Plot ===

# Plot slope raster with log-transformed slope values
plot_gg <- ggplot() +
  geom_tile(data = slope_df, aes(x = long, y = lat, fill = slope_log)) +
  scale_fill_gradient(low = "grey95", high = "black") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white")) +
  guides(fill = "none")  # This line removes the legend for slope values


# Adding cities (names)
plot_gg <- plot_gg + geom_point(data = subset_market_towns_df, aes(x = long, y = lat), color = "black", size = 2, shape = 4) + geom_text_repel(data = subset_market_towns_df, aes(x = long, y = lat, label = Market_town), size = 2.5)

# Adding railway lines opened <= 1875
plot_gg <- plot_gg + geom_sf(data = subset(shape_data, opened <= 1875), size = 0.5, inherit.aes = FALSE, alpha = 1)

# Adding LCP('s)
plot_gg <- plot_gg + geom_sf(data = hypo2, size = 1, inherit.aes = FALSE, col = "firebrick1")

# Adding LCP('s)
#plot_gg <- plot_gg + geom_sf(data = hypo5, size = 1, inherit.aes = FALSE, col = "dodgerblue")

# Change legend position
#plot_gg <- plot_gg + theme(legend.position = c(0.75, 0.7), legend.text = element_text(size = 12), legend.title = element_text(size = 14), legend.key.size = unit(4, "mm"))


# Plot
plot_gg


#####
################################################################################
# Save plot
#ggsave(filename = "C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Results/Figures/LCP_IV.png", plot = plot_gg, width = 8, dpi = 600)
################################################################################




















