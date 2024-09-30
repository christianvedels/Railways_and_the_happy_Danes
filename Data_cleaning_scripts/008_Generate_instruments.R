# Railways: Generate instruments
#
# Date updated:   2023-11-17
# Auhtor:         Tom Görges
# Purpose:        This script takes slope information and returns shape files
#                 of predicted railways based on least cost paths

# ==== Libraries ====
library(sf)
library(ggplot2)
library(elevatr)
library(viridis)
library(raster)
library(gdistance)
library(sp)
library(ggrepel)
library(tidygeocoder)
library(readr)
library(dplyr)


# ==== Load data (Railway shape data and Outline of Denmark) ====
shape_data <- st_read("../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp") %>% st_transform(4326)
outline_dk <- st_read("../Data not redistributable/Outline DK/DNK_adm0.shp")  %>% st_transform(4326)


# Obtain elevation raster (from OpenStreetMap)
denmark_elev <- get_elev_raster(outline_dk, z = 9, source = "osm", clip = "locations") # z(oom) = 9 also used by package "movecost"
plot(denmark_elev)

# Create slope raster: %
slope_raster <- terrain(denmark_elev, opt='slope', unit='tangent')*100

plot(slope_raster)


# ==== Create and save transitions ====

# critical slope value
# median_slope <- median(values(slope_raster), na.rm = T)

# s_crit <- median_slope

# base cost + slope (Herzog and Costaz-Fernandez) + geocorrection
# Maybe increase directions = 16 if computational power permits

### Create cost surface / transition matrix (changing critical slope value)
# transitions <- transition(slope_raster, transitionFunction = function(x) (1 + (x / s_crit)^2) , directions = 8) %>% geoCorrection(type="c")
# transitions_scrit2 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 2)^2) , directions = 8) %>% geoCorrection(type="c")
# transitions_scrit3 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 3)^2) , directions = 8) %>% geoCorrection(type="c")
# transitions_scrit4 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 4)^2) , directions = 8) %>% geoCorrection(type="c")
# transitions_scrit5 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 5)^2) , directions = 8) %>% geoCorrection(type="c")
# transitions_scrit6 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 6)^2) , directions = 8) %>% geoCorrection(type="c")
# transitions_scrit7 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 7)^2) , directions = 8) %>% geoCorrection(type="c")
# transitions_scrit8 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 8)^2) , directions = 8) %>% geoCorrection(type="c")
# transitions_scrit12 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 12)^2) , directions = 8) %>% geoCorrection(type="c")
# transitions_scrit16 <- transition(slope_raster, transitionFunction = function(x) (1 + (x / 16)^2) , directions = 8) %>% geoCorrection(type="c")


### Save cost surfaces
# save(transitions, file = "../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_median.RData")
# save(transitions_scrit2, file = "../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit2.RData")
# save(transitions_scrit3, file = "../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit3.RData")
# save(transitions_scrit4, file = "../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit4.RData")
# save(transitions_scrit5, file = "../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit5.RData")
# save(transitions_scrit6, file = "../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit6.RData")
# save(transitions_scrit7, file = "../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit7.RData")
# save(transitions_scrit8, file = "../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit8.RData")
# save(transitions_scrit12, file = "../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit12.RData")
# save(transitions_scrit16, file = "../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit16.RData")



# === Load Transitions ====
load("../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_median.RData") # 1 + (s/median)^2 Costaz-Fernandet et al. 2020
load("../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit2.RData") # 1 + (s/2)^2
load("../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit3.RData") # 1 + (s/3)^2
load("../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit4.RData") # 1 + (s/4)^2
load("../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit5.RData") # 1 + (s/5)^2
load("../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit6.RData") # 1 + (s/6)^2
load("../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit7.RData") # 1 + (s/7)^2
load("../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit8.RData") # 1 + (s/8)^2: Herzog (2016), lower bound
load("../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit12.RData") # 1 + (s/12)^2: Herzog (2016)
load("../Railways_and_the_happy_Danes/Data/lcp_transitions/transitions_scrit16.RData") # 1 + (s/16)^2: Herzog (2016), upper bound



# === Nodes / Market towns ====


# Reading in market towns
market_towns <- read_delim("../Railways_and_the_happy_Danes/Data/Market_towns.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


# Calculate the median of the Pop1801 column
median_pop1801 <- median(market_towns$Pop1801, na.rm = TRUE)

# Subset the data frame, sort and select
subset_market_towns <- subset(market_towns, Pop1801 > median_pop1801) %>%
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
padborg <- data.frame(Market_town = "Padborg", Pop1801 = NA)
vojens <- data.frame(Market_town = "Vojens", Pop1801 = NA)
holstebro <- data.frame(Market_town = "Holstebro", Pop1801 = NA)
tinglev <- data.frame(Market_town = "Tinglev", Pop1801 = NA)
rodekro <- data.frame(Market_town = "Rodekro", Pop1801 = NA)
skanderborg <- data.frame(Market_town = "Skanderborg", Pop1801 = NA)
silkeborg <- data.frame(Market_town = "Silkeborg", Pop1801 = NA)
maribo <- data.frame(Market_town = "Maribo", Pop1801 = NA)
bandholm <- data.frame(Market_town = "Bandholm", Pop1801 = NA)
orehoved <- data.frame(Market_town = "Orehoved", Pop1801 = NA)
varde <- data.frame(Market_town = "Varde", Pop1801 = NA)
rodbyhavn <- data.frame(Market_town = "Rodbyhavn", Pop1801 = NA)
grenaa <- data.frame(Market_town = "Grenaa", Pop1801 = NA)
herning <- data.frame(Market_town = "Herning", Pop1801 = NA)
oster_toreby <- data.frame(Market_town = "Oster Toreby", Pop1801 = NA)

subset_market_towns <- rbind(subset_market_towns, esbjerg, skive, struer, ringkobing, middelfart, frederikshavn, vordingborg, norresundby,
                             padborg, vojens, holstebro, tinglev, rodekro, skanderborg, silkeborg, maribo, bandholm, orehoved, varde, rodbyhavn, grenaa, herning, oster_toreby)


### Get coordinates

### Change names for OpenStreetMap
subset_market_towns$Market_town[subset_market_towns$Market_town == "Koebenhavn"] <- "Copenhagen"
subset_market_towns$Market_town[subset_market_towns$Market_town == "Soenderborg"] <- "Sonderborg" 
subset_market_towns$Market_town[subset_market_towns$Market_town == "Aeroeskoebing"] <- "Ærøskøbing"
subset_market_towns$Market_town[subset_market_towns$Market_town == "Neksoe"] <- "Nexø"
subset_market_towns$Market_town[subset_market_towns$Market_town == "Rudkoebing"] <- "Rudkøbing"
subset_market_towns$Market_town[subset_market_towns$Market_town == "Roenne"] <- "Rønne"

# Geocode (Open Street Map)
subset_market_towns <- subset_market_towns %>% geocode(Market_town, method = 'osm')

# correct coordinates
subset_market_towns$lat[subset_market_towns$Market_town == "Skive"] <- 56.56171662979727
subset_market_towns$long[subset_market_towns$Market_town == "Skive"] <- 9.026041387513118

subset_market_towns$lat[subset_market_towns$Market_town == "Middelfart"] <- 55.49697028894646
subset_market_towns$long[subset_market_towns$Market_town == "Middelfart"] <- 9.746609766176393

subset_market_towns$lat[subset_market_towns$Market_town == "Toender"] <- 54.93333684818414
subset_market_towns$long[subset_market_towns$Market_town == "Toender"] <- 8.860980954906205

subset_market_towns$lat[subset_market_towns$Market_town == "Holstebro"] <- 56.3590324
subset_market_towns$long[subset_market_towns$Market_town == "Holstebro"] <- 8.6159188


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



# Define market town pairs (routes) node to node
town_pairs <- matrix(c("Copenhagen", "Roskilde",
                       "Roskilde", "Korsoer",
                       "Aarhus", "Randers",
                       "Aarhus", "Viborg",
                       "Copenhagen", "Helsingoer",
                       "Padborg", "Vojens",
                       "Nyborg", "Odense", 
                       "Odense", "Middelfart",
                       "Viborg", "Skive",
                       "Struer", "Skive",
                       "Vojens", "Haderslev",
                       "Vojens", "Kolding",
                       "Fredericia", "Kolding",
                       "Struer", "Holstebro",
                       "Toender", "Tinglev",
                       "Aabenraa", "Rodekro",
                       "Vejle", "Fredericia",
                       "Horsens", "Vejle",
                       "Horsens", "Skanderborg",
                       "Skanderborg", "Aarhus",
                       "Randers", "Aalborg",
                       "Maribo", "Bandholm", # oldest private line
                       "Roskilde", "Koege", 
                       "Koege", "Naestved", 
                       "Naestved", "Masnedsund",
                       "Norresundby", "Frederikshavn",
                       "Skanderborg", "Silkeborg",
                       "Orehoved", "Nykoebing Falster",
                       "Roskilde", "Holbaek", 
                       "Holbaek", "Kalundborg",
                       "Esbjerg", "Kolding",
                       "Esbjerg", "Varde",
                       "Oster Toreby", "Nakskov",
                       "Maribo", "Rodbyhavn",
                       "Varde", "Ringkobing",
                       "Ringkobing", "Holstebro",
                       "Ribe", "Esbjerg",
                       "Randers", "Grenaa",
                       "Odense", "Svendborg",
                       "Grenaa", "Aarhus",
                       "Silkeborg", "Herning"),
                     ncol = 2, byrow = TRUE)

# === CRITICAL SLOPE VALUE: MEDIAN ====

# Initialize paths list
paths <- list()

# Loop through each town pair to calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  cat("Processing:", start_town, "to", end_town, "\n")
  
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
# Add year when section opened
all_paths$opened <- c(1847, 1856, 1862, 1863, 1864, 1864, 1865, 1865, 1865, 1865, 1866, 1866, 1866, 1866, 1867, 1868, 1868, 1868, 1868, 1868, 1869, 1869, 1870, 1870, 1870, 1871, 1871, 1872,
                      1874, 1874, 1874, 1874, 1874, 1874, 1875, 1875, 1875, 1876, 1876, 1877, 1877)


# Save the sf object as a shapefile
#st_write(all_paths, "../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_median.shp", driver = "ESRI Shapefile")

# === CRITICAL SLOPE VALUE: 2 ====

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  cat("Processing:", start_town, "to", end_town, "\n")
  
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
# Add year when section opened
all_paths$opened <- c(1847, 1856, 1862, 1863, 1864, 1864, 1865, 1865, 1865, 1865, 1866, 1866, 1866, 1866, 1867, 1868, 1868, 1868, 1868, 1868, 1869, 1869, 1870, 1870, 1870, 1871, 1871, 1872,
                      1874, 1874, 1874, 1874, 1874, 1874, 1875, 1875, 1875, 1876, 1876, 1877, 1877)

# Save the sf object as a shapefile
#st_write(all_paths, "../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit2.shp", driver = "ESRI Shapefile")

# === CRITICAL SLOPE VALUE: 3 ====

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  cat("Processing:", start_town, "to", end_town, "\n")
  
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
# Add year when section opened
all_paths$opened <- c(1847, 1856, 1862, 1863, 1864, 1864, 1865, 1865, 1865, 1865, 1866, 1866, 1866, 1866, 1867, 1868, 1868, 1868, 1868, 1868, 1869, 1869, 1870, 1870, 1870, 1871, 1871, 1872,
                      1874, 1874, 1874, 1874, 1874, 1874, 1875, 1875, 1875, 1876, 1876, 1877, 1877)

# Save the sf object as a shapefile
#st_write(all_paths, "../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit3.shp", driver = "ESRI Shapefile")

# === CRITICAL SLOPE VALUE: 4 ====

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  cat("Processing:", start_town, "to", end_town, "\n")
  
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
# Add year when section opened
all_paths$opened <- c(1847, 1856, 1862, 1863, 1864, 1864, 1865, 1865, 1865, 1865, 1866, 1866, 1866, 1866, 1867, 1868, 1868, 1868, 1868, 1868, 1869, 1869, 1870, 1870, 1870, 1871, 1871, 1872,
                      1874, 1874, 1874, 1874, 1874, 1874, 1875, 1875, 1875, 1876, 1876, 1877, 1877)

# Save the sf object as a shapefile
#st_write(all_paths, "../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit4.shp", driver = "ESRI Shapefile")

# === CRITICAL SLOPE VALUE: 5 ====

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  cat("Processing:", start_town, "to", end_town, "\n")
  
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
# Add year when section opened
all_paths$opened <- c(1847, 1856, 1862, 1863, 1864, 1864, 1865, 1865, 1865, 1865, 1866, 1866, 1866, 1866, 1867, 1868, 1868, 1868, 1868, 1868, 1869, 1869, 1870, 1870, 1870, 1871, 1871, 1872,
                      1874, 1874, 1874, 1874, 1874, 1874, 1875, 1875, 1875, 1876, 1876, 1877, 1877)

# Save the sf object as a shapefile
#st_write(all_paths, "../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit5.shp", driver = "ESRI Shapefile")


# === CRITICAL SLOPE VALUE: 6 ====

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  cat("Processing:", start_town, "to", end_town, "\n")
  
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
# Add year when section opened
all_paths$opened <- c(1847, 1856, 1862, 1863, 1864, 1864, 1865, 1865, 1865, 1865, 1866, 1866, 1866, 1866, 1867, 1868, 1868, 1868, 1868, 1868, 1869, 1869, 1870, 1870, 1870, 1871, 1871, 1872,
                      1874, 1874, 1874, 1874, 1874, 1874, 1875, 1875, 1875, 1876, 1876, 1877, 1877)

# Save the sf object as a shapefile
#st_write(all_paths, "../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit6.shp", driver = "ESRI Shapefile")

# === CRITICAL SLOPE VALUE: 7 ====

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  cat("Processing:", start_town, "to", end_town, "\n")
  
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
# Add year when section opened
all_paths$opened <- c(1847, 1856, 1862, 1863, 1864, 1864, 1865, 1865, 1865, 1865, 1866, 1866, 1866, 1866, 1867, 1868, 1868, 1868, 1868, 1868, 1869, 1869, 1870, 1870, 1870, 1871, 1871, 1872,
                      1874, 1874, 1874, 1874, 1874, 1874, 1875, 1875, 1875, 1876, 1876, 1877, 1877)

# Save the sf object as a shapefile
#st_write(all_paths, "../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit7.shp", driver = "ESRI Shapefile")

# === CRITICAL SLOPE VALUE: 8 ====

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  cat("Processing:", start_town, "to", end_town, "\n")
  
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
# Add year when section opened
all_paths$opened <- c(1847, 1856, 1862, 1863, 1864, 1864, 1865, 1865, 1865, 1865, 1866, 1866, 1866, 1866, 1867, 1868, 1868, 1868, 1868, 1868, 1869, 1869, 1870, 1870, 1870, 1871, 1871, 1872,
                      1874, 1874, 1874, 1874, 1874, 1874, 1875, 1875, 1875, 1876, 1876, 1877, 1877)

# Save the sf object as a shapefile
#st_write(all_paths, "../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit8.shp", driver = "ESRI Shapefile")

# === CRITICAL SLOPE VALUE: 12 ====

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate LCP
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  cat("Processing:", start_town, "to", end_town, "\n")
  
  # Create dynamic name for each path
  path_name <- paste(start_town, end_town, sep = "_")
  
  # Calculate shortest path
  paths[[path_name]] <- st_as_sf(shortestPath(
    transitions_scrit12, ### Changed transitions
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

# Add year when section opened
all_paths$opened <- c(1847, 1856, 1862, 1863, 1864, 1864, 1865, 1865, 1865, 1865, 1866, 1866, 1866, 1866, 1867, 1868, 1868, 1868, 1868, 1868, 1869, 1869, 1870, 1870, 1870, 1871, 1871, 1872,
                      1874, 1874, 1874, 1874, 1874, 1874, 1875, 1875, 1875, 1876, 1876, 1877, 1877)

# Save the sf object as a shapefile
#st_write(all_paths, "../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit12.shp", driver = "ESRI Shapefile")



# === CRITICAL SLOPE VALUE: 16 ====

# Initialize empty paths list
paths <- list()

# Loop through each town pair and calculate paths
for (i in 1:nrow(town_pairs)) {
  start_town <- town_pairs[i, 1]
  end_town <- town_pairs[i, 2]
  cat("Processing:", start_town, "to", end_town, "\n")
  
  # Create dynamic name for each path
  path_name <- paste(start_town, end_town, sep = "_")
  
  # Calculate shortest path
  paths[[path_name]] <- st_as_sf(shortestPath(
    transitions_scrit16, ### Changed transitions
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
# Add year when section opened
all_paths$opened <- c(1847, 1856, 1862, 1863, 1864, 1864, 1865, 1865, 1865, 1865, 1866, 1866, 1866, 1866, 1867, 1868, 1868, 1868, 1868, 1868, 1869, 1869, 1870, 1870, 1870, 1871, 1871, 1872,
                      1874, 1874, 1874, 1874, 1874, 1874, 1875, 1875, 1875, 1876, 1876, 1877, 1877)

# Save the sf object as a shapefile
#st_write(all_paths, "../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit16.shp", driver = "ESRI Shapefile")




