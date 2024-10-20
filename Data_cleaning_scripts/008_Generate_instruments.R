# Railways: Generate instruments
#
# Date updated:   2024-10-14
# Auhtor:         Tom Görges
# Purpose:        This script takes slope information and returns shape files
#                 of predicted railways based on least cost paths (Updated Version)


# ==== Libraries ====
library(sf)
library(tidyverse)
library(elevatr)
library(viridis)
library(raster)
library(gdistance)
library(sp)
library(ggrepel)
library(tidygeocoder)

library(leastcostpath)
library(terra)

# ==== Parameters ====
# Define the range of crit_slope values
crit_slope_values = c(1:16)

# ==== Load data (Railway shape data and Outline of Denmark) ====
shape_data = st_read("../Data not redistributable/Railways Fertner/jernbane_historisk_v050413/jernbane_historisk.shp") %>% st_transform(4326)
outline_dk = st_read("../Data not redistributable/Outline DK/DNK_adm0.shp") %>% st_transform(4326)

# Reading in market towns
market_towns = read_delim("Data/Market_towns.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Obtain elevation raster (from OpenStreetMap)
denmark_elev = get_elev_raster(outline_dk, z = 9, source = "osm", clip = "locations") # z(oom) = 9 used by package "movecost", probably need zoom = 10 or higher but my computer breaks down at this resolution

# ==== Plot elev ====
plot(denmark_elev)
dnk = as(denmark_elev, "SpatRaster")
plot(dnk, main="Original Raster")

###################
# === % Slope === # Dont need the following part just playing around and sanity checks
###################

# # Calculate slope in radians - percentage %
# dnk_slope <- terrain(dnk, "slope", unit = "radians")

# plot(dnk_slope)
# max(dnk_slope$slope)


# # Apply the tangent function and convert to percentage
# dnk_slope_perc <- app(dnk_slope, function(x) tan(x) * 100)

# plot(dnk_slope_perc, main="Slope Raster")
# max(dnk_slope_perc$lyr.1)


# # Store Median and Mean slope values
# slopeValues <- values(dnk_slope_perc, na.rm = T)
# summary(slopeValues)
# mean_slope <- mean(slopeValues)
# median_slope <- median(slopeValues)

#############################################################################################
# === Create slope cost surface using pre-installed wheeled transport function (Herzog) === #
#############################################################################################

# Loop over the range of crit_slope values
for (crit_slope in crit_slope_values) {
  cat("crit_slope:", crit_slope, "\n")
  
  # Create slope cost surface with the current crit_slope
  slope_cs = create_slope_cs(
    dnk,
    cost_function = "wheeled transport",
    neighbours = 8,
    crit_slope = crit_slope, # Use current crit_slope value
    max_slope = NULL,
    exaggeration = FALSE
  )
  
  # Plot the resulting slope cost surface
  plot(slope_cs)
  
  # Construct the file name dynamically based on the crit_slope value
  slope_label = crit_slope
  
  # Construct the file path dynamically based on the crit_slope value
  file_path = paste0(
    "../Data not redistributable/Instrument_shapes/lcp_slope_cost_surfaces/slope_cs_crit_",
    slope_label,
    ".rds"
  )
  
  # Save the 'slope_cs' object to a .rds file with the current crit_slope in the filename
  write_rds(slope_cs, file_path)
}



############################
# === Some more checks === #
############################


# Customize own cost function as sanity check ---------------------------------------
# crit_slope <- median_slope

# slope_cs2 <- create_slope_cs(
# dnk,
# cost_function = function(x) {1 / (1 + ((x*100)/crit_slope)^2)},
# neighbours = 8,
# max_slope = NULL,
# exaggeration = FALSE
# )

# plot(slope_cs2)


# ---------------------------------------------------------------------------------------
# ==== Load slope cost surfaces ==== #

# Loop to load slope_cs_1 to slope_cs_16
for (i in crit_slope_values) {
  
  # Construct the file path for each crit_slope value
  file_path = paste0(
    "../Data not redistributable/Instrument_shapes/lcp_slope_cost_surfaces/slope_cs_crit_",
    i,
    ".rds"
  )
  
  # Dynamically assign the loaded rds file to a variable named slope_cs_1, slope_cs_2, etc.
  assign(paste0("slope_cs_", i), read_rds(file_path))
}


# ----------------------------------------------------------------------------------------------------------

# === Nodes / Market towns ====
# Calculate the median of the Pop1801 column
median_pop1801 = median(market_towns$Pop1801, na.rm = TRUE)

# Subset the data frame, sort and select
subset_market_towns = market_towns %>% 
  filter(
    Pop1801 > median_pop1801
  ) %>% 
  dplyr::select(Market_town, Pop1801 )%>% 
  mutate(
    node_type = "Above median 1801"
  )

### Add nodes
additional_nodes = list( # TODO: Get rid of most of these
  esbjerg = data.frame(Market_town = "Esbjerg", Pop1801 = NA),
  struer = data.frame(Market_town = "Struer", Pop1801 = NA),
  skive = data.frame(Market_town = "Skive", Pop1801 = NA),
  ringkobing = data.frame(Market_town = "Ringkobing", Pop1801 = NA),
  middelfart = data.frame(Market_town = "Middelfart", Pop1801 = NA),
  frederikshavn = data.frame(Market_town = "Frederikshavn", Pop1801 = NA),
  vordingborg = data.frame(Market_town = "Masnedsund", Pop1801 = NA),
  norresundby = data.frame(Market_town = "Norresundby", Pop1801 = NA),
  padborg = data.frame(Market_town = "Padborg", Pop1801 = NA),
  vojens = data.frame(Market_town = "Vojens", Pop1801 = NA),
  holstebro = data.frame(Market_town = "Holstebro", Pop1801 = NA),
  tinglev = data.frame(Market_town = "Tinglev", Pop1801 = NA),
  rodekro = data.frame(Market_town = "Rodekro", Pop1801 = NA),
  skanderborg = data.frame(Market_town = "Skanderborg", Pop1801 = NA),
  silkeborg = data.frame(Market_town = "Silkeborg", Pop1801 = NA),
  maribo = data.frame(Market_town = "Maribo", Pop1801 = NA),
  bandholm = data.frame(Market_town = "Bandholm", Pop1801 = NA),
  orehoved = data.frame(Market_town = "Orehoved", Pop1801 = NA),
  varde = data.frame(Market_town = "Varde", Pop1801 = NA),
  rodbyhavn = data.frame(Market_town = "Rodbyhavn", Pop1801 = NA),
  grenaa = data.frame(Market_town = "Grenaa", Pop1801 = NA),
  herning = data.frame(Market_town = "Herning", Pop1801 = NA),
  oster_toreby = data.frame(Market_town = "Oster Toreby", Pop1801 = NA)
) %>% do.call("rbind", .) %>% 
  mutate(
    node_type = "Additional nodes"
  )


subset_market_towns = subset_market_towns %>% bind_rows(additional_nodes)


### Get coordinates

### Change names for OpenStreetMap
subset_market_towns$Market_town[subset_market_towns$Market_town == "Koebenhavn"] = "Copenhagen"
subset_market_towns$Market_town[subset_market_towns$Market_town == "Soenderborg"] = "Sonderborg" 
subset_market_towns$Market_town[subset_market_towns$Market_town == "Aeroeskoebing"] = "Ærøskøbing"
subset_market_towns$Market_town[subset_market_towns$Market_town == "Neksoe"] = "Nexø"
subset_market_towns$Market_town[subset_market_towns$Market_town == "Rudkoebing"] = "Rudkøbing"
subset_market_towns$Market_town[subset_market_towns$Market_town == "Roenne"] = "Rønne"

# Geocode (Open Street Map)
subset_market_towns_geo = subset_market_towns %>% 
  geocode( # TODO: Coordinates can be found in 
    Market_town, 
    method = 'osm', 
    full_results = TRUE, 
    custom_query = list(countrycodes = 'dk')
  ) %>%
  dplyr::select(all_of(colnames(subset_market_towns)), lat, long, display_name)

# correct coordinates
subset_market_towns_geo$lat[subset_market_towns_geo$Market_town == "Toender"] = 54.93333684818414
subset_market_towns_geo$long[subset_market_towns_geo$Market_town == "Toender"] = 8.860980954906205

# keep df
subset_market_towns_df = subset_market_towns_geo

# Convert the data frame to an sf object
subset_market_towns_sf = st_as_sf(subset_market_towns_geo, coords = c("long", "lat"), crs = 4326)

### roughly check
plot(outline_dk$geometry)
plot(subset_market_towns_sf$geometry, add = T, col = "blue")


### Make data frame a Spatial points df
coordinates(subset_market_towns_geo) = ~long+lat
proj4string(subset_market_towns_geo) = CRS("+proj=longlat +datum=WGS84")



# Define market town pairs (routes) node to node
town_pairs = matrix(c("Copenhagen", "Roskilde",
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
                     ncol = 2, byrow = TRUE) %>% 
  data.frame() %>% 
  mutate(
    id = 1:n()
  )

# TODO: Derive "Data/Opened_pairs.csv" from data here


# Define a function to calculate LCP between two towns for any cost surface
calculate_lcp = function(cost_surface, town1_coords, town2_coords) {
  lcp = create_lcp(cost_surface, town1_coords, town2_coords, cost_distance = FALSE, check_locations = FALSE)
  return(lcp)
}

# Create an empty list to store the LCPs for each cost surface as sf objects
lcp_sf_all_cost_surfaces = list()

# Check if the dataset is an sf object and extract coordinates accordingly
if (inherits(subset_market_towns_sf, "sf")) {
  coords = st_coordinates(subset_market_towns_sf)
  subset_market_towns_geo = cbind(subset_market_towns_sf, coords)
}

# Create a list of all cost surfaces
cost_surfaces = list() # HERE

# Add cost surfaces for slope_cs_1 to slope_cs_16
for (i in crit_slope_values) {
  cost_surfaces[[as.character(i)]] <- get(paste0("slope_cs_", i))
}

# Iterate over each cost surface
for (slope_label in names(cost_surfaces)) {
  # Get the current cost surface
  slope_cs = cost_surfaces[[slope_label]]

  # Create an empty list to store the LCPs for this particular cost surface
  lcp_sf_list = list()

  # Iterate over each town pair and calculate LCPs
  for (i in 1:nrow(town_pairs)) {
    town1_name = town_pairs[i, 1]
    town2_name = town_pairs[i, 2]

    # Get the coordinates of each town from the market towns dataset
    town1_coords = subset_market_towns_geo[subset_market_towns_geo$Market_town == town1_name, c("X", "Y")]
    town2_coords = subset_market_towns_geo[subset_market_towns_geo$Market_town == town2_name, c("X", "Y")]

    # Ensure there are valid coordinates
    if (nrow(town1_coords) > 0 & nrow(town2_coords) > 0) {
      # Calculate the least cost path
      lcp = calculate_lcp(slope_cs, town1_coords, town2_coords)

      # Convert LCP to an sf object and store in the list
      lcp_sf = st_as_sf(lcp)

      # Create a new column for start and end town names
      lcp_sf = lcp_sf %>% 
        mutate(
          town_pair = paste0(town1_name, "_", town2_name),
          town1_coords = town1_coords %>% st_drop_geometry(),
          town2_coords = town1_coords %>% st_drop_geometry(),
          town_pair_id = town_pairs$id[i]
        )

      # Store the LCP with the town pair names
      lcp_sf_list[[paste0(town1_name, "_", town2_name)]] = lcp_sf
    } else {
      message(paste("Coordinates for", town1_name, "or", town2_name, "not found. Skipping..."))
    }
  }
  
  # Combine all LCPs for the current cost surface into a single sf object
  all_lcps_sf = do.call("bind_rows", lcp_sf_list)
  
  # Add the "opened" column
  opened = read_csv("Data/Opened_pairs.csv") %>% dplyr::select(town_pair_id, opened)
  # Test of 'opened' has expected dims
  test1 = !all(sort(all_lcps_sf$town_pair_id) == sort(opened$town_pair_id))
  test2 = !(NROW(opened) == NROW(all_lcps_sf))
  if(test1 | test2){
    stop("'Data/Opened_pairs.csv' did not have expected content")
  }
  
  # Join opened info
  all_lcps_sf = all_lcps_sf %>% 
    full_join(opened, by = "town_pair_id")
    
  
  # Store the result in the overall list, keyed by the cost surface label
  lcp_sf_all_cost_surfaces[[slope_label]] = all_lcps_sf
  
  # Plot the LCPs for this cost surface
  plot(st_geometry(outline_dk), main = paste("Least Cost Paths for Cost Surface:", slope_label))
  plot(st_geometry(all_lcps_sf), add = TRUE, col = "blue", lwd = 2)
}

# Optional: Save the LCPs for each cost surface with town_pair in shapefile
for (slope_label in names(lcp_sf_all_cost_surfaces)) {
  st_write(lcp_sf_all_cost_surfaces[[slope_label]], 
           paste0("../Data not redistributable/Instrument_shapes/lcp_shape_files/LCP_scrit_", slope_label, ".shp"), 
           driver = "ESRI Shapefile",
           append = F) # replace existing file
}


test = st_read("../Data not redistributable/Instrument_shapes/lcp_shape_files/LCP_scrit_10.shp")
plot(test$geometry)


