# Clear workspace
rm(list=ls())


# Install and load the required libraries
library(sf)
library(ggplot2)
library(terra)
library(elevatr)
library(viridis)
library(raster)
library(gdistance)
library(sp)
library(ggrepel)
library(rgdal)


# Read railway shape data and the outline of Denmark
shape_data <- st_read("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Data/Raw/Railroads/Fertner 2012/jernbane_historisk.shp")

# Transforming to WGS 84
shape_data <- st_transform(shape_data, 4326)

# Read outline of Denmark
outline_dk <- st_read("C:/Users/Win7ADM/Dropbox/Railways and Mobility in Denmark/Data/Raw/Outline DK/DNK_adm0.shp")

####################
### East Jutland ###############################################################
####################
my_extent <- extent(9, 10.3, 55.9, 57.2)


outline_dk_sp <- as(outline_dk, "Spatial")


outline_dk_cropped <- crop(outline_dk_sp, my_extent)

outline_dk_cropped_sf <- st_as_sf(outline_dk_cropped)

### Obtain elevation data
denmark_elev <- get_elev_raster(outline_dk_cropped_sf, z = 9, source = "aws", clip = "locations")

# Create slope raster
slope_raster <- terrain(denmark_elev, opt='slope', unit='degrees')

# Extract resolution for plotting later
raster_res <- res(denmark_elev)
raster_res_slop <- res(slope_raster)


# Convert the raster to a dataframe for ggplot
denmark_elev_df <- as.data.frame(rasterToPoints(denmark_elev))
colnames(denmark_elev_df) <- c("lng", "lat", "elevation")

slope_raster_df <- as.data.frame(rasterToPoints(slope_raster))
colnames(slope_raster_df) <- c("lng", "lat", "degrees")


plot(denmark_elev)
plot(slope_raster)

###################
### Transitions ###
###################

altDiff <- function(x) 0 + abs( x[2] - x[1] )


# use elevation raster
transitions <- transition(denmark_elev, transitionFunction = altDiff, directions = 8)  # directions = 8 = NSWO + diagonals


# base cost + slope
transitions <- transition(slope_raster, transitionFunction = function(x) x[2], directions = 8)

# exponential function: the steeper the slope, the higher the cost
# when x[2] = 0, cost = 1
# when x[x2] > 0 & < 1 => 1 < x < 2
# when x[2] = 1, cost = 2
# when x[2] = 2, cost = 4
# when x[2] = 3, cost = 2^3 = 8
# no negative values when using slope

transitions <- transition(slope_raster, transitionFunction = function(x) 2^x[2], directions = 8)



# Transition function penalizing both uphill and downhill movements, emphazising the penalty of going uphill
base_cost <- 5

transitions <- transition(slope_raster, 
                          transitionFunction = function(x) {
                            elev_diff = diff(x) # diff(x): subtracts the second from the first input
                            if(elev_diff > 0) {  # going uphill, default = 0
                              return(base_cost + 15 * elev_diff)  
                            } else if(elev_diff < 0) {  # going downhill, default = 0
                              return(base_cost + 1 * abs(elev_diff)) # abs: to ensure positive costs  
                            } else {
                              return(base_cost)  # flat terrain
                            }
                          }, 
                          directions = 16) # 4, 8, 16


### GeoCorrection: The function transition calculates transition values based on the values of adjacent cells in the input raster.
# However, diagonal neighbours are more remote from each other than orthogonal neighbours. Also, on equirectangular (longitude-latitude) grids,
# West-East connections are longer at the equator and become shorter towards the poles, as the meridians approach each other.
# Therefore, the values in the matrix need to be corrected for these two types of distance distortion. Both types of distortion can be corrected
# by dividing each conductance matrix value by the distance between cell centres. This is what function geoCorrection does when type is set to “c”.
transitions <- geoCorrection(transitions, type="c")



### SAVE & LOAD ################################################################

#save(transitions, file = "/Users/tom/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions.RData")
#load("/Users/tom/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions.RData")

################################################################################


### Define Nodes
cities <- list(
  Aarhus = c(10.21070, 56.1572),
  Aalborg = c(9.9187, 57.048),
  Silkeborg = c(9.54508, 56.1697),
  Randers = c(10.038519290275211, 56.46018184602793),
  Hobro = c(9.797268145620746, 56.63920545456215),
  Skive = c(9.02607611629542, 56.56175785341591),
  Skanderborg = c(9.9272, 56.03991),
  Viborg = c(9.399389325657026, 56.45292573295401)
)


# Convert list to data frame
cities_df <- data.frame(
  city_name = names(cities),
  lat = sapply(cities, '[', 2),
  lng = sapply(cities, '[', 1)
)

# Extracting coordinates
get_coords <- function(city_name) {
  return(cities[[city_name]])
}

# Function to create spatial points
lcp <- function(start_city, end_city) {
  start_coords <- get_coords(start_city)
  end_coords <- get_coords(end_city)
  
  start_sp <- SpatialPoints(coords = matrix(start_coords, ncol = 2), proj4string = CRS(projection(denmark_elev)))
  end_sp <- SpatialPoints(coords = matrix(end_coords, ncol = 2), proj4string = CRS(projection(denmark_elev)))
  
  path <- shortestPath(transitions, start_sp, end_sp, output="SpatialLines")
  path_sf <- st_as_sf(path)
  
  st_crs(path_sf) <- st_crs(4326)
  
  list(path=path, path_sf=path_sf)
}


# Initialize an empty list to store the paths
paths <- list()

# Now calculate each LCP and store it
paths$Aarhus_Randers <- lcp("Aarhus", "Randers")
paths$Aalborg_Randers <- lcp("Randers", "Aalborg")
paths$Randers_Skive <- lcp("Aarhus", "Skive")
paths$Aarhus_Viborg <- lcp("Aarhus", "Viborg")



#####################
### VISUALIZATION ###
#####################

# Plot elevation raster
plot_gg <- ggplot() + geom_tile(data = denmark_elev_df, aes(x = lng, y = lat, fill = elevation), width = raster_res[1],  height = raster_res[2]) + scale_fill_gradientn(name = "Elevation (m)", colors = terrain.colors(100), limits = c(0, 200))

################################################################################
# plot slope raster
#plot_gg <- ggplot() + geom_tile(data = slope_raster_df, aes(x = lng, y = lat, fill = degrees), width = raster_res[1],  height = raster_res[2])
################################################################################

# Adding paths
for (path in names(paths)) {
  plot_gg <- plot_gg + geom_sf(data = paths[[path]]$path_sf, color = "red", size = 3, alpha = 0.75, inherit.aes = FALSE)
}

# Adding cities (names)
plot_gg <- plot_gg + geom_point(data = cities_df, aes(x = lng, y = lat), color = "black", size = 2, shape = 4) + geom_text_repel(data = cities_df, aes(x = lng, y = lat, label = city_name), size = 2)


# Plot
plot_gg

################################################################################
# Save plot

#ggsave(filename = "/Users/tom/Dropbox/Railways and Mobility in Denmark/Results/Figures/LCP_4_abs_diff_sqrd.png", plot = plot_gg, width = 8, dpi = 600)
################################################################################



#####################
### Whole Denmark ##############################################################
#####################


# Obtain elevation data for whole denmark and prepare for plotting
denmark_elev <- get_elev_raster(outline_dk, z = 8, source = "aws", clip = "locations") # z(oom) = 8 is quite good, 9 better but slows down

# Create slope raster
slope_raster <- terrain(denmark_elev, opt='slope', unit='degrees')

# Extract resolution for plotting later
raster_res <- res(denmark_elev)

# Convert the raster to a dataframe for ggplot
denmark_elev_df <- as.data.frame(rasterToPoints(denmark_elev))
colnames(denmark_elev_df) <- c("lng", "lat", "elevation")

##########################################################
# Extract resolution for plotting later (slope raster)
raster_res <- res(slope_raster)

# Convert the raster to a dataframe for ggplot
slope_raster_df <- as.data.frame(rasterToPoints(slope_raster))
colnames(slope_raster_df) <- c("lng", "lat", "elevation")
###############################################################

plot(denmark_elev)
plot(slope_raster)


###########################################################
### Transition matrix: Convert elevation / slope raster ###
###########################################################

# use elevation raster
transitions <- transition(denmark_elev, transitionFunction = function(x) 4 + abs(diff(x)), directions = 16) # directtions = 8 = NSWO + diagonals

# use slope raster
transitions <- transition(slope_raster, transitionFunction = function(x) 2 + abs(diff(x)), directions = 16)

# max
transitions <- transition(slope_raster, transitionFunction = function(x) max(x), directions = 8)

# a + b^x
transitions <- transition(slope_raster, transitionFunction = function(x) 2 + 2^x, directions = 8)


# Transition function penalizing both uphill and downhill movements, emphazising the penalty of going uphill
base_cost <- 5

transitions <- transition(slope_raster, 
                          transitionFunction = function(x) {
                            elev_diff = diff(x) # diff(x): subtracts the second from the first input
                            if(elev_diff > 0) {  # going uphill, default = 0
                              return(base_cost + 15 * elev_diff)  
                            } else if(elev_diff < 0) {  # going downhill, default = 0
                              return(base_cost + 1 * abs(elev_diff)) # abs: to ensure positive costs  
                            } else {
                              return(base_cost)  # flat terrain
                            }
                          }, 
                          directions = 16) # 4, 8, 16


### GeoCorrection
transitions <- geoCorrection(transitions, type="c")


### SAVE & LOAD ################################################################

#save(transitions, file = "/Users/tom/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions.RData")
#load("/Users/tom/Dropbox/Railways and Mobility in Denmark/Results/R files/transitions.RData")

################################################################################

# Definitions
cities <- list(
  Struer = c(8.58376, 56.49122),
  Aarhus = c(10.21070, 56.1572),
  Aalborg = c(9.9187, 57.048),
  Skanderborg = c(9.9272, 56.03991),
  Silkeborg = c(9.54508, 56.1697),
  Horsens = c(9.85034, 55.86066),
  Vejle = c(9.5357, 55.70927),
  Fredericia = c(9.748514221400482, 55.56928697129558),
  Viborg = c(9.399389325657026, 56.45292573295401), 
  Copenhagen = c(12.56553, 55.67594),
  Roskilde = c(12.088964469788431, 55.63933283775055),
  Hjørring = c(9.98229, 57.46417),
  Frederikshavn = c(10.53661, 57.44073),
  #Skjern = c(8.492294797211757, 55.94769381784658),
  Nyborg = c(10.797332755756909, 55.3243791504007),
  Varde = c(8.482493665983045, 55.62278987366815),
  Skive = c(9.02607611629542, 56.56175785341591), 
  Holstebro = c(8.619182085061286, 56.362059101212786),
  Middelfart = c(9.746446080170111 ,55.497457375103274),
  Esbjerg = c(8.457946775154161, 55.47696197437741),
  Kalundborg = c(11.089167060412331, 55.67862282816924),
  Helsingor = c(12.61443186838208, 56.03360418164652),
  #Langa = c(9.895358685046986, 56.384879325215266),
  Randers = c(10.038519290275211, 56.46018184602793),
  Hillerod = c(12.311142973922511, 55.92703146790992),
  #Ringsted = c(11.786698802724363, 55.4384609414422), 
  Koge = c(12.186525014917956, 55.458128525855805),
  Naestved = c(11.757972793749381, 55.22454999571345),
  Vordingborg = c(11.908859304623697, 55.00901790449799),
  Ringkobing = c(8.250210858502607, 56.088214936350724),
  Odense = c(10.401948191309815, 55.40379059587304),
  Ribe = c(8.77283022076043, 55.32698316570357),
  Hobro = c(9.797268145620746, 56.63920545456215),
  Norresundby = c(9.922685263770477, 57.05740005806976),
  Kolding = c(9.472776105158626, 55.49598859646194),
  Korsor = c(11.1499994, 55.333332)
)


################################################################################

# Convert list to data frame
cities_df <- data.frame(
  city_name = names(cities),
  lat = sapply(cities, '[', 2),
  lng = sapply(cities, '[', 1)
)

# Extracting coordinates
get_coords <- function(city_name) {
  return(cities[[city_name]])
}

# Function to create spatial points
lcp <- function(start_city, end_city) {
  start_coords <- get_coords(start_city)
  end_coords <- get_coords(end_city)
  
  start_sp <- SpatialPoints(coords = matrix(start_coords, ncol = 2), proj4string = CRS(projection(denmark_elev)))
  end_sp <- SpatialPoints(coords = matrix(end_coords, ncol = 2), proj4string = CRS(projection(denmark_elev)))
  
  path <- shortestPath(transitions, start_sp, end_sp, output="SpatialLines")
  path_sf <- st_as_sf(path)
  
  st_crs(path_sf) <- st_crs(4326)
  
  list(path=path, path_sf=path_sf)
}


# Initialize an empty list to store the paths
paths <- list()

# Now calculate each LCP and store it
paths$Copenhagen_Roskilde <- lcp("Copenhagen", "Roskilde")
#paths$Roskilde_Ringsted <- lcp("Roskilde", "Ringsted")
paths$Korsor_Roskilde <- lcp("Roskilde", "Korsor")
paths$Nyborg_Odense <- lcp("Nyborg", "Odense")
paths$Middelfart_Odense <- lcp("Odense", "Middelfart")
paths$Fredericia_Odense <- lcp("Fredericia", "Vejle")
paths$Skanderborg_Silkeborg <- lcp("Skanderborg", "Silkeborg")
paths$Skanderborg_Horsens <- lcp("Skanderborg", "Horsens")
paths$Skanderborg_Aarhus <- lcp("Skanderborg", "Aarhus")
paths$Horsens_Vejle <- lcp("Horsens", "Vejle")
#paths$Horsens_Aarhus <- lcp("Horsens", "Aarhus")
paths$Randers_Aarhus <- lcp("Aarhus", "Randers")
paths$Viborg_Aarhus <- lcp("Aarhus", "Viborg") 
#paths$Langaa_Viborg <- lcp("Viborg", "Langa")
#paths$Langaa_Randers <- lcp("Randers", "Langa")
paths$Hobro_Randers <- lcp("Randers", "Hobro") 
paths$Hobro_Aalborg <- lcp("Aalborg", "Hobro")
paths$Norresundby_Hjørring <- lcp("Norresundby", "Hjørring")
paths$Hjorring_Frederikshaven <- lcp("Frederikshavn", "Hjørring")
#paths$Norresundby_Frederikshavn <- lcp("Norresundby", "Frederikshavn")
paths$Viborg_Skive <- lcp("Viborg", "Skive")
paths$Struer_Skive <- lcp("Struer", "Skive")
paths$Struer_Holstebro <- lcp("Struer", "Holstebro")
paths$Holstebro_Ringkobing <- lcp("Holstebro", "Ringkobing")
#paths$Skjern_Ringkobing <- lcp("Skjern", "Ringkobing")
#paths$Skjern_Varde <- lcp("Skjern", "Varde")
paths$Ringkobing_Varde <- lcp("Ringkobing", "Varde")
paths$Esbjerg_Varde <- lcp("Esbjerg", "Varde")
paths$Roskilde_Kalundborg <- lcp("Roskilde", "Kalundborg")
paths$Roskilde_Koge <- lcp("Roskilde", "Koge")
paths$Naestved_Koge <- lcp("Naestved", "Koge")
paths$Naestved_Vordingborg <- lcp("Naestved", "Vordingborg")
paths$Copenhagen_Hillerod <- lcp("Copenhagen", "Hillerod")
paths$Helsingor_Hillerod <- lcp("Helsingor", "Hillerod")
paths$Esbjerg_Ribe <- lcp("Esbjerg", "Ribe")
paths$Esbjerg_Kolding <- lcp("Esbjerg", "Kolding")
paths$Fredericia_Kolding <- lcp("Fredericia", "Kolding")


#####################
### VISUALIZATION ###
#####################

# Plot elevation raster
plot_gg <- ggplot() + geom_tile(data = denmark_elev_df, aes(x = lng, y = lat, fill = elevation), width = raster_res[1],  height = raster_res[2]) + scale_fill_gradientn(name = "Elevation (m)", colors = terrain.colors(100), limits = c(0, 200))

################################################################################
# plot slope raster
#plot_gg <- ggplot() + geom_tile(data = slope_raster_df, aes(x = lng, y = lat, fill = elevation), width = raster_res[1],  height = raster_res[2])
################################################################################

# Adding paths
for (path in names(paths)) {
  plot_gg <- plot_gg + geom_sf(data = paths[[path]]$path_sf, color = "red", size = 3, alpha = 1, inherit.aes = FALSE)
}

# Adding cities (names)
plot_gg <- plot_gg + geom_point(data = cities_df, aes(x = lng, y = lat), color = "black", size = 2, shape = 4) + geom_text_repel(data = cities_df, aes(x = lng, y = lat, label = city_name), size = 2)

# Adding railway lines opened <= 1875
plot_gg <- plot_gg + geom_sf(data = subset(shape_data, opened <= 1875), color = "black", size = 0.5, inherit.aes = FALSE, alpha = 1)


# Change legend position
plot_gg <- plot_gg + theme(legend.position = c(0.9, 0.75), legend.text = element_text(size = 12), legend.title = element_text(size = 14), legend.key.size = unit(4, "mm"))

# Plot
plot_gg

################################################################################
# Save plot

#ggsave(filename = "/Users/tom/Dropbox/Railways and Mobility in Denmark/Results/Figures/LCP_4_abs_diff_sqrd.png", plot = plot_gg, width = 8, dpi = 600)
################################################################################

##################################
### Create and Store Shapefile #################################################
##################################


# Extract all 'path_sf' objects from paths list, add 'id' column, and bind them together
lcp_sf <- do.call(rbind, lapply(names(paths), function(id) {
  path_sf <- paths[[id]]$path_sf
  path_sf$id <- id
  return(path_sf)
}))

### Change CRS
lcp_sf <- st_transform(lcp_sf, 25832)


# Buffer 2.5km
lcp_sf_buff <- st_buffer(lcp_sf, dist = 2000)

############
### PLOT ###
############
outline_dk <- st_transform(outline_dk, 25832)


# Plot
plot_gg <- ggplot() +
  geom_sf(data = outline_dk, fill = "grey90", color = "grey90") +   # Plot the outline of Denmark
  geom_sf(data = lcp_sf, size = 1.5, color = "blue") +
  geom_sf(data = lcp_sf_buff, color = "blue", size = 1.5, alpha = 0.5) +
  geom_sf(data = subset(shape_data, opened <= 1875), color = "black", size = 0.5, alpha = 1) +
  theme_void()

plot_gg

# To save the plot, uncomment the line below:
#ggsave(filename = "/Users/tom/Dropbox/Railways and Mobility in Denmark/Results/Figures/IV_buff.png", plot = plot_gg, width = 10, dpi = 600)








