# Instruments to panel data
#
# Date updated:   2023-11-17
# Auhtor:         Tom Görges
# Purpose:        Reads shape files of predicted railways based on 
#                 least cost paths


# ==== Libraries ====
library(tidyverse)
library(foreach)
library(ggspatial)
library(sf)
library(permute)

source("000_Functions.R") # Contains calc_rail()

# ==== Load data ====
# Load shape files
shape_parishes = read_sf("Data/sogne_shape/sogne.shp")

shapes <- list(st_read("../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_median.shp") %>% st_transform(crs = 32632),
               st_read("../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit2.shp") %>% st_transform(crs = 32632),
               st_read("../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit3.shp") %>% st_transform(crs = 32632),
               st_read("../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit4.shp") %>% st_transform(crs = 32632),
               st_read("../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit5.shp") %>% st_transform(crs = 32632),
               st_read("../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit6.shp") %>% st_transform(crs = 32632),
               st_read("../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit7.shp") %>% st_transform(crs = 32632),
               st_read("../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit8.shp") %>% st_transform(crs = 32632),
               st_read("../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit12.shp") %>% st_transform(crs = 32632),
               st_read("../Railways_and_the_happy_Danes/Data/lcp_shape_files/LCP_scrit16.shp") %>% st_transform(crs = 32632)
               )

### Add variable fake close? necessary for function?
for(i in seq_along(shapes)) {
  shapes[[i]]$fake_close <- 2020
}


# ==== Run it ====
for(i in 1:length(shapes)){
  shape_i = shapes[[i]] # access list elements
  
  railways_panel_i = calc_rail(
    shape_i,
    shape_parishes,
    verbose = TRUE,
    plots = TRUE,
    id = paste0("predicted_rail_paramS",i),
    years = 1846:1877
  )
  
  railways_panel_i %>% 
    write_csv2(paste0("Data/Instruments/paramS",i, ".csv"))
  
  cat("\n===> FINISHED",i)
}


# === Load and merge instruments + finding instrment with best fit (RMSE) ===

# Read files
real_rail <-read_delim("../Railways_and_the_happy_Danes/Data/Panel_of_railways_in_parishes.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(Year < 1878) # filter because intstrument only up to 1877 (for now?)

z_median <- read_delim("../Railways_and_the_happy_Danes/Data/Instruments/paramS1.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(Connected_rail_median = Connected_rail)  %>% 
  rename(Distance_to_nearest_railway_median = Distance_to_nearest_railway)

z_scrit2 <- read_delim("../Railways_and_the_happy_Danes/Data/Instruments/paramS2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(Connected_rail_scrit2 = Connected_rail) %>% 
  rename(Distance_to_nearest_railway_scrit2 = Distance_to_nearest_railway)

z_scrit3 <- read_delim("../Railways_and_the_happy_Danes/Data/Instruments/paramS2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(Connected_rail_scrit3 = Connected_rail) %>% 
  rename(Distance_to_nearest_railway_scrit3 = Distance_to_nearest_railway)

z_scrit4 <- read_delim("../Railways_and_the_happy_Danes/Data/Instruments/paramS2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(Connected_rail_scrit4 = Connected_rail) %>% 
  rename(Distance_to_nearest_railway_scrit4 = Distance_to_nearest_railway)

z_scrit5 <- read_delim("../Railways_and_the_happy_Danes/Data/Instruments/paramS2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(Connected_rail_scrit5 = Connected_rail) %>% 
  rename(Distance_to_nearest_railway_scrit5 = Distance_to_nearest_railway)

z_scrit6 <- read_delim("../Railways_and_the_happy_Danes/Data/Instruments/paramS2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(Connected_rail_scrit6 = Connected_rail) %>% 
  rename(Distance_to_nearest_railway_scrit6 = Distance_to_nearest_railway)

z_scrit7 <- read_delim("../Railways_and_the_happy_Danes/Data/Instruments/paramS2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(Connected_rail_scrit7 = Connected_rail) %>% 
  rename(Distance_to_nearest_railway_scrit7 = Distance_to_nearest_railway)

z_scrit8 <- read_delim("../Railways_and_the_happy_Danes/Data/Instruments/paramS2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(Connected_rail_scrit8 = Connected_rail) %>% 
  rename(Distance_to_nearest_railway_scrit8 = Distance_to_nearest_railway)

z_scrit12 <- read_delim("../Railways_and_the_happy_Danes/Data/Instruments/paramS2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(Connected_rail_scrit12 = Connected_rail) %>% 
  rename(Distance_to_nearest_railway_scrit12 = Distance_to_nearest_railway)

z_scrit16 <- read_delim("../Railways_and_the_happy_Danes/Data/Instruments/paramS2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(Connected_rail_scrit16 = Connected_rail) %>% 
  rename(Distance_to_nearest_railway_scrit16 = Distance_to_nearest_railway)

# Merge
z_all <- left_join(real_rail, z_median, by = c("GIS_ID", "Year"))
z_all <- left_join(z_all, z_scrit2, by = c("GIS_ID", "Year"))
z_all <- left_join(z_all, z_scrit3, by = c("GIS_ID", "Year"))
z_all <- left_join(z_all, z_scrit4, by = c("GIS_ID", "Year"))
z_all <- left_join(z_all, z_scrit5, by = c("GIS_ID", "Year"))
z_all <- left_join(z_all, z_scrit6, by = c("GIS_ID", "Year"))
z_all <- left_join(z_all, z_scrit7, by = c("GIS_ID", "Year"))
z_all <- left_join(z_all, z_scrit8, by = c("GIS_ID", "Year"))
z_all <- left_join(z_all, z_scrit12, by = c("GIS_ID", "Year"))
z_all <- left_join(z_all, z_scrit16, by = c("GIS_ID", "Year"))


# Find instrument with least RMSE => Main specification
rmse1 <- sqrt(sum(z_all$Connected_rail - z_all$Connected_rail_median)^2 / nobs(z_all))
rmse2 <- sqrt(sum(z_all$Connected_rail - z_all$Connected_rail_scrit2)^2 / nobs(z_all))
rmse3 <- sqrt(sum(z_all$Connected_rail - z_all$Connected_rail_scrit3)^2 / nobs(z_all))
rmse4 <- sqrt(sum(z_all$Connected_rail - z_all$Connected_rail_scrit4)^2 / nobs(z_all))
rmse5 <- sqrt(sum(z_all$Connected_rail - z_all$Connected_rail_scrit5)^2 / nobs(z_all))
rmse6 <- sqrt(sum(z_all$Connected_rail - z_all$Connected_rail_scrit6)^2 / nobs(z_all))
rmse7 <- sqrt(sum(z_all$Connected_rail - z_all$Connected_rail_scrit7)^2 / nobs(z_all))
rmse8 <- sqrt(sum(z_all$Connected_rail - z_all$Connected_rail_scrit8)^2 / nobs(z_all))
rmse12 <- sqrt(sum(z_all$Connected_rail - z_all$Connected_rail_scrit12)^2 / nobs(z_all))
rmse16 <- sqrt(sum(z_all$Connected_rail - z_all$Connected_rail_scrit16)^2 / nobs(z_all))


# ...

# Plot




















