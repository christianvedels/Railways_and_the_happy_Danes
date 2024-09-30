# Load picture data
#
# Date updated:   2024-07-04
# Author:         Tom Görges, Christian Vedel
# Purpose:        Load up data from arkiv.dk


# ==== Libraries ====
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(progress)
library(jsonlite)
library(sf)
source("Picture_data_cleaning/000_Functions.R")

# ==== Setup ====
progress_bar_format = "[:bar] :elapsedfull -- :current of :total -- :percent eta: :eta"

# ==== read_and_modify_rds() + load_all_tables() ====
# Define a function to read each RDS file, add a custom row, and return the modified table
read_and_modify_rds = function(file_path, pb) {
  # Read the RDS file
  df = readRDS(file_path)
  
  # Extract the file ID from the file path
  file_id = tools::file_path_sans_ext(basename(file_path))
  
  # Detect error meta data:
  if(any(names(df) %in% c("Cookieudbyder", "Navn", "Type", "Varighed"))){
    pb$tick()
    return(data.frame())
  }
  
  if("list" %in% class(df)){
    pb$tick()
    return(data.frame())
  }
  
  df = df %>% 
    mutate(id = file_id)
  
  pb$tick()
  
  # Combine the new row with the original data frame
  return(df)
}

# load_all_tables(): Wrapper function to load all
load_all_tables = function(folder_path = "../Data not redistributable/Arkiv.dk/Tables", toyload = FALSE) {
  # Folder path: Path containing all the .rds tables
  # toyload: Should only some files be loaded? Useful for development
  
  # List all .rds files in the folder
  rds_files = list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
  
  if(toyload){
    # Then only load the first 50
    rds_files = rds_files[1:50]
  }
  
  # Initialize progress bar
  pb = progress_bar$new(
    total = length(rds_files),
    format = paste0("Loading tables: ", progress_bar_format)
  )
  
  all_tables = map_df(rds_files, ~read_and_modify_rds(.x, pb)) %>% 
    filter(X1 != "") %>% 
    pivot_wider(names_from = X1, values_from = X2, values_fn = ~paste(.x, collapse = ";")) %>% 
    mutate_all(as.character)
  
  return(all_tables)  
}

# === Merge with Meta data and subset for real pictures ===

# load_all_meta(): Function to load all Meta data
load_all_meta = function(folder_path = "../Data not redistributable/Arkiv.dk/Metadata") {
  # List all .rds files in the folder
  rds_files = list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
  
  # Initialize progress bar
  pb = progress_bar$new(
    total = length(rds_files),
    format = paste0("Loading metadata: ", progress_bar_format)
  )
  
  # Load all meta .rds files and combine them into one long dataframe
  all_meta = map_df(rds_files, function(file) {
    pb$tick()
    readRDS(file)
  })
  
  return(all_meta)  
}

# ==== Load geo data ====
load_all_geo = function(folder_path = "../Data not redistributable/Arkiv.dk/Geodata/") {
  # List all .rds files in the folder
  rds_files = list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
  
  # Initialize progress bar
  pb = progress_bar$new(
    total = length(rds_files),
    format = paste0("Loading geodata: ", progress_bar_format)
  )
  
  read_one = function(f){
    pb$tick()
    x = read_rds(f)
    
    # Convert polygon JSON string to an sf object
    if (!is.null(x$polygon) && x$polygon != "") {
      polygon_data = fromJSON(x$polygon)
      coords = polygon_data$features$geometry$coordinates[[1]]
      
      
      if(class(coords) == "list"){
        polygon_list = lapply(coords, function(polygon) {
          long = polygon[,,1]
          lat = polygon[,,2]
          cbind(long, lat)
        })
      } else {
        polygon_list = list(drop(coords))
      }
      
      
      polygon_sf = st_multipolygon(list(polygon_list))
      centroid = st_centroid(polygon_sf)
      
    } else {
      polygon_sf = NA
      centroid = c(NA, NA)
    }
    
    # Create an sf object for the point if it exists
    if (!is.na(x$coords) && x$coords != "") {
      coords_split = strsplit(x$coords, ", ")[[1]]
      point_sf = st_point(c(as.numeric(coords_split[1]), as.numeric(coords_split[2])))
    } else {
      point_sf = NA
    }
    
    # Combine into a single sf object with a geometry column
    data.frame(
      centroid_long = centroid[1],
      centroid_lat = centroid[2],
      coords_long = point_sf[1],
      coords_lat = point_sf[2],
      id = gsub(folder_path, "", f) %>% gsub(".rds", "", .)
    )
  }
  
  # Load all geo .rds files and combine them into one long dataframe
  all_geo = map_df(rds_files, function(x) read_one(x))
  
  return(all_geo)  
}


# ==== Main ====
data0 = load_all_tables()
metadata0 = load_all_meta()
geo0 = load_all_geo()

data1 = data0 %>%
  left_join(metadata0, by = "id") %>%
  left_join(geo0, by = "id")

# Assertion to test join
test_join = assertthat::assert_that(NROW(data1) == NROW(data0))

# Save tmp data
saveRDS(data1, "../Data not redistributable/Tmp_data/Tmp_picture_data.rds")

# ==== Quick summary stats for sanity checks ====
# geo0 = load_all_geo()
geo0 %>%
  ggplot(aes(centroid_long, centroid_lat)) + geom_point(alpha = 0.1) +
  theme_bw()

geo0 %>%
  filter(coords_long > 7) %>%
  filter(coords_long < 20) %>%
  filter(coords_lat > 52) %>%
  ggplot(aes(coords_long, coords_lat)) + geom_point(alpha = 0.1) +
  theme_bw()

geo0 %>%
  drop_na(coords_long) %>%
  count()

geo0 %>%
  drop_na(centroid_long) %>%
  count()

data1 %>%
  drop_na(Årstal) %>% 
  mutate(
    decade = round0(as.numeric(Årstal), 10)
  ) %>% 
  group_by(decade) %>% 
  filter(decade>1850) %>% 
  count() %>% knitr::kable()

data1 %>%
  drop_na(Årstal) %>% 
  mutate(
    decade = round0(as.numeric(Årstal), 10)
  ) %>% 
  filter(decade>1800) %>% 
  filter(coords_long > 7) %>%
  filter(coords_long < 20) %>%
  filter(coords_lat > 52) %>%
  ggplot(aes(coords_long, coords_lat)) + geom_point(alpha = 0.1) +
  theme_bw() + 
  facet_wrap(~decade)
  


