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
    mutate(id = file_id) %>% 
    filter(X1 != "") %>% 
    pivot_wider(names_from = X1, values_from = X2, values_fn = ~paste(.x, collapse = ";")) %>% 
    mutate_all(as.character)
  
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
  
  all_tables = map_df(rds_files, ~read_and_modify_rds(.x, pb))
  
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

# ==== Main ====
data0 = load_all_tables()
metadata0 = load_all_meta()

data1 = data0 %>% 
  left_join(metadata0, by = "id")

# Assertion to test join
test_join = assertthat::assert_that(NROW(data1) == NROW(data0))

# Save tmp data
saveRDS(data1, "../Data not redistributable/Tmp_data/Tmp_picture_data.rds")
