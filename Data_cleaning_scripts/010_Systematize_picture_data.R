# Systematize scraping results
#
# Date updated:   2024-07-01
# Author:         
# Purpose:        Systematize scraped data from arkiv.dk


# ==== Libraries ====
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)

# ==== Setup ====
# Set the path to the folder containing your .rds files




# ==== read_and_modify_rds() ====
# Define a function to read each RDS file, add a custom row, and return the modified table
read_and_modify_rds = function(file_path) {
  # Read the RDS file
  df = readRDS(file_path)
  
  # Extract the file ID from the file path
  file_id = tools::file_path_sans_ext(basename(file_path))
  
  # Detect error meta data:
  if(any(names(df) %in% c("Cookieudbyder", "Navn", "Type", "Varighed"))){
    return(data.frame())
  }
  
  df = df %>% 
    mutate(id = file_id) %>% 
    filter(X1 != "") %>% 
    pivot_wider(names_from = X1, values_from = X2) %>% 
    mutate_all(as.character)
  
  cat(file_id,"             \r")

  # Combine the new row with the original data frame
  return(df)
}

load_website_data = function(folder_path = "../Data not redistributable/Arkiv.dk/Tables"){
  # List all .rds files in the folder
  rds_files = list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
  all_tables = map_df(rds_files, read_and_modify_rds)

  return(all_tables)  
}

# === Merge with Meta data and subset for real pictures ===

# Load .rds (Meta)

# Set the path to the folder containing your .rds files
folder_path <- "Metadata"

# List all .rds files in the folder
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)

# Load all meta .rds files and combine them into one long dataframe
all_meta <- lapply(rds_files, readRDS) %>% bind_rows()



# ==== Main ====
# Apply the function to each file and combine into one dataframe




# === Export placeholder Images list === ###################


# This will filter and add '.png' to 'id' for rows where placeholder_image is TRUE
placeholder <- all_meta %>%
  filter(placeholder_image == TRUE) %>%
  mutate(id = paste0(id, ".png"))


write.csv(placeholder, "C:/Users/Win7ADM/Dropbox/Data not redistributable/Arkiv.dk/placeholder.csv", row.names = F)
############################################################



# Merge with final.df
final_df$id <- as.numeric(final_df$id)
all_meta$id <- as.numeric(all_meta$id)


final_df <- left_join(final_df, all_meta, by = "id")




# Subset final_df for rows where placeholder_image is FALSE
filtered_df <- final_df %>%
  filter(placeholder_image == FALSE)


# change column order
filtered_df <- filtered_df %>% select(id, Årstal, Dateringsnote, Periode, Arkiv, everything())




# === Lowest / Highest / Midpoint



# Step 1: Extracting year information and preparing the data
filtered_df <- filtered_df %>%
  mutate(
    Årstal_numeric = as.numeric(Årstal),
    Year_from_Period = as.numeric(str_extract(Periode, "^\\d+")),
    Year_to_Period = as.numeric(str_extract(Periode, "\\d+$")),
    Year_from_Dateringsnote = as.numeric(str_extract(Dateringsnote, "\\d{4}"))
  ) %>%
  rowwise() %>%
  mutate(
    lowest_year = min(c(Årstal_numeric, Year_from_Period, Year_from_Dateringsnote), na.rm = TRUE),
    highest_year = max(c(Årstal_numeric, Year_to_Period, Year_from_Dateringsnote), na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2: Calculating midpoint
filtered_df <- filtered_df %>%
  mutate(
    midpoint_year = (lowest_year + highest_year) / 2
  )

# Handle cases where lowest_year or highest_year could not be calculated
filtered_df <- filtered_df %>%
  mutate(
    midpoint_year = ifelse(is.na(lowest_year) | is.na(highest_year), NA, midpoint_year)
  )

# handle outliers
filtered_df$midpoint_year <- ifelse(filtered_df$midpoint_year > 2024, NA, filtered_df$midpoint_year)


# change column order
filtered_df <- filtered_df %>% select(id, Årstal, Dateringsnote, Periode, Arkiv, lowest_year, highest_year, midpoint_year, everything())

# === Extract city names ===

# First, normalize spacing in the Arkiv column by replacing multiple consecutive spaces with a single space
filtered_df <- filtered_df %>%
  mutate(Arkiv = gsub("\\s+", " ", Arkiv))  # Normalize spaces: replace multiple consecutive spaces with a single space

#################################################################################################

# Define a vector of keywords to remove, ordered by specificity (most specific first)
keywords_to_remove <- c("Byhistorisk Samling og Arkiv i",
                        "Arkivet ved Dansk Centralbibliotek for",
                        "Lokalhistorisk Arkiv for den tidl.",
                        "Lokalhistorisk Arkiv og Forening i",
                        "Egnsarkivet f. tidl.",
                        "Lokalhistoriske Forening og Arkiv",
                        "Sognehistorisk Forening og Arkiv",
                        "Sognes Lokalhistoriske Arkiv",
                        "Museum og Lokalhistoriske Arkiv",
                        "Slægts- og Lokalhistorisk Forening",
                        "Forstadsmuseet Historiens Huse",
                        "Historisk Forening for",
                        "Egns- og Byhistoriske",
                        "Egnshistoriske Samling",
                        "Byhistoriske",
                        "Hjemstavnsarkiv",
                        "Arkiv / Byhistorisk Hus",
                        "Stads- og Lokalarkiv",
                        "Historisk Arkiv for",
                        "Historisk Arkiv",
                        "Historiske Arkiv",
                        "Lokalhistorisk Samling",
                        "Lokalhistorisk Forening og Arkiv",
                        "Lokalhistorisk Forening for",
                        "Lokalhistorisk Forening",
                        "Lokalhistoriske Forening",
                        "Lokalhistoriske forening",
                        "Sognehistoriske Forening",
                        "lokalhistorisk forening",
                        "Lokalhistorisk Arkiv for",
                        "Lokalhistorisk arkiv for",
                        "Lokalhist. Arkiv for",
                        "Lokalhistorisk Arkiv i",
                        "Lokalhistorisk Arkiv",
                        "Lokalhistoriske Arkiv",
                        "lokalhistoriske Arkiv",
                        "lokalhistorisk Arkiv",
                        "lokalhistoriske arkiver",
                        "Egnshistoriske Arkiv",
                        "Lokalhistoriske Samling",
                        "Lokalarkivet for",
                        "Lokal Arkiv",
                        "Stadsarkiv",
                        "Folkemindesamling",
                        "Egnsmindesamlingen for",
                        "Egnssamlingen",
                        "Egnsarkivet for",
                        "Egnsarkivet",
                        "Egnsarkiv",
                        "Sognearkiv",
                        "Lokalarkivet",
                        "Lokalarkiv",
                        "Byarkivet",
                        "Byarkiv",
                        "Arkiverne",
                        "Arkiver",
                        "Arkivet",
                        "Arkiv",
                        "arkivet",
                        "arkiv",
                        "Bibliotek",
                        "og Omegns",
                        "og Omegn", # "and surroundings"
                        "og omegn",
                        "& Omegns",
                        "-området",
                        ",",
                        " - ",
                        "Erindringsværksted",
                        "Sogns",
                        "sogn",
                        "Kommune",
                        "Sogn",
                        "Museum",
                        "Nordsjælland",
                        "For",
                        "for",
                        "Blicheregnen",
                        "Industrimuseet",
                        "Lokalhistorisk ening"
                        )

# Initialize the location column as a copy of Arkiv to preserve the original data
filtered_df$location <- filtered_df$Arkiv

# Iteratively remove each keyword from the location column
for (keyword in keywords_to_remove) {
  filtered_df$location <- gsub(keyword, "", filtered_df$location)
  filtered_df$location <- trimws(filtered_df$location) # This trims whitespace from the beginning and end of each string
}


# change column order
filtered_df <- filtered_df %>% select(id, Arkiv, location, Årstal, Dateringsnote, Periode, Arkiv, lowest_year, highest_year, midpoint_year, everything())

# === Handle cases like: Thyborøn-Harboøre-Engbjerg

# Split the string by the dash and select the first location
filtered_df$location <- as.character(filtered_df$location)

filtered_df$location <- sapply(strsplit(filtered_df$location, "-"), `[`, 1)

# Some additional cleaning (Looked at google maps)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Byarkivet - Horsens", "Horsens", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Æ Fjandbo Arkiv", "Stoholm", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "SIFA Idrætshistorisk Samling", "Aalborg", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Nørhald Egns-Arkiv", "Gjerlev", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Højeregnens Lokalhistoriske Arkiv", "Hoyer", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Sønderhald Egnsarkiv", "Oster Alling", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Lokalhist. Arkiv for Frejlev, Nørholm og Sønderholm sogne", "Nibe", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Forstadsmuseet Historiens Huse, Hvidovre Lokalarkiv", "Hvidovre", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Egvad Egnshistoriske Samling", "Egvad", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Tingsted og Systofte Sognes Lokalhistoriske Arkiv", "Tingsted", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Museum Sønderjyllands Mediearkiv", "Rødekro", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Lokalhistorisk Arkiv for Åes, Gangsted og Søvind", "Horsens", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Lokalhistorisk Arkiv for Fraugde, Allerup, Davinde og Tornbjerg Sogne", "Fraugde", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Industrimuseet Frederiks Værk, Arkivet", "Frederiksværk", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Lokalhistorisk Arkiv for Gislev, Kværndrup og Ryslinge Sogne", "Ryslinge", filtered_df$location)
filtered_df$location <- ifelse(filtered_df$Arkiv == "Bøvling Sognehistorisk Forening og Arkiv", "Bovlingbjerg", filtered_df$location)

# === GEOCODING === ###########################
filtered_df <- filtered_df %>% geocode(address = location, method = "osm", custom_query = list(countrycodes = 'dk'))
###############################################

# === Safe === ###############################################
write.csv(filtered_df, "C:/Users/Win7ADM/Dropbox/Data not redistributable/Arkiv.dk/systematized.csv", row.names = F)
##############################################################









