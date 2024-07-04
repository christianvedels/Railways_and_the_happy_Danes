# Data cleaning
#
# Date updated:   2024-07-04
# Author:         Tom Görges, Christian Vedel
# Purpose:        Clean data from arkiv.dk

# ==== Libraries ====
library(tidyverse)
source("Picture_data_cleaning/000_Functions.R")

# ==== Read data ====
data0 = read_rds("../Data not redistributable/Tmp_data/Tmp_picture_data.rds")

# ==== Get rid of Scandinavian letters in col names ====
names(data0) = sub_scandi(names(data0))

# ==== Var types ====
data0 = data0 %>% 
  mutate(
    placeholder_image = as.logical(placeholder_image)
  )

# ==== Export placeholder image data ====
data0 %>% 
  select(id, placeholder_image) %>% 
  write_csv("Data/Placeholder_images.csv")

# ==== Subset data for rows where placeholder_image is FALSE ====
data0 = data0 %>% 
  filter(!placeholder_image)

# === DATE INFO: Lowest / Highest / Midpoint ====

# Step 1: Extracting year information and preparing the data
data0 = data0 %>%
  mutate(
    Aarstal_numeric = as.numeric(Aarstal),
    Year_from_Period = as.numeric(str_extract(Periode, "^\\d+")),
    Year_to_Period = as.numeric(str_extract(Periode, "\\d+$")),
    Year_from_Dateringsnote = as.numeric(str_extract(Dateringsnote, "\\d{4}"))
  ) %>%
  rowwise() %>%
  mutate(
    lowest_year = min(c(Aarstal_numeric, Year_from_Period, Year_from_Dateringsnote), na.rm = TRUE),
    highest_year = max(c(Aarstal_numeric, Year_to_Period, Year_from_Dateringsnote), na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2: Calculating midpoint
data0 = data0 %>%
  mutate(
    midpoint_year = (lowest_year + highest_year) / 2
  )

# Handle exceptions:
data0 = data0 %>% 
  mutate( # Handle cases where lowest_year or highest_year could not be calculated
    midpoint_year = ifelse(is.na(lowest_year) | is.na(highest_year), NA, midpoint_year)
  ) %>% 
  mutate( # Handle outliers
    midpoint_year = ifelse(midpoint_year > 2024, NA, midpoint_year)
  )

# === Extract city names ===

# First, normalize spacing in the Arkiv column by replacing multiple consecutive spaces with a single space
data0 = data0 %>%
  mutate(Arkiv = gsub("\\s+", " ", Arkiv))  # Normalize spaces: replace multiple consecutive spaces with a single space

# Define a vector of keywords to remove, ordered by specificity (most specific first)
keywords_to_remove = c(
  "Byhistorisk Samling og Arkiv i",
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
  "og Omegn",
  # "and surroundings"
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
data0 = data0 %>% 
  mutate(
    location = Arkiv
  )

# Iteratively remove each keyword from the location column
for (keyword in keywords_to_remove) {
  data0 = data0 %>% 
    rowwise() %>% 
    mutate(
      location = trimws(gsub(keyword, "", location))
    )
}


# === Handle cases like: Thyborøn-Harboøre-Engbjerg

# Split the string by the dash and select the first location
data0$location = as.character(data0$location)
data0$location = sapply(strsplit(data0$location, "-"), `[`, 1)

# Some additional cleaning based on specific cases (looked at Google Maps)
data0 = data0 %>%
  mutate(
    location = ifelse(Arkiv == "Byarkivet - Horsens", "Horsens", location),
    location = ifelse(Arkiv == "Æ Fjandbo Arkiv", "Stoholm", location),
    location = ifelse(Arkiv == "SIFA Idrætshistorisk Samling", "Aalborg", location),
    location = ifelse(Arkiv == "Nørhald Egns-Arkiv", "Gjerlev", location),
    location = ifelse(Arkiv == "Højeregnens Lokalhistoriske Arkiv", "Hoyer", location),
    location = ifelse(Arkiv == "Sønderhald Egnsarkiv", "Oster Alling", location),
    location = ifelse(Arkiv == "Lokalhist. Arkiv for Frejlev, Nørholm og Sønderholm sogne", "Nibe", location),
    location = ifelse(Arkiv == "Forstadsmuseet Historiens Huse, Hvidovre Lokalarkiv", "Hvidovre", location),
    location = ifelse(Arkiv == "Egvad Egnshistoriske Samling", "Egvad", location),
    location = ifelse(Arkiv == "Tingsted og Systofte Sognes Lokalhistoriske Arkiv", "Tingsted", location),
    location = ifelse(Arkiv == "Museum Sønderjyllands Mediearkiv", "Rødekro", location),
    location = ifelse(Arkiv == "Lokalhistorisk Arkiv for Åes, Gangsted og Søvind", "Horsens", location),
    location = ifelse(Arkiv == "Lokalhistorisk Arkiv for Fraugde, Allerup, Davinde og Tornbjerg Sogne", "Fraugde", location),
    location = ifelse(Arkiv == "Industrimuseet Frederiks Værk, Arkivet", "Frederiksværk", location),
    location = ifelse(Arkiv == "Lokalhistorisk Arkiv for Gislev, Kværndrup og Ryslinge Sogne", "Ryslinge", location),
    location = ifelse(Arkiv == "Bøvling Sognehistorisk Forening og Arkiv", "Bovlingbjerg", location)
  )

# ==== Save ====
data0 = data0 %>% # Column order
  select(
    id,
    Arkiv,
    location,
    Aarstal,
    Dateringsnote,
    Periode,
    Arkiv,
    lowest_year,
    highest_year,
    midpoint_year,
    everything()
  )

saveRDS(data0, "../Data not redistributable/Tmp_data/Tmp_picture_data.rds")
