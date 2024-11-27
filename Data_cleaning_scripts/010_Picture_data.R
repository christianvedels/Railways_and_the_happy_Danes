# Cleaning picture data to be regression ready
#
# Date updated:   2024-10-16
# Author:         Tom Görges, Christian Vedel
# Purpose:        Cleans up arhival picture data


# ==== Libraries ====
library(tidyverse)
library(sf)
library(foreach)
library(progress)
source("Picture_data_cleaning/000_Functions.R")

# ==== Setup ====
progress_bar_format = "[:bar] :elapsedfull -- :current of :total -- :percent eta: :eta"

# ==== Read data ====
# Read aps data
df = read_rds("../Data not redistributable/Tmp_data/Tmp_picture_data.rds")
railways = read_csv2("Data/Panel_of_railways_in_parishes.csv")
geo = read_csv2("Data/Geo_info.csv", guess_max = 2000)

# ==== Fix midpoint year ====
df = df %>% 
  filter(midpoint_year > 1800)

# ==== Railway data ====
first_treated_year = function(Year, Connected_rail){
  valid_years = Year[Connected_rail == 1]
  if(length(valid_years)==0){
    return(0)
  }
  
  min_year = min(valid_years)
  
  res = Year*Connected_rail
  
  res = ifelse(all(res==0), 0, min(res[res != 0]))
  
  return(res)
}

treatment_disapear = function(Year, Connected_rail){
  valid_years = Year[Connected_rail == 1]
  if(length(valid_years)==0){
    return(0)
  }
  
  max_year = max(valid_years)
  
  if(max_year == max(Year)){
    return(0)
  }
  
  res = max_year
  
  return(res)
}

# Expand railway data backwards (with no railways)
railways = expand.grid(
  GIS_ID = unique(railways$GIS_ID),
  Year = 1800:2020
) %>% 
  left_join(
    railways, by = c("GIS_ID", "Year")
  ) %>% 
  mutate(
    Connected_rail = replace_na(Connected_rail, 0)
  )

# Treatment vars
railways = railways %>% 
  arrange(GIS_ID, Year) %>% 
  group_by(GIS_ID) %>% 
  mutate(
    rail_opened = first_treated_year(Year, Connected_rail),
    rail_closed = treatment_disapear(Year, Connected_rail)
  ) %>% 
  mutate(
    treat_time_opened = ifelse(rail_opened != 0, rail_opened - midpoint_year, 0),
    treat_time_closed = ifelse(rail_closed != 0, rail_closed - midpoint_year, 0)
  )

# ==== Parish-level ====
# Clean_it
clean_it = function(x, grouping_vars){
  x %>% 
    drop_na(coords_long) %>%  # Only pictures with exact coordinates
    filter(`Face Detected` == "Yes") %>% 
    group_by(across(all_of(grouping_vars))) %>% 
    summarise(
      across(
        emotion_score_1:emotion_score_neutral,
        ~ mean(.x, na.rm = TRUE)
      ),
      n_people = n(),
      n_pictures = length(unique(Image))
    ) %>% ungroup()
}

# Exact locaiton
parish_data = df %>% clean_it(grouping_vars = c("midpoint_year", "GIS_ID"))
  
# Approximate location (coord not in shape)
parish_data_approx = df %>% clean_it(grouping_vars = c("midpoint_year", "GIS_ID_w_closest"))

# Sanity check
sanity_check = function(x){
  x %>% 
    filter(midpoint_year>=1800) %>% 
    ggplot(aes(n_people, n_pictures, col = midpoint_year)) + 
    geom_point() + 
    geom_abline(intercept = 0, slope = 1, size = 0.5) # 45 deg. line
}

parish_data %>% sanity_check()
parish_data_approx %>% sanity_check()
# Should be under this line - which it is! 

# Imputation
pb = progress_bar$new(
  format = progress_bar_format,
  total = length(unique(parish_data_approx$GIS_ID_w_closest))
)
parish_data_approx = foreach(GIS_ID = unique(parish_data_approx$GIS_ID_w_closest), .combine = "bind_rows") %do% {
  # pb$tick()
  if(NROW(parish_data_approx)==1){ # Skip on obs cases
    return(NULL)
  }
  parish_data_approx %>% 
    filter(GIS_ID_w_closest == GIS_ID) %>% 
    impute_it(1800, 2020) %>% 
    mutate(GIS_ID_w_closest = GIS_ID)
}

parish_data_approx = parish_data_approx %>% 
  left_join(railways, by = c("GIS_ID_w_closest"="GIS_ID", "midpoint_year"="Year")) %>% 
  mutate(
    Connected_rail = ifelse(is.na(Connected_rail), 0, Connected_rail)
  )

# ==== Individual reg data ====
reg_data_indiv = df %>% # TODO: Move creation of regdata in sepperate script
  mutate(
    midpoint_year = floor(midpoint_year)
  ) %>% 
  left_join(railways, by = c("GIS_ID_w_closest"="GIS_ID", "midpoint_year"="Year")) %>% 
  # filter(`Face Detected` == "Yes") %>% 
  mutate(
    approximate_coordiate = is.na(coords_long) # If shape, then this is NA
  ) %>% 
  select(
    Image,
    Detected,
    midpoint_year,
    GIS_ID_w_closest,
    Connected_rail, Distance_to_nearest_railway,
    emotion_score_angry:emotion_score_neutral,
    long, lat,
    decade,
    rail_opened,
    rail_closed,
    treat_time_opened,
    treat_time_closed,
    approximate_coordiate
  ) %>% 
  filter(midpoint_year>=1800)

# ==== Number of pictures =====
number_of_pictures = df %>% 
  mutate(
    midpoint_year = floor(midpoint_year)
  ) %>% 
  drop_na(coords_long) %>% # Only point data
  filter(midpoint_year>=1800) %>% 
  group_by(midpoint_year, GIS_ID_w_closest) %>% 
  summarise(
    n_objects = n(),
    n_pictures = length(unique(Image))
  )

# Fill in with zeros
number_of_pictures = expand.grid(
  midpoint_year = min(number_of_pictures$midpoint_year):max(number_of_pictures$midpoint_year),
  GIS_ID_w_closest = unique(geo$GIS_ID)
) %>% 
  left_join(
    number_of_pictures,
    by = c("GIS_ID_w_closest", "midpoint_year")
  ) %>% 
  mutate(
    n_objects = replace_na(n_objects, 0),
    n_pictures = replace_na(n_pictures, 0)
  )

number_of_pictures = number_of_pictures %>% 
  full_join(railways, by = c("GIS_ID_w_closest"="GIS_ID", "midpoint_year"="Year")) %>% 
  mutate(
    Connected_rail = ifelse(is.na(Connected_rail), 0, Connected_rail)
  )

# ==== Save data ====
reg_data_indiv %>% # TODO: Move to public repo
  write_csv2("../Data not redistributable/Picture_regression_data_indiv.csv")

parish_data_approx  %>% 
  write_csv2("Data/Picture_regression_data_parish.csv")

number_of_pictures  %>% 
  write_csv2("Data/Picture_regression_data_number_of_pictures.csv")
   

