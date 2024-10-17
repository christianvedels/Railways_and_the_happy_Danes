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
    )
}

# Exact locaiton
parish_data = df %>% clean_it(grouping_vars = c("decade", "GIS_ID"))
  
# Approximate location (coord not in shape)
parish_data_approx = df %>% clean_it(grouping_vars = c("decade", "GIS_ID_w_closest"))

# Sanity check
sanity_check = function(x){
  x %>% 
    filter(decade>=1800) %>% 
    ggplot(aes(n_people, n_pictures, col = decade)) + 
    geom_point() + 
    geom_abline(intercept = 0, slope = 1, size = 0.5) # 45 deg. line
}

parish_data %>% sanity_check()
parish_data_approx %>% sanity_check()
# Should be under this line - which it is! 

# ==== Individual reg data ====
reg_data_indiv = df %>% 
  mutate(
    midpoint_year = floor(midpoint_year)
  ) %>% 
  left_join(railways, by = c("GIS_ID_w_closest"="GIS_ID", "midpoint_year"="Year")) %>% 
  filter(`Face Detected` == "Yes") %>% 
  drop_na(coords_long) %>% # Only point data
  select(
    Image,
    Connected_rail, Distance_to_nearest_railway,
    emotion_score_angry:emotion_score_neutral,
    long, lat
  )

# ==== Save data ====
reg_data_indiv %>% # TODO: Move to public repo
  write_csv2("../Data not redistributable/Picture_regression_data_indiv.csv")
   

