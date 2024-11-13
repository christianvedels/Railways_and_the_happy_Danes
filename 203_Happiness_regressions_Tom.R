# Happyness regressions
#
# Date updated:   2024-11-13
# Auhtor:         Christian Vedel, Tom Görges 
# Purpose:        Runs a few simple regressions

rm(list = ls())



# ==== Libraries ====
library(tidyverse)
library(fixest)
library(did)
library(etwfe)
library(foreach)
library(tm)
source("Data_cleaning_scripts/000_Functions.R")

############################
# === Individual level === #
############################

# ==== Load data ====
reg_data_indiv = read_csv2("../Data not redistributable/Picture_regression_data_indiv.csv", guess_max = 100000)

# --------------------------------------------------------------
reg_data_indiv <- reg_data_indiv %>%
  mutate(unique_id = 1:n()) %>%
  filter(Detected == "person" & !is.na(emotion_score_happy) & !is.na(rail_opened)) %>%
  select(unique_id, emotion_score_happy, emotion_score_sad, midpoint_year, rail_opened, decade) %>%
  sample_frac(1.0)

# Create a decade variable for rail_opened
reg_data_indiv <- reg_data_indiv %>%
  mutate(rail_opened_decade = floor(rail_opened / 10) * 10)

# reduce time window
reg_data_indiv <- reg_data_indiv %>% filter(decade > 1830 & decade < 1930)

table(reg_data_indiv$decade)

#####################
# === HAPPINESS === #
#####################

# Running the staggered DiD for repeated cross-sectional data
did_result <- att_gt(
  yname = "emotion_score_happy",    # outcome variable
  tname = "decade",          # time variable (e.g., observation year), alternatively: midpoint_year
  gname = "rail_opened_decade",            # time period when treatment starts (staggered by year), alternatively: rail_opened
  data = reg_data_indiv,            # your dataset
  panel = FALSE # specify data as repeated cross-sections
)

# Summarizing results
summary(did_result)

# Plotting the group-time average treatment effects
ggdid(did_result)

# -----------------------------------------------

# Aggregate results by relative time (event study / dynamic effects)
did_dynamic <- aggte(did_result, type = "dynamic", na.rm = T)

# Summarizing dynamic results
summary(did_dynamic)

# Plotting the dynamic (relative time) average treatment effects
ggdid(did_dynamic)


###################
# === SADNESS === #
###################

# Running the staggered DiD for repeated cross-sectional data
did_result <- att_gt(
  yname = "emotion_score_sad",    # outcome variable
  tname = "decade",          # time variable (e.g., observation year), alternatively: midpoint_year
  gname = "rail_opened_decade",            # time period when treatment starts (staggered by year), alternatively: rail_opened
  data = reg_data_indiv,            # your dataset
  panel = FALSE # specify data as repeated cross-sections
)

# Summarizing results
summary(did_result)

# Plotting the group-time average treatment effects
ggdid(did_result)

# -----------------------------------------------

# Aggregate results by relative time (event study / dynamic effects)
did_dynamic <- aggte(did_result, type = "dynamic", na.rm = T)

# Summarizing dynamic results
summary(did_dynamic)

# Plotting the dynamic (relative time) average treatment effects
ggdid(did_dynamic)


########################
# === Parish level === #
########################

# ==== Load data ====
reg_data_parish = read_csv2("Data/Picture_regression_data_parish.csv", guess_max = 100000)

# Convert GIS_ID_w_closest to numeric (needed for DID)
reg_data_parish$GIS_ID_w_closest <- as.numeric(reg_data_parish$GIS_ID_w_closest)

# Check for missing values in key columns and remove rows with NAs if needed
reg_data_parish <- reg_data_parish %>%
  filter(!is.na(emotion_score_happy) & !is.na(midpoint_year) & !is.na(rail_opened) & !is.na(GIS_ID_w_closest))

reg_data_parish <- reg_data_parish %>% 
  mutate(decade = floor(midpoint_year / 10) * 10) %>%
  mutate(rail_opened_decade = floor(rail_opened / 10) * 10)

#-------------------------------------------------------------------------------

# Aggregate emotion_score_sad and emotion_score_happy by decade
reg_data_parish_decade <- reg_data_parish %>%
  group_by(decade, GIS_ID_w_closest) %>%
  summarise(
    emotion_score_sad = mean(emotion_score_sad, na.rm = TRUE),
    emotion_score_happy = mean(emotion_score_happy, na.rm = TRUE),
    rail_opened_decade = mean(rail_opened_decade, na.rm = T),
    .groups = "drop"  # This removes the grouping after summarising
  )

#-------------------------------------------------------------------------------


# Running the staggered DiD after adjustments
did_result <- att_gt(
  yname = "emotion_score_happy",     # Outcome variable
  tname = "decade",           # Time variable
  gname = "rail_opened_decade",             # Treatment initiation time          
  idname = "GIS_ID_w_closest", # Unit identifier (now numeric)
  data = reg_data_parish,            # Dataset
  panel = T,                         # Treat data as repeated cross-sections when panel = F
  allow_unbalanced_panel = TRUE
)


# Displaying summary of results
summary(did_result)


# -----------------------------------------------

# Aggregate results by relative time (event study / dynamic effects)
did_dynamic <- aggte(did_result, type = "dynamic", na.rm = T)

# Summarizing dynamic results
summary(did_dynamic)

# Plotting the dynamic (relative time) average treatment effects
ggdid(did_dynamic)





