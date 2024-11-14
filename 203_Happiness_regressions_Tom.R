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
reg_data_indiv = read_csv2("../Data not redistributable/Picture_regression_data_indiv.csv", guess_max = 100000) %>%
  filter(midpoint_year >= 1800 & midpoint_year <= 1950)

# --------------------------------------------------------------
reg_data_indiv <- reg_data_indiv %>%
  filter(Detected == "person" & !is.na(emotion_score_happy) & !is.na(rail_opened)) %>%
  select(emotion_score_happy, emotion_score_sad, midpoint_year, rail_opened, decade) %>%
  sample_frac(1.0)


# Create a decade variable for rail_opened
reg_data_indiv <- reg_data_indiv %>%
  mutate(rail_opened_decade = floor(rail_opened / 10) * 10)



#####################
# === HAPPINESS === #
#####################

# Running the staggered DiD for repeated cross-sectional data
did_result <- att_gt(
  yname = "emotion_score_happy",    # outcome variable
  tname = "midpoint_year",          # time variable (e.g., observation year), alternatively: midpoint_year
  gname = "rail_opened",            # time period when treatment starts (staggered by year), alternatively: rail_opened
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

table(reg_data_parish$midpoint_year)

# Convert GIS_ID_w_closest to numeric (needed for DID)
reg_data_parish$GIS_ID_w_closest <- as.numeric(reg_data_parish$GIS_ID_w_closest)

# filter for faster computation
# -------------------------------------------------------------------
reg_data_parish <- reg_data_parish %>% 
  sample_frac(1.0) %>%
  filter(midpoint_year >= 1800 & midpoint_year <= 1950)
# -------------------------------------------------------------------


# Running the staggered DiD after adjustments
did_result <- att_gt(
  yname = "emotion_score_happy",     # Outcome variable
  tname = "midpoint_year",           # Time variable
  gname = "rail_opened",             # Treatment initiation time          
  idname = "GIS_ID_w_closest", # Unit identifier (now numeric)
  data = reg_data_parish,            # Dataset
  panel = T,                         # Treat data as repeated cross-sections when panel = F
  allow_unbalanced_panel = F     # F: did package forces the data into being balanced by dropping units with obs that are missing in any time period
)

# Displaying summary of results
summary(did_result)

# -----------------------------------------------

# Aggregate results by relative time (event study / dynamic effects)
did_dynamic <- aggte(did_result, type = "dynamic", na.rm = T)

# Summarizing dynamic results
summary(did_dynamic)

# Plotting the dynamic (relative time) average treatment effects
plot_did_parish <- ggdid(did_dynamic, xgap = 50)

plot_did_parish

#################################
# === Parish level (decade) === # Only go until approx. 1930 because this is the year when they start to close the railways down?
#################################

# ==== Load data ====
reg_data_parish = read_csv2("Data/Picture_regression_data_parish.csv", guess_max = 100000) %>%
  filter(midpoint_year >= 1800 & midpoint_year <= 1950)

# Convert GIS_ID_w_closest to numeric (needed for DID)
reg_data_parish$GIS_ID_w_closest <- as.numeric(reg_data_parish$GIS_ID_w_closest)

#-------------------------------------------------------------------------------

# decade
reg_data_parish <- reg_data_parish %>% 
  mutate(decade = floor(midpoint_year / 10) * 10) %>%
  mutate(rail_opened_decade = floor(rail_opened / 10) * 10)


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
  yname = "emotion_score_sad",     # Outcome variable
  tname = "decade",           # Time variable
  gname = "rail_opened_decade",             # Treatment initiation time          
  idname = "GIS_ID_w_closest", # Unit identifier (now numeric)
  data = reg_data_parish_decade,            # Dataset
  panel = T,                         # Treat data as repeated cross-sections when panel = F
  allow_unbalanced_panel = F
)


# Displaying summary of results
summary(did_result)


# -----------------------------------------------

# Aggregate results by relative time (event study / dynamic effects)
did_dynamic <- aggte(did_result, type = "dynamic", na.rm = T)

# Summarizing dynamic results
summary(did_dynamic)

# Plotting the dynamic (relative time) average treatment effects
plot_did_parish_decade <- ggdid(did_dynamic, xgap = 3)

plot_did_parish_decade




