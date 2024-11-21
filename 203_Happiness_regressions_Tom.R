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
  filter(midpoint_year >= 1840 & midpoint_year <= 1930) # almost no data until 1840 and railway networks shrinks drastically after 1930

# --------------------------------------------------------------
reg_data_indiv <- reg_data_indiv %>%
  filter(Detected == "person" & !is.na(emotion_score_happy) & !is.na(rail_opened)) %>%
  select(emotion_score_happy, emotion_score_sad, midpoint_year, rail_opened, decade) %>%
  sample_frac(1.0)


# Create a decade variable for rail_opened
reg_data_indiv <- reg_data_indiv %>%
  mutate(rail_opened_decade = floor(rail_opened / 10) * 10)

##############################
# === Individual: Decade === #
##############################

# Running the staggered DiD for repeated cross-sectional data
did_result <- att_gt(
  yname = "emotion_score_sad",    # outcome variable
  tname = "decade",          # time variable (e.g., observation year), alternatively: midpoint_year
  gname = "rail_opened_decade",            # time period when treatment starts (staggered by year), alternatively: rail_opened
  data = reg_data_indiv,            # your dataset
  panel = FALSE,                    # specify data as repeated cross-sections
  bstrap = T   # = false: reduces the computation time
  #allow_unbalanced_panel = F     # F: did package forces the data into being balanced by dropping units with obs that are missing in any time period
)

# Summarizing results
summary(did_result)

# Plotting the group-time average treatment effects
ggdid(did_result)

# -----------------------------------------------

# Aggregate results by relative time (event study / dynamic effects)
did_dynamic <- aggte(did_result, type = "dynamic", na.rm = T) # "dynamic", "calendar"

# Summarizing dynamic results
summary(did_dynamic)

# Plotting the dynamic (relative time) average treatment effects
ggdid(did_dynamic)



