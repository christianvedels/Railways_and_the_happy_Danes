# Happyness regressions
#
# Date updated:   2024-11-13
# Auhtor:         Tom Görges 
# Purpose:        Runs a few simple regressions on emotions and rail opening
#                 This is only the effect of the opening, disregarding the closing and the very long run effects

rm(list = ls())

# ==== Libraries ====
library(tidyverse)
library(fixest)
library(did)
library(etwfe)
library(foreach)
library(tm)
source("Data_cleaning_scripts/000_Functions.R")

# ==== Load data ====
reg_data_indiv <- read_csv2("../Data not redistributable/Picture_regression_data_indiv.csv", guess_max = 100000) %>%
  filter(midpoint_year >= 1840 & midpoint_year <= 1920) %>% # almost no obs. until 1840, no significant openings after 1920, rapid decline after 1930
  filter(Detected == "person" & !is.na(emotion_score_happy) & !is.na(rail_opened)) %>% # loose many obs.
  select(midpoint_year, decade, rail_opened, contains("emotion_score")) %>% 
  mutate(rail_opened_decade = floor(rail_opened / 10) * 10) %>% # rail decade
  sample_frac(1.0)

# ==== Extract lowest and highest decades ====
lowest_decade <- min(reg_data_indiv$decade, na.rm = TRUE)
highest_decade <- max(reg_data_indiv$decade, na.rm = TRUE)

# ==== Create output directory ====
output_dir <- paste0("plots_", lowest_decade, "_", highest_decade, "_emotions")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# ==== Define emotion columns ====
emotion_columns <- colnames(reg_data_indiv)[grepl("emotion_score_", colnames(reg_data_indiv))]

# ==== Loop through emotions ====
for (emotion in emotion_columns) {
  
  # Run the staggered DiD for the current emotion
  did_result <- att_gt(
    yname = emotion,                # Outcome variable
    tname = "decade",               # Time variable (decade)
    gname = "rail_opened_decade",   # Treatment period (decade)
    data = reg_data_indiv,          # Dataset
    panel = FALSE,                  # Repeated cross-section
    bstrap = TRUE                   # Enable bootstrapping
  )
  
  # Plot group-time average treatment effects
  gtid_plot <- ggdid(did_result) +
    ggtitle(paste0("Group-Time ATT: ", emotion, " (", lowest_decade,"-" ,highest_decade,")"))
  
  # Save group-time plot
  ggsave(filename = paste0(output_dir, "/group_time_", emotion, "_", lowest_decade, "_", highest_decade, ".png"),
         plot = gtid_plot,
         width = 10, height = 7)
  
  # Aggregate results by relative time (dynamic effects)
  did_dynamic <- aggte(did_result, type = "dynamic", na.rm = TRUE)
  
  # Plot dynamic average treatment effects
  dynamic_plot <- ggdid(did_dynamic) +
    ggtitle(paste0("Dynamic ATT: ", emotion, " (", lowest_decade, "-", highest_decade, ")"))
  
  # Save dynamic plot
  ggsave(filename = paste0(output_dir, "/dynamic_", emotion, "_", lowest_decade, "_", highest_decade, ".png"),
         plot = dynamic_plot,
         width = 10, height = 7)
  
  # Aggregate results by calendar time
  did_calendar <- aggte(did_result, type = "calendar", na.rm = TRUE)
  
  # Plot calendar average treatment effects
  calendar_plot <- ggdid(did_calendar) +
    ggtitle(paste0("Calendar ATT: ", emotion, " (", lowest_decade, "-", highest_decade, ")"))
  
  # Save calendar plot
  ggsave(filename = paste0(output_dir, "/calendar_", emotion, "_", lowest_decade, "_", highest_decade, ".png"),
         plot = calendar_plot,
         width = 10, height = 7)
  
  # Log progress
  cat("Finished analysis for:", emotion, "\n")
}

# ==== Summary ====
cat("All plots saved in:", output_dir, "\n")



