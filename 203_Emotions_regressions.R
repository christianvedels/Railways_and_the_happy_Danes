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
  filter(midpoint_year >= 1840) %>% # almost no obs. until 1840, no significant openings after 1920, rapid decline after 1930
  filter(Detected == "person" & !is.na(emotion_score_happy) & !is.na(rail_opened)) %>% # loose many obs.
  select(midpoint_year, decade, rail_opened, contains("emotion_score")) %>% 
  mutate(rail_opened_decade = floor(rail_opened / 10) * 10) %>% # rail decade
  sample_frac(1.0)

# ==== Extract lowest and highest decades ====
lowest_decade <- min(reg_data_indiv$decade, na.rm = TRUE)
highest_decade <- max(reg_data_indiv$decade, na.rm = TRUE)

# ==== Create output directory ====
output_dir <- paste0("Figures/plots_", lowest_decade, "_", highest_decade, "_emotions")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# ==== Define emotion columns ====
emotion_columns <- colnames(reg_data_indiv)[grepl("emotion_score_", colnames(reg_data_indiv))]

# ==== Loop through emotions ====
for (emotion in emotion_columns) {
  
  # Run the staggered DiD for the current emotion
  did_result <- att_gt(
    yname = emotion ,               # Outcome variable
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

# Below: Repeated with logs
# ==== Create output directory ====
output_dir <- paste0("Figures/plots_logs", lowest_decade, "_", highest_decade, "_emotions")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Precompute log-transformed columns
for (emotion in emotion_columns) {
  # Add a new column with log-transformed values
  reg_data_indiv[[paste0("log_", emotion)]] <- log(reg_data_indiv[[emotion]] + 0.000001)
}

# Loop through emotions
for (emotion in emotion_columns) {
  # Define the new log-transformed column name
  log_emotion <- paste0("log_", emotion)

  # Run the staggered DiD for the current log emotion
  did_result <- att_gt(
    yname = log_emotion,            # Precomputed log column
    tname = "decade",               # Time variable (decade)
    gname = "rail_opened_decade",   # Treatment period (decade)
    data = reg_data_indiv,          # Dataset
    panel = FALSE,                  # Repeated cross-section
    bstrap = TRUE                   # Enable bootstrapping
  )

  # Plot group-time average treatment effects
  gtid_plot <- ggdid(did_result) +
    ggtitle(paste0("Group-Time ATT: log ", emotion, " (", lowest_decade, "-", highest_decade, ")"))

  # Save group-time plot
  ggsave(filename = paste0(output_dir, "/group_time_log_", emotion, "_", lowest_decade, "_", highest_decade, ".png"),
         plot = gtid_plot,
         width = 10, height = 7)

  # Aggregate results by relative time (dynamic effects)
  did_dynamic <- aggte(did_result, type = "dynamic", na.rm = TRUE)

  # Plot dynamic average treatment effects
  dynamic_plot <- ggdid(did_dynamic) +
    ggtitle(paste0("Dynamic ATT: log ", emotion, " (", lowest_decade, "-", highest_decade, ")"))

  # Save dynamic plot
  ggsave(filename = paste0(output_dir, "/dynamic_log_", emotion, "_", lowest_decade, "_", highest_decade, ".png"),
         plot = dynamic_plot,
         width = 10, height = 7)

  # Aggregate results by calendar time
  did_calendar <- aggte(did_result, type = "calendar", na.rm = TRUE)

  # Plot calendar average treatment effects
  calendar_plot <- ggdid(did_calendar) +
    ggtitle(paste0("Calendar ATT: log ", emotion, " (", lowest_decade, "-", highest_decade, ")"))

  # Save calendar plot
  ggsave(filename = paste0(output_dir, "/calendar_log_", emotion, "_", lowest_decade, "_", highest_decade, ".png"),
         plot = calendar_plot,
         width = 10, height = 7)

  # Log progress
  cat("Finished analysis for: log ", emotion, "\n")
}

# ==== Summary ====
cat("All plots saved in:", output_dir, "\n")


# ==== Number of pics ====
# ==== Load data ====
reg_data_n_pics = read_csv2("Data/Picture_regression_data_number_of_pictures.csv", guess_max = 100000)

# ==== Small data cleaning ====
reg_data_n_pics <- reg_data_n_pics %>% 
  mutate( # Must be numeric for did library
    id = as.numeric(factor(GIS_ID_w_closest))
  ) %>% 
  drop_na(id) %>% 
  arrange(as_numeric0(treat_time_opened)) %>% 
  mutate(
    treat_time_opened = relevel(factor(treat_time_opened), ref = "0"),
    treat_time_closed = relevel(factor(treat_time_closed), ref = "0")
  )

# ==== Check balance ====
reg_data_n_pics %>% 
  group_by(midpoint_year) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(var(n))

reg_data_n_pics %>% 
  group_by(id) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(var(n))

reg_data_n_pics %>% 
  count(is.na(id))

# ==== Number of pics ====
estimate_n_pics_opened = att_gt(
  yname = "l_n_pics",
  tname = "midpoint_year",
  idname = "id",
  gname = "rail_opened",
  data = reg_data_n_pics %>% mutate(l_n_pics = log(n_pictures + 1)),
  panel = TRUE
)

sum_simple = aggte(estimate_n_pics_opened, type = "simple", na.rm = TRUE)
sum_dynamic = aggte(estimate_n_pics_opened, type = "dynamic", na.rm = TRUE)
sum_calendar = aggte(estimate_n_pics_opened, type = "calendar", na.rm = TRUE)

sink("Tables/DID_n_pictures_opened.txt")
print(sum_simple)
sink()

p1 = sum_dynamic %>%
  ggdid() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  scale_x_continuous(
    breaks = seq(-180, 180, by = 20)
  )


p1
ggsave("Figures/DiD_n_pictures_opened.png", width = 8, height = 6, plot = p1)

p1 = sum_calendar %>% ggdid() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  scale_x_continuous(
    breaks = seq(1800, 2000, by = 20)
  )

p1
ggsave("Figures/DiD_n_pictures_opened_effect_by_calendar.png", width = 8, height = 6, plot = p1)

# ==== Number of pics railway closed ====
estimate_n_pics_closed = att_gt(
  yname = "l_n_pics",
  tname = "midpoint_year",
  idname = "id",
  gname = "rail_closed",
  data = reg_data_n_pics %>% mutate(l_n_pics = log(n_pictures + 1)),
  panel = TRUE
)

sum_simple = aggte(estimate_n_pics_closed, type = "simple", na.rm = TRUE)
sum_dynamic = aggte(estimate_n_pics_closed, type = "dynamic", na.rm = TRUE)
sum_calendar = aggte(estimate_n_pics_closed, type = "calendar", na.rm = TRUE)

sink("Tables/DID_n_pictures_closed.txt")
print(sum_simple)
sink()

p1 = sum_dynamic %>%
  ggdid() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  scale_x_continuous(
    breaks = seq(-180, 180, by = 20)
  )

p1
ggsave("Figures/DiD_n_pictures_closed.png", width = 8, height = 6, plot = p1)

p1 = sum_calendar %>% ggdid() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  scale_x_continuous(
    breaks = seq(1800, 2000, by = 20)
  )

p1
ggsave("Figures/DiD_n_pictures_closed_effect_by_calendar.png", width = 8, height = 6, plot = p1)


