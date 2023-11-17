# Instruments to panel data
#
# Date updated:   2023-11-17
# Auhtor:         Tom Görges
# Purpose:        Reads shape files of predicted railways based on 
#                 least cost paths


# ==== Libraries ====
library(tidyverse)
source("000_Functions.R") # Contains calc_rail()

# ==== Load data ====
# Load shape files 

# ==== Run it ====
for(i in 1:length(shapes)){
  param_i = params[i]
  shape_i = shapes[i]
  
  railways_panel_i = calc_rail(
    shape_i,
    shape_parishes,
    verbose = TRUE,
    plots = TRUE,
    id = paste0("predicted_rail_paramS",i),
    years = 1846:2020
  )
  
  railways_panel_i %>% 
    write_csv2(paste0("Data/Instruments/paramS",i))
  
  cat("\n===> FINISHED",i)
}

