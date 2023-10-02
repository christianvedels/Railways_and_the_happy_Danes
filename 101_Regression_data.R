# Regression data
#
# Date updated:   2023-10-02
# Auhtor:         Christian Vedel 
# Purpose:        Collects data 

# ==== Libraries ====
library(tidyverse)

# ==== Load data ====
railways = read_csv2("Data/Panel_of_railways_in_parishes.csv", guess_max = 10000)

Assembly_houses = read_csv2("Data/Panel_of_assembly_houses.csv", guess_max = 10000)
Assembly_houses_MA = read_csv2("Data/Panel_of_MA_assembly_houses.csv", guess_max = 10000)

Folk_high_schools = read_csv2("Data/Panel_of_folk_high_schools.csv", guess_max = 10000)
Folk_high_schools_MA = read_csv2("Data/Panel_of_MA_folk_high_schools.csv", guess_max = 10000)

census = read_csv2("Data/Census_data.csv", guess_max = 10000)  

# ==== Join assembly houses ====
railways_assembly_houses = railways %>% 
  inner_join(
    Assembly_houses %>% mutate(GIS_ID = as.character(GIS_ID)), 
    by = c("GIS_ID", "Year")
  ) %>% 
  inner_join(
    Assembly_houses_MA %>% mutate(GIS_ID = as.character(GIS_ID)), 
    by = c("GIS_ID", "Year")
  ) %>% 
  rename(MA_assembly = MA) %>% 
  select(-long, -lat) %>% 
  inner_join(
    Folk_high_schools %>% mutate(GIS_ID = as.character(GIS_ID)), 
    by = c("GIS_ID", "Year")
  ) %>% 
  inner_join(
    Folk_high_schools_MA %>% mutate(GIS_ID = as.character(GIS_ID)), 
    by = c("GIS_ID", "Year")
  ) %>% 
  rename(MA_folkhigh = MA) %>% 
  select(-long, -lat)

railways_census = railways_assembly_houses %>% 
  inner_join(census, by = c("GIS_ID", "Year"))

# ==== Save data ====
railways_assembly_houses %>% 
  write_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv")

railways_census %>% 
  write_csv2("Data/REGRESSION_DATA_Demography.csv")
