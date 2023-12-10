# Regression data
#
# Date updated:   2023-10-02
# Auhtor:         Christian Vedel 
# Purpose:        Collects data 

# ==== Libraries ====
library(tidyverse)
library(foreach)

# ==== Load data ====
railways = read_csv2("Data/Panel_of_railways_in_parishes.csv", guess_max = 10000)

Assembly_houses = read_csv2("Data/Panel_of_assembly_houses.csv", guess_max = 10000)
Assembly_houses_MA = read_csv2("Data/Panel_of_MA_assembly_houses.csv", guess_max = 10000)

Folk_high_schools = read_csv2("Data/Panel_of_folk_high_schools.csv", guess_max = 10000)
Folk_high_schools_MA = read_csv2("Data/Panel_of_MA_folk_high_schools.csv", guess_max = 10000)

census = read_csv2("Data/Census_data.csv", guess_max = 10000)  

geo = read_csv2("Data/Geo_info.csv", guess_max = 2000)

# ==== Load instrument ====
generated_rails = list.files("Data/Instruments")
instruments = foreach(f = generated_rails) %do% {
  data_f = read_csv2(paste0("Data/Instruments/", f))
  
  param_string = data_f$parameter %>% unique()
  if(length(param_string)>1){
    stop("Non unique param string")
  }
  
  # Rename appropiate variables
  names(data_f)[which(names(data_f)=="Connected_rail")] = paste0("Connected_rail_", param_string)
  names(data_f)[which(names(data_f)=="Distance_to_nearest_railway")] = paste0("Distance_to_nearest_railway_", param_string)
  data_f = data_f %>% select(-parameter)
  
  return(data_f)
}

# Join everything
instruments = foreach(i = instruments, .combine = "left_join") %do% {
  i
}

railways = railways %>% 
  left_join(instruments, by = c("GIS_ID", "Year"))

# ==== Misc small data juggling ====
pop1801 = census %>% 
  filter(Year == 1801) %>% 
  select(GIS_ID, Pop) %>% 
  rename(Pop1801 = Pop)

pop1787 = census %>% 
  filter(Year == 1787) %>% 
  select(GIS_ID, Pop) %>% 
  rename(Pop1787 = Pop)

# ==== Join assembly houses ====
railways_assembly_houses = railways %>% 
  left_join(
    Assembly_houses %>% mutate(GIS_ID = as.character(GIS_ID)), 
    by = c("GIS_ID", "Year")
  ) %>% 
  left_join(
    Assembly_houses_MA %>% mutate(GIS_ID = as.character(GIS_ID)), 
    by = c("GIS_ID", "Year")
  ) %>% 
  rename(MA_assembly = MA) %>% 
  select(-long, -lat) %>% 
  left_join(
    Folk_high_schools %>% mutate(GIS_ID = as.character(GIS_ID)), 
    by = c("GIS_ID", "Year")
  ) %>% 
  left_join(
    Folk_high_schools_MA %>% mutate(GIS_ID = as.character(GIS_ID)), 
    by = c("GIS_ID", "Year")
  ) %>% 
  rename(MA_folkhigh = MA) %>% 
  select(-long, -lat) %>% 
  select(-Parish) %>% 
  left_join(geo, by = "GIS_ID") %>% 
  left_join(pop1787, by = "GIS_ID") %>% 
  left_join(pop1801, by = "GIS_ID")

railways_census = railways_assembly_houses %>% 
  inner_join(census, by = c("GIS_ID", "Year"))

# ==== Save data ====
railways_assembly_houses %>% 
  write_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv")

railways_census %>% 
  write_csv2("Data/REGRESSION_DATA_Demography.csv")
