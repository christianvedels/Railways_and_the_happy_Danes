# Correlates of happiness
#
# Date updated:   2024-10-20
# Auhtor:         Christian Vedel 
# Purpose:        This script runs regressions for correlates of happiness and picture data


# ==== Libraries ====
library(tidyverse)
library(dataverse)

# ==== Load data ====
read_csv2()




# ==== Set dataverse env ====
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

# ==== Load data ====
merged_data = read_fst("Data/tmp_census.fst") 
hisco = get_dataframe_by_name(
  filename = "Census_HISCO_codes_clean.csv",
  dataset = "10.7910/DVN/WZILNI", # DOI
  server = "dataverse.harvard.edu",
  .f = function(x) read_csv(x) # Function to read the file
)