# Read data
#
# Date updated:   2023-09-15
# Auhtor:         Christian Vedel 
# Purpose:        Reads data

# ==== Libraries ====
library(tidyverse)

# ==== Popdata ====
path = "https://raw.githubusercontent.com/christianvedels/A_perfect_storm_replication/main/Data/Pop_reg.csv"
popdata = read_csv2(path)

# ==== Geo data ==== 
path = "https://raw.githubusercontent.com/christianvedels/A_perfect_storm_replication/main/Data/Parish_soil.csv"
soil = read_csv2(path)
path = "https://raw.githubusercontent.com/christianvedels/A_perfect_storm_replication/main/Data/Geo.csv"
geo = read_csv2(path, guess_max = 2000)

# ==== Creameries ====
creameries = read_csv2("Data/MDS_DM_merge_w_add_data.csv")

# ==== Assembly houses ====
load("Data/key.Rdata")
load("Data/Huse_panel.Rdata")
assembly_houses = huse_panel
rm(huse_panel)

# ==== Hoejskoler (Grundtvigian High Schools) ====
hoejskoler = read_csv2("Data/Hoejskoler_clean_panel.csv")

# ==== OxRoads ====
oxroads = haven::read_dta("../Data not redistributable/Parish_data_Boberg_Fazlic_et_al_2023.dta")
oxroads %>% 
  select(GIS_ID, DistOxRoad) %>% 
  write_csv2("Data/OxRoads.csv")

# ==== Legislators database ====
# Klint, Thorkil, 2023, "The Danish Legislator Database (DLD)", 
# https://doi.org/10.7910/DVN/4SFNC0, Harvard Dataverse, V1
# election = read_rds("../Data not redistributable/Danish Legislators Database/election1849-2022.Rds")
# members = read_rds("../Data not redistributable/Danish Legislators Database/member1849-2022.Rds")

# ==== Save image ====
save.image("../Data not redistributable/All_raw_data_for_project.Rdata")

