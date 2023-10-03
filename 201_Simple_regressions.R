# Simple regressions
#
# Date updated:   2023-10-02
# Auhtor:         Christian Vedel 
# Purpose:        Runs a few simple regressions

# ==== Libraries ====
library(tidyverse)
library(fixest)

# ==== Load data ====
grundtvig = read_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv")
census = read_csv2("Data/REGRESSION_DATA_Demography.csv")

# ==== TWFE regressions (Demography) ====
mod1 = feols(
  log(Pop) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod2 = feols(
  log(Child_women_ratio) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod3 = feols(
  log(Manufacturing_789) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod4 = feols(
  log(hisclass_avg) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod5 = feols(
  log(Born_different_county) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

etable(mod1, mod2, mod3, mod4, mod5)

# ==== TWFE regressions (Grundtvigianisme) ====
mod1 = feols(
  Assembly_house ~ Connected_rail | GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

mod2 = feols(
  log(MA_assembly) ~ Connected_rail | GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

mod3 = feols(
  HighSchool ~ Connected_rail | GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)

mod4 = feols(
  log(MA_folkhigh) ~ Connected_rail | GIS_ID + Year,
  data = grundtvig,
  cluster = ~ GIS_ID
)


etable(mod1, mod2, mod3, mod4)

# Add mission houses 
