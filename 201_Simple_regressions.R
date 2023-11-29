# Simple regressions
#
# Date updated:   2023-10-02
# Auhtor:         Christian Vedel 
# Purpose:        Runs a few simple regressions

# ==== Libraries ====
library(tidyverse)
library(fixest)
source("000_Functions.R")

# ==== Load data ====
grundtvig = read_csv2("Data/REGRESSION_DATA_Grundtvigianism.csv")
census = read_csv2("Data/REGRESSION_DATA_Demography.csv")

# ==== TWFE regressions (Demography) ====
mod1 = feols(
  log(Pop) ~ Connected_rail | GIS_ID + Year,
  data = census,
  cluster = ~ GIS_ID
)

mod1_dash = feols( # Like the one in the thesis
  log(Pop) ~ Connected_rail | County + Year,
  data = census %>% filter(Year %in% c(1850, 1901)),
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

# ==== Doubly Robust DID ====
# This is included because of doubly robust estimator
library(did)

set.seed(20)
census0 = census %>% 
  mutate(lPop = log(Pop)) %>% 
  mutate(
    Year_num = as.numeric(as.character(Year)),
    GIS_ID_num = as.numeric(factor(GIS_ID))
  ) %>% 
  group_by(GIS_ID) %>% 
  mutate( # Treat year
    Treat_year = ifelse(first_occurence(Connected_rail)==1, Year, 0)
  ) %>% 
  mutate( # Treat year
    Treat_year = ifelse(any(Year!=0), max(Treat_year), 0)
  ) %>% 
  mutate(
    lManu = log(Manufacturing_789 + 1),
    lAgri = log(hisco_major6 + 1)
  )

# No covariates
out1 = att_gt(
  yname = "lPop",
  tname = "Year_num",
  gname = "Treat_year",
  idname = "GIS_ID_num",
  data = census0,
)

ggdid(out1)

# Covariates
out2 = att_gt(
  yname = "lPop",
  tname = "Year_num",
  gname = "Treat_year",
  idname = "GIS_ID_num",
  data = census0,
  xformla = ~ lManu + lAgri + Child_women_ratio
)

ggdid(out2)

# Pretreatment outcome also as covariate
out3 = att_gt(
  yname = "lPop",
  tname = "Year_num",
  gname = "Treat_year",
  idname = "GIS_ID_num",
  data = census0,
  xformla = ~ lManu + lAgri + Child_women_ratio + Boulder_clay_pct + area_parish + Distance_market_town + DistOxRoad
)

ggdid(out3)

es3 = aggte(out3, "dynamic")
ggdid(es3)

summary(out1); summary(out2); summary(out3)
