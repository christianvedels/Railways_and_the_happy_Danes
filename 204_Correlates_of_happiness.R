# Correlates of happiness
#
# Date updated:   2024-10-20
# Auhtor:         Christian Vedel 
# Purpose:        This script runs regressions for correlates of happiness and picture data


# ==== Libraries ====
library(tidyverse)
library(dataverse)
library(fixest)
library(foreach)
source("Data_cleaning_scripts/000_Functions.R")

# ==== Set dataverse env ====
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

# ==== Load data ====
dhowi = read_csv2("../Data not redistributable/DHOWI.csv") # TODO: Replace with new version
census1901 = read_csv("../Data not redistributable/census/standardized_sources/census/1901/census.csv")
census1880 = read_csv("../Data not redistributable/census/standardized_sources/census/1880/census.csv")
gis_key = read_csv2("https://raw.githubusercontent.com/christianvedels/A_perfect_storm/refs/heads/main/Data/Key_census_to_shape.csv")

hisco = get_dataframe_by_name(
  filename = "Census_HISCO_codes_clean.csv",
  dataset = "10.7910/DVN/WZILNI", # DOI
  server = "dataverse.harvard.edu",
  .f = function(x) read_csv(x) # Function to read the file
)

reg_data_parish = read_csv2("Data/Picture_regression_data_parish.csv", guess_max = 100000)
reg_data_indiv = read_csv2("../Data not redistributable/Picture_regression_data_indiv.csv", guess_max = 100000)

path = "https://raw.githubusercontent.com/christianvedels/A_perfect_storm_replication/main/Data/Geo.csv"
geo = read_csv2(path, guess_max = 2000)

# ==== Light data wrangling ====
# Merge census data
census = census1880 %>% 
  bind_rows(census1901)

# Clean GIS_ID key
gis_key = gis_key %>% 
  rowwise() %>% 
  mutate(tmp = strsplit(GIS_ID, ", ")) %>% 
  mutate(GIS_ID = tmp[3]) %>% 
  select(-tmp)


# Grab relevant census data and link with geo
census = census %>% 
  filter(
    sex == "m", # Working age men
    age >= 25, 
    age <= 65
  ) %>% 
  select(pa_id, event_parish, event_district, event_county, event_year) %>% 
  mutate(
    across(c(event_parish, event_district, event_county),
           sub_scandi)
  ) %>% 
  left_join(
    gis_key,
    by = c("event_parish", "event_district", "event_county")
  )

# Grab relevant HISCO data:
hisco = hisco %>% 
  mutate(
    event_year = case_when(
      grepl("1901", Kilde) ~ 1901,
      grepl("1880", Kilde) ~ 1880,
      TRUE ~ NA
    )
  ) %>% 
  drop_na(event_year)

# Link census and HISCO
census = census %>% 
  left_join(
    hisco, by = c("pa_id", "event_year")
  )

# DHOWI clean
dhowi = dhowi %>% 
  select(HISCO, DHOWI)

# Link to DHOWI
census = census %>% 
  left_join(
    dhowi, by = c("hisco_1" = "HISCO")
  ) %>% 
  rename(DHOWI1 = DHOWI) %>% 
  left_join(
    dhowi, by = c("hisco_2" = "HISCO")
  ) %>% 
  rename(DHOWI2 = DHOWI) %>% 
  left_join(
    dhowi, by = c("hisco_3" = "HISCO")
  ) %>% 
  rename(DHOWI3 = DHOWI) %>% 
  select(-c(hisco_4:desc_5)) %>% 
  mutate(
    across(
      starts_with("prob_"),
      function(x) replace_na(x, 0)
    )
  ) %>% 
  mutate(
    prob_sum = prob_1 + prob_2 + prob_3
  ) %>% 
  mutate( # Normalize sum
    prob_1 = prob_1 / prob_sum,
    prob_2 = prob_2 / prob_sum,
    prob_3 = prob_3 / prob_sum
  ) %>% 
  mutate(
    occinc = ifelse(is.na(DHOWI1), 0, DHOWI1 * prob_1) +
      ifelse(is.na(DHOWI2), 0, DHOWI2 * prob_2) +
      ifelse(is.na(DHOWI3), 0, DHOWI3 * prob_3)
  )

# Summarise
inc_sum = census %>% 
  group_by(GIS_ID, event_year) %>% 
  summarise(
    occinc = mean(occinc, na.rm = TRUE)
  )

# Reg data parish level
reg_data_parish_cross = reg_data_parish %>% 
  left_join(
    inc_sum %>% filter(event_year==1901), 
    by = c("GIS_ID_w_closest" = "GIS_ID")
  ) %>% 
  left_join(geo, by = c("GIS_ID_w_closest" = "GIS_ID")) %>% 
  filter(
    midpoint_year <= 1920,
    midpoint_year >= 1880,
  ) %>% 
  drop_na(emotion_score_angry:emotion_score_neutral, long, lat, midpoint_year, occinc)

reg_data_parish_panel = reg_data_parish %>% 
  left_join(
    inc_sum, 
    by = c("GIS_ID_w_closest" = "GIS_ID")
  ) %>% 
  left_join(geo, by = c("GIS_ID_w_closest" = "GIS_ID")) %>% 
  filter(
    midpoint_year <= 1920,
    midpoint_year >= 1880,
  ) %>% 
  drop_na(emotion_score_angry:emotion_score_neutral, long, lat, midpoint_year, occinc)

# ==== Regressions parish data ====
emotions = reg_data_parish %>% select(emotion_score_angry:emotion_score_neutral) %>% names()
foreach(e = emotions) %do% {
  formula0 = as.formula(paste0(e,"~ log(occinc) + long*lat + long^2 + lat^2 | midpoint_year + County"))
  feols(
    formula0,
    data = reg_data_parish
  )
} %>% etable()

# Get residuals for plots
plots = foreach(e = emotions) %do% {
  formula1 = as.formula(paste0(e,"~ long*lat + long^2 + lat^2 | midpoint_year"))
  res1 = feols( # Resids outcome
    formula1,
    data = reg_data_parish
  ) %>% residuals()
  
  formula2 = log(occinc) ~ long*lat + long^2 + lat^2 | midpoint_year
  res2 = feols( # Resids expl.
    formula2,
    data = reg_data_parish
  ) %>% residuals()
  
  data.frame(res1, res2) %>% 
    ggplot(aes(res2, res1)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth(
      method = "lm",
      col = colours$red
    ) +
    labs(
      x = "log(occinc) residualized\n(Parish level)",
      y = paste0("Pr(", e, ") residualized"),
      title = e
    ) +
    theme_bw()
}

names(plots) = emotions

for(e in emotions){
  ggsave(
    paste0("Figures/Parish_incom_and_emotions/", e, ".png"),
    plot = plots[[e]],
    width = dims$width,
    height = dims$height
  )
}

# ==== Regressions parish data ====
emotions = reg_data_parish %>% select(emotion_score_angry:emotion_score_neutral) %>% names()
ests = foreach(e = emotions) %do% {
  formula0 = as.formula(paste0(e,"~ log(occinc) + long*lat + long^2 + lat^2 | midpoint_year + County"))
  mod = feols(
    formula0,
    data = reg_data_parish_cross
  )
  attr(mod, "outcome") = e
  return(mod)
}

etable(ests)

p1 = lapply(ests, function(x){
  coeftable(x) %>%
    data.frame() %>%
    mutate(
      var = rownames(coeftable(x))
    ) %>%
    filter(
      var == "log(occinc)"
    ) %>% 
    mutate(
      outcome = attr(x, "outcome")
    ) %>% 
    mutate(
      Upper = Estimate + 1.96*Std..Error,
      Lower = Estimate - 1.96*Std..Error
    )
}) %>% 
  do.call("bind_rows", .) %>% 
  mutate(
    outcome = gsub("emotion_score_", "", outcome)
  ) %>% 
  ggplot(aes(Estimate, outcome)) + 
  geom_vline(xintercept = 0) + 
  geom_point(col = colours$red) + 
  labs(
    x = "Estimated parameter of log(Occ. Income) on avg. emotion",
    y = "Emotion"
  ) + 
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), col = colours$red) + 
  theme_bw()

ggsave(
  paste0("Figures/Reg_income_emotion.png"),
  plot = p1,
  width = dims$width,
  height = dims$height
)


# Get residuals for plots
plots = foreach(e = emotions) %do% {
  formula1 = as.formula(paste0(e,"~ long*lat + long^2 + lat^2 | midpoint_year"))
  res1 = feols( # Resids outcome
    formula1,
    data = reg_data_parish
  ) %>% residuals()
  
  formula2 = log(occinc) ~ long*lat + long^2 + lat^2 | midpoint_year
  res2 = feols( # Resids expl.
    formula2,
    data = reg_data_parish
  ) %>% residuals()
  
  data.frame(res1, res2) %>% 
    ggplot(aes(res2, res1)) + 
    geom_point(alpha = 0.1) + 
    geom_smooth(
      method = "lm",
      col = colours$red
    ) +
    labs(
      x = "log(occinc) residualized\n(Parish level)",
      y = paste0("Pr(", e, ") residualized"),
      title = e
    ) +
    theme_bw()
}

names(plots) = emotions

for(e in emotions){
  ggsave(
    paste0("Figures/Parish_incom_and_emotions/", e, ".png"),
    plot = plots[[e]],
    width = dims$width,
    height = dims$height
  )
}
