# Happyness regressions
#
# Date updated:   2024-10-02
# Auhtor:         Christian Vedel 
# Purpose:        Runs a few simple regressions

# ==== Libraries ====
library(tidyverse)
library(fixest)
library(did)
library(etwfe)
library(foreach)
library(tm)
source("Data_cleaning_scripts/000_Functions.R")


# ==== Load data ====
reg_data_indiv = read_csv2("../Data not redistributable/Picture_regression_data_indiv.csv", guess_max = 100000)
reg_data_parish = read_csv2("Data/Picture_regression_data_parish.csv", guess_max = 100000)
reg_data_n_pics = read_csv2("Data/Picture_regression_data_number_of_pictures.csv", guess_max = 100000)

# ==== Small data cleaning ====
reg_data_indiv = reg_data_indiv %>% 
  arrange(midpoint_year) %>% 
  mutate( # Must be numeric for did library
    id = as.numeric(factor(Image)),
    id_gis = as.numeric(factor(GIS_ID_w_closest))
  ) %>% 
  arrange(as_numeric0(treat_time_opened)) %>% 
  mutate(
    treat_time_opened = relevel(factor(treat_time_opened), ref = "0"),
    treat_time_closed = relevel(factor(treat_time_closed), ref = "0")
  )

reg_data_parish = reg_data_parish %>% 
  mutate( # Must be numeric for did library
    id = as.numeric(factor(GIS_ID_w_closest))
  ) %>% 
  drop_na(id) %>% 
  arrange(as_numeric0(treat_time_opened)) %>% 
  mutate(
    treat_time_opened = relevel(factor(treat_time_opened), ref = "0"),
    treat_time_closed = relevel(factor(treat_time_closed), ref = "0")
  )

reg_data_n_pics = reg_data_n_pics %>% 
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

# ==== plot TWFE function ====
plots_funky = function(x, treat_var, e, se = TRUE){
  treat_var = removePunctuation(treat_var, preserve_intra_word_dashes = TRUE)
  
  p1 = x %>% 
    data.frame() %>% 
    mutate(
      var = row.names(x)
    ) %>% 
    # filter(grepl(":", var)) %>% 
    arrange(var) %>% 
    mutate(
      var = removePunctuation(var, preserve_intra_word_dashes = TRUE)
    ) %>% 
    mutate(
      var = gsub(treat_var, "", var) %>% as.numeric
    ) %>% 
    filter(abs(var) < 100) %>% 
    ggplot(aes(var, Estimate)) + 
    geom_point() + 
    labs(
      title = e
    ) + 
    geom_vline(
      xintercept = 0,
      lty = 2
    ) + 
    geom_hline(
      yintercept = 0
    ) + 
    theme_bw() + 
    scale_alpha_continuous(breaks = seq(-100, 100, by = 20))
  
  if(se){
    p1 = p1 +
      geom_errorbar(aes(
        ymax = Estimate + 1.96 * Std..Error,
        ymin = Estimate - 1.96 * Std..Error,
      ))
  }
  
  return(p1)
}


# ==== TWFE ====
mods_opened = foreach(e = reg_data_parish %>% select(emotion_score_angry:emotion_score_neutral) %>% names()) %do% {
  formula0 = as.formula(paste0(e, " ~ treat_time_opened | GIS_ID_w_closest + midpoint_year"))
  mod = feols(
    formula0,
    data = reg_data_parish
  )
  attr(mod, "outcome") = e
  return(mod)
}

plots_opnened = lapply(mods_opened, function(x) {
  plots_funky(coeftable(x),
              e = attr(x, "outcome"),
              treat_var = "treat_time_opened")
})
names(plots_opnened) = emotions

for(e in emotions){
  ggsave(
    paste0("Figures/Railways_on_emotions_opened/", e, ".png"),
    plot = plots_opnened[[e]],
    width = dims$width,
    height = dims$height
  )
}

mods_closed = foreach(e = reg_data_parish %>% select(emotion_score_angry:emotion_score_neutral) %>% names()) %do% {
  formula0 = as.formula(paste0(e, " ~ treat_time_closed | GIS_ID_w_closest + midpoint_year"))
  mod = feols(
    formula0,
    data = reg_data_parish
  )
  attr(mod, "outcome") = e
  return(mod)
}

plots_closed = lapply(mods_closed, function(x) {
  plots_funky(coeftable(x),
              e = attr(x, "outcome"),
              treat_var = "treat_time_closed")
})
names(plots_closed) = emotions

for(e in emotions){
  ggsave(
    paste0("Figures/Railways_on_emotions_closed/", e, ".png"),
    plot = plots_closed[[e]],
    width = dims$width,
    height = dims$height
  )
}

# ==== RIF ====
# To understand what is going on with variance
library(rifreg)

# Demeaning data for efficiency
rif_data = reg_data_indiv %>%
  drop_na(emotion_score_angry:emotion_score_neutral) %>% 
  # mutate(across(emotion_score_angry:emotion_score_neutral, calculate_influence)) %>% 
  mutate(across(emotion_score_angry:emotion_score_neutral, function(x) scale(x, scale = FALSE)))

fit_uqr = rifreg(
  emotion_score_happy ~ treat_time_opened, # Already demeaned above
  data = rif_data,
  statistic = "variance"
  # bootstrap = TRUE
)
x = data.frame(fit_uqr$estimates) %>% rename(Estimate = rif_variance)
attr(x, "outcome") = "rif_variance"
plots_funky(x, "treat_time_opened", se = FALSE)

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

# ==== Railways and Happiness DiD ====
estimate_happy = att_gt(
  yname = "emotion_score_happy",
  tname = "midpoint_year",
  idname = "id",
  gname = "group_var",
  data = reg_data_parish
)

sum_simple = aggte(estimate_happy, type = "simple", na.rm = TRUE)
sum_dynamic = aggte(estimate_happy, type = "dynamic", na.rm = TRUE)
sum_calendar = aggte(estimate_happy, type = "calendar", na.rm = TRUE)

sum_simple
p1 = sum_dynamic %>%
  ggdid() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  scale_x_continuous(
    breaks = seq(-180, 180, by = 20)
  )


p1
ggsave("Tmp_happy.png", width = 8, height = 6, plot = p1)

sum_calendar %>% ggdid() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  scale_x_continuous(
    breaks = seq(1800, 2000, by = 20)
  )

# ==== Railways and Happiness DiD (individuals) ====
estimate_happy = att_gt(
  yname = "emotion_score_happy",
  tname = "midpoint_year",
  # idname = "id",
  gname = "group_var",
  data = reg_data_indiv,
  panel = FALSE,
  allow_unbalanced_panel = TRUE
)

sum_simple = aggte(estimate_happy, type = "simple", na.rm = TRUE)
sum_dynamic = aggte(estimate_happy, type = "dynamic", na.rm = TRUE)
sum_calendar = aggte(estimate_happy, type = "calendar", na.rm = TRUE)

sum_simple
p1 = sum_dynamic %>%
  ggdid() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  scale_x_continuous(
    breaks = seq(-180, 180, by = 20)
  )


p1
ggsave("Tmp_happy.png", width = 8, height = 6, plot = p1)

sum_calendar %>% ggdid() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  scale_x_continuous(
    breaks = seq(1800, 2000, by = 20)
  )



