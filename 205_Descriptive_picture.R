# Descriptive picture stats
#
# Date updated:   2024-10-23
# Auhtor:         Christian Vedel 
# Purpose:        Descirptive stats of the picture data

# ==== Libraries ====
library(tidyverse)
source("Data_cleaning_scripts/000_Functions.R")

# ==== Load data ====
reg_data_indiv = read_csv2("../Data not redistributable/Picture_regression_data_indiv.csv", guess_max = 100000)

# ==== Number of pictures ====
p1 = reg_data_indiv %>% 
  distinct(decade, Image) %>% 
  group_by(decade) %>% 
  count() %>% 
  ggplot(aes(decade, n)) + 
  geom_point() +
  geom_line() +
  theme_bw() 

ggsave("Figures/Number_of_pictures.png", plot = p1, width = dims$width*1.5, height = dims$height)

# ==== Different objects ====
tmp_data = reg_data_indiv %>% 
  group_by(decade, Detected) %>% 
  count() %>% 
  group_by(decade) %>% 
  mutate(
    n_total = sum(n)
  ) %>% 
  mutate(
    pct = n/n_total
  )

objects_to_use = tmp_data %>% 
  ungroup() %>% 
  filter(n>1000) %>% 
  distinct(Detected) %>% 
  drop_na()

p1 = tmp_data %>% 
  filter(Detected %in% objects_to_use$Detected) %>% 
  ggplot(aes(decade, pct)) + 
  geom_point() +
  geom_line() +
  theme_bw() + 
  facet_wrap(~Detected, scales = "free_y") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

p1
ggsave("Figures/Objects_detected_above1000.png", plot = p1, width = dims$width*1.5, height = dims$height)

# ==== Emotions over time ====
p1 = reg_data_indiv %>% 
  select(Image, decade, emotion_score_angry:emotion_score_neutral) %>% 
  pivot_longer(
    emotion_score_angry:emotion_score_neutral
  ) %>% 
  mutate(
    name = gsub("emotion_score_", "", name)
  ) %>% 
  rename(
    emotion = name
  ) %>% 
  group_by(decade, emotion) %>% 
  summarise(
    value = mean(value, na.rm = TRUE)
  ) %>% 
  ggplot(
    aes(decade, value, col = emotion)
  ) +
  geom_line() + 
  theme_bw() + 
  labs(
    y = "Average emotion confidence"
  ) + 
  scale_y_continuous(labels = scales::percent)

p1
ggsave("Figures/Average_emotions_by_decade.png", plot = p1, width = dims$width*1.5, height = dims$height)

