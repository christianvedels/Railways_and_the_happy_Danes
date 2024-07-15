# Data cleaning
#
# Date updated:   2024-07-04
# Author:         Tom Görges, Christian Vedel
# Purpose:        This script loads and cleans the data created by archival picture sentiments

# ==== Libraries ====
library(tidyverse)
source("Picture_data_cleaning/000_Functions.R")

# ==== Read data ====
# Read aps data
df = read_aps_data("archival_picture_sentiments/fer_results_test.csv")

# Metadata
metadata = read_rds("../Data not redistributable/Tmp_data/Tmp_picture_data.rds")

# Geographical features
path = "https://raw.githubusercontent.com/christianvedels/A_perfect_storm_replication/main/Data/Geo.csv"
geo = read_csv2(path, guess_max = 2000)

# Shape file 


# ==== Join data ====
df = df %>% # Joining on metadata
  mutate(
    id = gsub(".png","", Image)
  ) %>% 
  left_join(metadata, by = "id")

# Time periods
df = df %>% 
  mutate(
    decade = round0(midpoint_year, 5)
  )


# ==== Plots ====

df %>% 
  pivot_longer(
    emotion_score_1:emotion_score_neutral,
    names_to = "emotion"
  ) %>% 
  mutate(
    emotion = gsub("emotion_score_", "", emotion)
  ) %>% 
  filter(emotion != "1") %>% 
  filter(midpoint_year>1850) %>% 
  ggplot(aes(midpoint_year, value, col = emotion)) + 
  geom_smooth() +
  # geom_point(alpha = 0.1) + 
  # ylim(0, 1) +
  theme_bw() +  
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(~emotion, scales = "free_y")
  

df %>% 
  group_by(decade, Detected) %>% 
  count() %>% 
  group_by(decade) %>% 
  mutate(
    n_total_decade = sum(n) 
  ) %>% 
  mutate(
    pct = n / n_total_decade
  ) %>% 
  group_by(Detected) %>% 
  mutate(
    n_total_object = sum(n) 
  ) %>% 
  filter(n_total_object > 100) %>% 
  filter(decade > 1840) %>% 
  filter(Detected == "tie") %>%
  ggplot(aes(decade, pct)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~Detected, scales = "free_y") +
  theme_bw() +  
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(n.breaks = 20)


df %>% 
  group_by(decade, Detected) %>% 
  count() %>% 
  group_by(decade) %>% 
  mutate(
    n_total_decade = sum(n) 
  ) %>% 
  mutate(
    pct = n / n_total_decade
  ) %>% 
  group_by(Detected) %>% 
  mutate(
    n_total_object = sum(n) 
  ) %>% 
  filter(n_total_object > 100) %>% 
  filter(decade > 1840) %>% 
  # filter(Detected == "tie") %>%
  ggplot(aes(decade, pct)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~Detected, scales = "free_y") +
  theme_bw() +  
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(n.breaks = 20)
  
