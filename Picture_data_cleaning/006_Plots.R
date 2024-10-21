# Plots
#
# Date updated:   2024-07-30
# Author:         Tom Görges, Christian Vedel
# Purpose:        Plots and maps

# ==== Libraries ====
library(tidyverse)
source("Picture_data_cleaning/000_Functions.R")

# ==== Read data ====
# Read aps data
df = read_rds("../Data not redistributable/Tmp_data/Tmp_picture_data.rds")

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
  pivot_longer(
    emotion_score_1:emotion_score_neutral,
    names_to = "emotion"
  ) %>% 
  mutate(
    emotion = gsub("emotion_score_", "", emotion)
  ) %>% 
  filter(emotion != "1") %>% 
  filter(midpoint_year>1850) %>% 
  mutate(midpoint_year0 = floor(midpoint_year)) %>% 
  group_by(midpoint_year0, emotion) %>% 
  summarise(
    value_std_error = sd(value, na.rm = TRUE)/sqrt(n()),
    value_mean = mean(value, na.rm = TRUE)
  ) %>% 
  ggplot(aes(midpoint_year0, value_mean, col = emotion)) + 
  # geom_smooth() +
  geom_line() +
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


# ==== Happiness and distance to Copenhagen ====
library(geosphere)
# Coordinates for Copenhagen
copenhagen_long = 12.5683
copenhagen_lat = 55.6761

# Define the function to calculate Haversine distance
haversine_distance = function(long1, lat1, long2, lat2) {
  p1 <- c(long1, lat1)
  p2 <- c(long2, lat2)
  dist <- distHaversine(p1, p2)  # distance in meters
  return(dist)
}

df = df %>% 
  rowwise() %>%
  mutate(
    distance_to_copenhagen_coord = haversine_distance(coords_long, coords_lat, copenhagen_long, copenhagen_lat),
    distance_to_copenhagen_centroid = haversine_distance(centroid_long, centroid_lat, copenhagen_long, copenhagen_lat),
    distance_to_copenhagen_archive = haversine_distance(long, lat, copenhagen_long, copenhagen_lat)
  ) %>%
  ungroup()

df %>% 
  select(long_archive, coords_long, centroid_long) %>% 
  summarise_all(
    function(x){sum(!is.na(x))}
  )

plot_data = df %>% 
  mutate(
    decade = round0(midpoint_year, 10)
  ) %>% 
  group_by(midpoint_year) %>% 
  # mutate(
  #   emotion_score_happy = demean(emotion_score_happy)
  # ) %>%
  filter(midpoint_year>=1850)

plot_data %>% 
  filter(distance_to_copenhagen_coord < 300000) %>% 
  ggplot(aes(x = distance_to_copenhagen_coord, y = emotion_score_happy)) +
  # geom_point(alpha = 0.1) +
  labs(title = "Relationship between Distance to Copenhagen and Happiness",
       x = "Distance to Copenhagen (meters)",
       y = "Happiness Score") +
  geom_smooth() + 
  theme_bw() + 
  facet_wrap(~decade)

plot_data %>% 
  filter(distance_to_copenhagen_coord < 300000) %>% 
  ggplot(aes(x = distance_to_copenhagen_coord, y = emotion_score_happy)) +
  # geom_point(alpha = 0.1) +
  labs(title = "Relationship between Distance to Copenhagen and Happiness",
       x = "Distance to Copenhagen (meters)",
       y = "Happiness Score") +
  geom_smooth() + 
  theme_bw() 


# Plotting the data
p1 = plot_data %>% 
  filter(coords_long < 15) %>% 
  filter(coords_long > 7.5) %>% 
  filter(coords_lat > 54) %>% 
  drop_na(emotion_score_happy) %>% 
  filter(decade<2000) %>% 
  # Plotting the data
  ggplot(aes(x = coords_long, y = coords_lat, z = emotion_score_happy)) +
  stat_summary_2d(binwidth = c(0.2, 0.2), fun = mean) +
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0.5, name = "Happiness Score") +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "Map of Happiness Scores by Location",
    x = "Longitude",
    y = "Latitude"
  )

p1

p1 = p1 + 
  facet_wrap(~decade)

x = plot_data %>% 
  group_by(coords_long, coords_lat) %>% 
  mutate(
    lag_emotion_score_happy = lag(emotion_score_happy)
  )

lm(emotion_score_happy ~lag_emotion_score_happy, data = x) %>% summary()

ggsave("Picture_data_cleaning/Spread_of_happiness.png", plot = p1)
