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

plot_data = df %>% 
  mutate(
    decade = round0(midpoint_year, 10)
  ) %>% 
  group_by(midpoint_year) %>% 
  mutate(
    emotion_score_happy = demean(emotion_score_happy)
  ) %>% 
  filter(midpoint_year>=1850)

plot_data %>% 
  ggplot(aes(x = distance_to_copenhagen_coord, y = emotion_score_happy)) +
  geom_point(alpha = 0.1) +
  labs(title = "Relationship between Distance to Copenhagen and Happiness",
       x = "Distance to Copenhagen (meters)",
       y = "Happiness Score") +
  geom_smooth() + 
  theme_bw() + 
  facet_wrap(~decade)


# Fit LOESS model
loees_data = plot_data %>% ungroup() %>% 
  filter(coords_long > 7) %>% 
  filter(coords_long < 20) %>% 
  filter(coords_lat > 52) %>% 
  dplyr::select(emotion_score_happy, coords_long, coords_lat) %>% 
  drop_na()
loess_fit = loess(emotion_score_happy ~ coords_long + coords_lat, data = loees_data, span = 0.1)

# Create a grid of values for prediction
grid_size = 100
x_range = seq(min(loees_data$coords_long), max(loees_data$coords_long), length.out = grid_size)
y_range = seq(min(loees_data$coords_lat), max(loees_data$coords_lat), length.out = grid_size)
grid = expand.grid(coords_long = x_range, coords_lat = y_range)

# Predict using the LOESS model
grid$emotion_score_happy <- as.vector(predict(loess_fit, newdata = grid))

grid = grid %>% 
  mutate(
    emotion_score_happy = ifelse(emotion_score_happy<0, 0, emotion_score_happy)
  ) %>% 
  mutate(
    emotion_score_happy = ifelse(emotion_score_happy>1, 1, emotion_score_happy)
  )

# Plot the results
ggplot(grid, aes(x = coords_long, y = coords_lat, fill = emotion_score_happy)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", name = "Happiness Score") +
  labs(title = "Nonparametric Estimation of Happiness Scores",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() + 
  geom_point(
    aes(x = coords_long, y = coords_lat, col = emotion_score_happy),
    data = loees_data
  )
