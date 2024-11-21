# Match church books to parishes
# Updated:    2024-11-12
# Auhtors:    Christian Vedel [christian-vs@sam.sdu.dk],
#
# Purpose:    Matches church books to parishes

# ==== Libraries ====
library(tidyverse)
library(sf)
library(stringdist)
source("Data_cleaning_scripts/000_Functions.R")

# ==== Read data ====
data0 = read_rds("../Data not redistributable/Tmp_data/Tmp_church_books.rds")

shape_parishes = read_sf("Data/sogne_shape/sogne.shp")

# ==== Unique parishes ====
clean_text0 = function(x){
  x = replace_na(x, "")
  x = sub_scandi(x)
  x = tolower(x)
  x = gsub("sogn", "", x)
  x = trimws(x)
  return(x)
}

data0 = data0 %>% 
  mutate(
    EventCounty0 = clean_text0(EventCounty0),
    EventCounty1 = clean_text0(EventCounty1),
    EventParish0 = clean_text0(EventParish0),
    EventParish1 = clean_text0(EventParish1)
  )

parishes_church_books = data0 %>% 
  group_by(EventCounty0, EventCounty1, EventParish0, EventParish1) %>% 
  count()

# Add shape guess
shape_parishes = shape_parishes %>% 
  data.frame() %>% 
  mutate(
    AMT = clean_text0(AMT),
    HERRED = clean_text0(HERRED),
    SOGN = clean_text0(SOGN)
  ) %>% 
  distinct(AMT, HERRED, SOGN, GIS_ID, lat, long)

dist_county0 = stringdistmatrix(parishes_church_books$EventCounty0, shape_parishes$AMT, method = "jw")
dist_county1 = stringdistmatrix(parishes_church_books$EventCounty1, shape_parishes$AMT, method = "jw") 
dist_parish0 = stringdistmatrix(parishes_church_books$EventParish0, shape_parishes$SOGN, method = "jw")
dist_parish1 = stringdistmatrix(parishes_church_books$EventParish1, shape_parishes$SOGN, method = "jw") 
total_dist = dist_county0 + dist_county1 + dist_parish0 + dist_parish1

suggested_match = apply(total_dist, 1, function(x){
  res = shape_parishes[which.min(x),] %>% 
    mutate(
      dist = x[which.min(x)]
    )
  
  if(NROW(res)>1) stop("This is something to handle")
  
  return(res)
}) %>% do.call("bind_rows", .)

suggested_gis_id = parishes_church_books %>% bind_cols(suggested_match)

# Add key guess
shape_parishes = shape_parishes %>% data.frame() %>% 
  distinct(AMT, HERRED, SOGN, GIS_ID, lat, long)

dist_county0 = stringdistmatrix(parishes_church_books$EventCounty0, shape_parishes$AMT, method = "jw")
dist_county1 = stringdistmatrix(parishes_church_books$EventCounty1, shape_parishes$AMT, method = "jw") 
dist_parish0 = stringdistmatrix(parishes_church_books$EventParish0, shape_parishes$SOGN, method = "jw")
dist_parish1 = stringdistmatrix(parishes_church_books$EventParish1, shape_parishes$SOGN, method = "jw") 
total_dist = dist_county0 + dist_county1 + dist_parish0 + dist_parish1

suggested_match = apply(total_dist, 1, function(x){
  res = shape_parishes[which.min(x),] %>% 
    mutate(
      dist = x[which.min(x)]
    )
  
  if(NROW(res)>1) stop("This is something to handle")
  
  return(res)
}) %>% do.call("bind_rows", .)

suggested_gis_id = parishes_church_books %>% bind_cols(suggested_match)

suggested_gis_id %>% 
  mutate(
    lat = as.numeric(lat),
    long = as.numeric(long)
  ) %>% 
  ungroup() %>%  
  write_csv2("tmp_Church_books_GIS_ID_guess.csv")

# ==== Link data using key ====
# TODO: Replace with manually corrected version
tmp = suggested_gis_id %>% select(GIS_ID, EventCounty0, EventCounty1, EventParish0, EventParish1)
data1 = data0 %>%
  left_join(
    tmp,
    by = c("EventCounty0", "EventCounty1", "EventParish0", "EventParish1")
  )

# ==== Simple desc ====
data1 %>%
  group_by(EventYear=="", event) %>% 
  count() %>% 
  group_by(event) %>% 
  mutate(
    pct = n / sum(n)
  )

data1 %>%
  group_by(EventYear_imp=="NO INFO", event) %>% 
  count() %>% 
  group_by(event) %>% 
  mutate(
    pct = n / sum(n)
  )

# Comment: A few missing event years. One example: data0 %>% filter(unique_identifier == 35511) %>% View() (stillbirth)


# ==== Summarise ====
data1 = data1 %>% ungroup()
# data1$event %>% unique()
# [1] "Baptism"      "Confirmation" "Arrival"      "Departure"    "Marriage"     "Death"        "Burial"      

# Baptism
baptisms = data1 %>% 
  filter(event == "Baptism") %>% 
  group_by(EventYear_imp, GIS_ID) %>% 
  summarise(
    baptisms = n()
  )

# Death
deaths = data1 %>% 
  filter(event == "Burial") %>% 
  mutate(
    age_clean = ifelse(EventAge %in% 0:125, as.numeric(EventAge), -1)
  ) %>% 
  group_by(EventYear_imp, GIS_ID, age_clean) %>% 
  count() %>% 
  arrange(age_clean) %>% 
  pivot_wider(values_from = n, names_from = age_clean, names_prefix = "n_age_of_death") %>% 
  mutate_all(function(x) replace_na(x, 0)) %>% 
  rename(
    n_age_of_death_unk = `n_age_of_death-1`
  ) %>%
  mutate(
    n_total = rowSums(across(starts_with("n_age_of_death")), na.rm = TRUE)  # Ensure only numeric columns are used
  )

# ==== Save data ====
path = "Data/Church_book_data/"

baptisms %>% write_csv2(paste0(path,"baptisms.csv"))
deaths %>% write_csv2(paste0(path,"deaths_by_age.csv"))


suggested_gis_id %>% 
  write_csv2(paste0(path,"automatic_parish_to_GIS_ID_key.csv"))

# ==== Simple sanity check descriptives ====
deaths %>% 
  pivot_longer(n_age_of_death0:n_age_of_death119) %>% 
  mutate(
    age = as.numeric(gsub("n_age_of_death", "", name))
  ) %>% 
  mutate(EventYear_imp = as.numeric(EventYear_imp)) %>% 
  mutate(
    decade = floor(EventYear_imp/10)*10
  ) %>% 
  group_by(decade, age) %>% 
  summarise(
    n = sum(value)
  ) %>% 
  ggplot(aes(age, n, col = factor(decade))) + 
  geom_line() + 
  facet_wrap(~decade, scales = "free_y") + 
  theme_bw()

# By gender
x = data1 %>% 
  filter(event == "Burial") %>% 
  mutate(
    age_clean = ifelse(EventAge %in% 0:125, as.numeric(EventAge), -1)
  ) %>% 
  group_by(EventYear_imp, age_clean, Gender) %>% 
  count()

expand.grid(
  EventYear_imp = unique(x$EventYear_imp),
  age_clean = unique(x$age_clean),
  Gender = unique(x$Gender)
) %>% 
  left_join(
    x, by = c("EventYear_imp", "age_clean", "Gender")
  ) %>% 
  filter(age_clean >= 0) %>% 
  mutate(
    EventYear_imp = floor(as.numeric(EventYear_imp)/10)*10
  ) %>% 
  ggplot(aes(x = age_clean, y = n, col = Gender)) + 
  geom_smooth() + 
  facet_wrap(~EventYear_imp, scales = "free_y") + 
  theme_bw()


  
  

