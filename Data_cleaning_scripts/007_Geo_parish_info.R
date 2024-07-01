# Geo parish data
#
# Date updated:   2023-11-04
# Auhtor:         Christian Vedel 
# Purpose:        Adds geo data

# ==== Libraries ====
library(tidyverse)

# ==== Read data ====
# geo data
path = "https://raw.githubusercontent.com/christianvedels/A_perfect_storm_replication/main/Data/Geo.csv"
geo = read_csv2(path, guess_max = 2000)

# Distance to market towns
path = "https://raw.githubusercontent.com/christianvedels/A_perfect_storm_replication/main/Data/Distance_to_market_town.csv"
dist_mt = read_csv2(path, guess_max = 2000)

# Market towns 
path = "https://raw.githubusercontent.com/christianvedels/A_perfect_storm_replication/main/Data/Market_towns.csv"
mt = read_csv2(path, guess_max = 2000)

# Soil type
path = "https://raw.githubusercontent.com/christianvedels/A_perfect_storm_replication/main/Data/Parish_soil.csv"
soil = read_csv2(path, guess_max = 2000)

# Ox roads 
oxroads = read_csv2("Data/OxRoads.csv") %>% mutate(GIS_ID = as.character(GIS_ID))

# ==== Distance to CPH and Hamburg ====
tmp = geo %>% 
  filter(GIS_ID == "1614") %>% 
  add_row(
    Parish = "Hamburg",
    long = 9.990964,
    lat = 53.547483
  )

dist_cph0 = geosphere::distGeo(
  geo %>% select(long, lat) %>% data.matrix(),
  tmp[1,] %>% select(long, lat) %>% data.matrix()
)

dist_hmb0 = geosphere::distGeo(
  geo %>% select(long, lat) %>% data.matrix(),
  tmp[2,] %>% select(long, lat) %>% data.matrix()
)

geo = geo %>% 
  mutate(
    dist_cph = dist_cph0 / 1000, # Convert to km
    dist_hmb = dist_hmb0 / 1000  # Convert to km
  )

# Sanity check
geo %>% 
  ggplot(aes(long, lat, col = dist_cph)) + 
  geom_point() + scale_color_continuous(type = "viridis")

geo %>% 
  ggplot(aes(long, lat, col = dist_hmb)) + 
  geom_point() + scale_color_continuous(type = "viridis")

# ==== Garther Geo data ====
soil = soil %>% 
  filter(SOIL_TYPE=="ML") %>% 
  mutate(area_parish = area_parish / 1000) %>% # Convert to km^2
  rename(Boulder_clay_pct = pct) %>% 
  select(-SOIL_TYPE)

geo %>% 
  select(GIS_ID, Parish, Hundred, County, distance_oce, coastal, dist_hmb, dist_cph) %>% 
  left_join(soil, by = "GIS_ID") %>% 
  left_join(dist_mt, by = "GIS_ID") %>% 
  rename(Dist_coast = distance_oce) %>% 
  left_join(oxroads, by = "GIS_ID") %>% 
  write_csv2("Data/Geo_info.csv")

