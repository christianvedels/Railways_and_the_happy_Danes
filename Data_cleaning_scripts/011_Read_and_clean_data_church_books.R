# Read data
# Updated:    2024-11-12
# Auhtors:    Christian Vedel [christian-vs@sam.sdu.dk],
#
# Purpose:    Reads church records

# ==== libraries ====
library(tidyverse)
library(data.table)
source("Data_cleaning_scripts/000_Functions.R")
library(tidygeocoder)

# ==== Read =====
data0 = fread("../Data not redistributable/Parish Records DK/61607_Denmark Church Books_FullDelivery.txt")

# ==== Turn all to character ====
data0 = data0 %>% mutate_all(as.character)

# ==== Descriptive ====
# List of variables
data0 %>% names()

# ==== Functions ====
non_empty = function(x){
  x != ""
}

# ==== Params ====
common_vars = data0 %>% 
  select(
    unique_identifier, 
    SourceDescription, 
    GivenName:Gender, 
    BrowseLevel,
    BrowseLevel1,
    starts_with("Birth"),
    ResidenceParish, ResidenceMunicipality, ResidenceMunicipality,
    FatherGivenName:MotherInLawSurnameAlias # Everything in the end
  ) %>% 
  names()

# ==== Baptism ====
bapt = data0 %>% 
  select(all_of(common_vars), starts_with("Baptism")) %>% 
  filter(
    non_empty(BaptismDay) | non_empty(BaptismMonth) | non_empty(BaptismYear)
  ) %>% 
  rename(
    EventYear = BaptismYear,
    EventMonth = BaptismMonth,
    EventDay =  BaptismDay,
    EventAge = BaptismAge,
    EventPlace = BaptismPlace,
    EventParish0 = BaptismParish,
    EventParish1 = BrowseLevel1,
    EventMunicipality = BaptismMunicipality,
    EventCounty0 = BrowseLevel,
    EventCounty1 = BaptismCounty,
    EventState = BaptismState,
    EventCountry = BaptismCountry
  ) %>% 
  mutate(
    event = "Baptism"
  )

# Check values
if(NROW(bapt)<10^5){
  Unique_misc_sum(bapt)
}

# ==== Confirmations ====
conf = data0 %>% 
  select(all_of(common_vars), starts_with("Confirmation")) %>% 
  filter(
    non_empty(ConfirmationDay) | non_empty(ConfirmationMonth) | non_empty(ConfirmationYear)
  ) %>% 
  rename(
    EventYear = ConfirmationYear,
    EventMonth = ConfirmationMonth,
    EventDay =  ConfirmationDay,
    EventAge = ConfirmationAge,
    EventPlace = ConfirmationPlace,
    EventParish0 = ConfirmationParish,
    EventParish1 = BrowseLevel1,
    EventMunicipality = ConfirmationMunicipality,
    EventCounty0 = BrowseLevel,
    EventCounty1 = ConfirmationCounty,
    EventState = ConfirmationState,
    EventCountry = ConfirmationCountry
  ) %>% 
  mutate(
    event = "Confirmation"
  )

# Check values
if(NROW(conf)<10^5){
  Unique_misc_sum(conf)
}

# ==== Arrival ====
arr = data0 %>% 
  select(all_of(common_vars), starts_with("Arrival")) %>% 
  filter(
    non_empty(ArrivalDay) | non_empty(ArrivalMonth) | non_empty(ArrivalYear)
  ) %>% 
  rename(
    EventYear = ArrivalYear,
    EventMonth = ArrivalMonth,
    EventDay =  ArrivalDay,
    EventAge = ArrivalAge,
    EventPlace = ArrivalPlace,
    EventParish0 = ArrivalParish,
    EventParish1 = BrowseLevel1,
    EventMunicipality = ArrivalMunicipality,
    EventCounty0 = BrowseLevel,
    EventCounty1 = ArrivalCounty,
    EventState = ArrivalState,
    EventCountry = ArrivalCountry
  ) %>% 
  mutate(
    event = "Arrival"
  )

# Check values
if(NROW(arr)<10^5){
  Unique_misc_sum(arr)
}

# ==== Departures ====
# "DepartureDay" "DepartureMonth" "DepartureYear"

depart = data0 %>% 
  select(all_of(common_vars), starts_with("Departure")) %>% 
  filter(
    non_empty(DepartureDay) | non_empty(DepartureMonth) | non_empty(DepartureYear)
  ) %>% 
  rename(
    EventYear = DepartureYear,
    EventMonth = DepartureMonth,
    EventDay =  DepartureDay,
    EventAge = DepartureAge,
    EventPlace = DeparturePlace,
    EventParish0 = DepartureParish,
    EventParish1 = BrowseLevel1,
    EventMunicipality = DepartureMunicipality,
    EventCounty0 = BrowseLevel,
    EventCounty1 = DepartureCounty,
    EventState = DepartureState,
    EventCountry = DepartureCountry
  ) %>% 
  mutate(
    event = "Departure"
  )

# Check values
if(NROW(depart)<10^5){
  Unique_misc_sum(depart)
}

# ==== Marriage ====
# "MarriageDay" "MarriageMonth" "MarriageYear"

marr = data0 %>% 
  select(all_of(common_vars), starts_with("Marriage")) %>% 
  filter(
    non_empty(MarriageDay) | non_empty(MarriageMonth) | non_empty(MarriageYear)
  ) %>% 
  rename(
    EventYear = MarriageYear,
    EventMonth = MarriageMonth,
    EventDay =  MarriageDay,
    EventAge = MarriageAge,
    EventPlace = MarriagePlace,
    EventParish0 = MarriageParish,
    EventParish1 = BrowseLevel1,
    EventMunicipality = MarriageMunicipality,
    EventCounty0 = BrowseLevel,
    EventCounty1 = MarriageCounty,
    EventState = MarriageState,
    EventCountry = MarriageCountry
  ) %>% 
  mutate(
    event = "Marriage"
  )

# Check values
if(NROW(marr)<10^5){
  Unique_misc_sum(marr)
}

# ==== Death ====
# "DeathDay" "DeathMonth" "DeathYear"
death = data0 %>% 
  select(all_of(common_vars), starts_with("Death")) %>% 
  filter(
    non_empty(DeathDay) | non_empty(DeathMonth) | non_empty(DeathYear)
  ) %>% 
  rename(
    EventYear = DeathYear,
    EventMonth = DeathMonth,
    EventDay =  DeathDay,
    EventAge =  DeathAge,
    EventPlace = DeathPlace,
    EventParish0 = DeathParish,
    EventParish1 = BrowseLevel1,
    EventCounty0 = BrowseLevel,
    EventMunicipality = DeathMunicipality,
    EventState = DeathState
  ) %>% 
  mutate(
    event = "Death"
  )

# Check values
if(NROW(death)<10^5){
  Unique_misc_sum(death)
}

# ==== Burial ====
# "BurialDay" "BurialMonth" "BurialYear"
burial = data0 %>% 
  select(all_of(common_vars), starts_with("Burial")) %>% 
  filter(
    non_empty(BurialDay) | non_empty(BurialMonth) | non_empty(BurialYear)
  ) %>% 
  rename(
    EventYear = BurialYear,
    EventMonth = BurialMonth,
    EventDay =  BurialDay,
    EventAge =  BurialAge,
    EventPlace = BurialPlace,
    EventParish0 = BurialParish,
    EventParish1 = BrowseLevel1,
    EventMunicipality = BurialMunicipality,
    EventCounty0 = BrowseLevel,
    EventCounty1 = BurialCounty,
    EventState = BurialState,
    EventCountry = BurialCountry
  ) %>% 
  mutate(
    event = "Burial"
  )

# Check values
if(NROW(burial)<10^5){
  Unique_misc_sum(burial)
}

# ==== Merge together ====
identifiers = data0$unique_identifier

rm(data0)

data_clean = bapt %>% 
  bind_rows(conf) %>% 
  bind_rows(arr) %>% 
  bind_rows(depart) %>% 
  bind_rows(marr) %>% 
  bind_rows(death) %>%
  bind_rows(burial) %>%
  ungroup()

data_clean %>% NROW()

# Missing
sum(!(identifiers %in% data_clean$unique_identifier)) # 5.5%

# ==== Save ====
data_clean %>% saveRDS(file = "../Data not redistributable/Tmp_data/Tmp_church_books.rds")

# ==== Descriptive ====
data_clean %>% 
  group_by(event) %>% 
  count()


