
# Load packages
library(here)
library(sf)
library(tidyverse)

# Load variables and parameters used everywhere
source(here("scripts", "2_processing", "0_setup.R"))

# Read fishing effort
gridded_effort <- readRDS(here("raw_data", "gridded_ff_by_gear_country.rds")) %>% 
  filter(year < 2019,
         is_fishing) %>% 
  drop_na(is_foreign) %>% 
  mutate(eez_id = as.numeric(eez_id))

# Create panel for kilowats
kilowats <- gridded_effort %>% 
  group_by(year, month, best_vessel_class, eez_id, eez_iso3, is_foreign) %>% 
  summarize(kilowats = sum(kilowats, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year, month, best_vessel_class, eez_id, eez_iso3) %>% 
  mutate(total_kilowats = sum(kilowats, na.rm = T)) %>% 
  ungroup() %>% 
  filter(is_foreign) %>% 
  select(-is_foreign) %>% 
  rename(foreign_kilowats = kilowats) %>% 
  mutate(local_kilowats = total_kilowats - foreign_kilowats,
         prop_kilowats_foreign = foreign_kilowats / total_kilowats)

# Create panel for hours
hours <- gridded_effort %>% 
  group_by(year, month, best_vessel_class, eez_id, eez_iso3, is_foreign) %>% 
  summarize(hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year, month, best_vessel_class, eez_id, eez_iso3) %>% 
  mutate(total_hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  filter(is_foreign) %>% 
  select(-is_foreign) %>% 
  rename(foreign_hours = hours) %>% 
  mutate(local_hours = total_hours - foreign_hours,
         prop_hours_foreign = foreign_hours / total_hours)

# Get both together
effort <- kilowats %>% 
  left_join(hours,  by = c("year", "month", "best_vessel_class", "eez_id", "eez_iso3")) %>% 
  mutate(date = lubridate::date(paste(year, month, 1, sep = "-")))


# Export it
saveRDS(object = effort,
        file = here("data", "eez_fishing_effort_panel.rds"))
         
         
# END OF SCRIPT
         
         
         
         
