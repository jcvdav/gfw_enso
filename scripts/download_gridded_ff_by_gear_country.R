# This scripts downloads the gridded fising effort from Google BigQuery.
# The data was originally created within BigQUery using a SQL script
# shown in scripts/gridded_ff_by_gear_country

# Load packages
library(startR)
library(lubridate)
library(tidyverse)

# Create a tbl and collect it in gff in case anything fails later
gff <- get_table(dataset = "enso_gfw",
                 table = "gridded_ff_by_gear_country",
                 show_tables = T)

# If the above didn't fail, we now tidy-up our data
gff <- gff %>%
  distinct() %>% 
  mutate(foreign = as.logical(foreign)) %>% 
  mutate(date = lubridate::date(paste(year, month, "01", sep = "/"))) %>% 
  select(year,
         month,
         date,
         longitude = lon_bin_center,
         latitude = lat_bin_center,
         best_label = best_vessel_class,
         best_flag,
         eez_id,
         eez_iso3,
         everything())

# Export the data as rds to keep the present characteristics
saveRDS(gff, file = here::here("raw_data", "gridded_ff_by_gear_country.rds"))
