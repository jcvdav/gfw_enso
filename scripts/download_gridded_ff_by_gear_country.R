# This scripts downloads the gridded fising effort from Google BigQuery.
# The data was originally created within BigQUery using a SQL script
# shown in scripts/gridded_ff_by_gear_country

# Load packages
library(dplyr)
library(dbplyr)
library(DBI)
library(magrittr)

# Create a BigQuery connection
BQc <- bigrquery::dbConnect(drv = bigrquery::bigquery(), 
                            project = "ucsb-gfw", 
                            dataset = "enso_gfw", 
                            allowLargeResults = TRUE)

# Test the connection by reading all the available tables
DBI::dbListTables(BQc)

# Specify the gears I'll want to keep
gears <- c("drifting_longlines",
           "set_longlines",
           "drifting_longlines|set_longlines",
           "tuna_purse_seines",
           "other_purse_seines",
           "purse_seines")

# Create a tbl and collect it in gff in case anything fails later
gff <- dplyr::tbl(BQc, "gridded_ff_by_gear_country") %>% 
  collect()

# If the above didn't fail, we now tidy-up our data
gff %<>% 
  mutate(is_foreign = is_foreign == "TRUE") %>% 
  filter(best_label %in% gears) %>% 
  mutate(date = lubridate::date(paste(year, month, "01", sep = "/")),
         best_label_short = ifelse(str_detect(best_label, "purse"), "purse_seines", "long_lines")) %>% 
  select(year,
         month,
         date,
         longitude = lon_bin_center,
         latitude = lat_bin_center,
         best_label,
         best_label_short,
         iso3,
         eez_iso3,
         everything())

# Export the data as rds to keep the present characteristics
saveRDS(gff, file = here::here("raw_data", "gridded_ff_by_gear_country.rds"))
