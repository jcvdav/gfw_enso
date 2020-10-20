# This scripts downloads the gridded fising effort from Google BigQuery.
# The data was originally created within BigQUery using a SQL script
# shown in scripts/gridded_ff_by_gear_country

# Load packages
library(startR)
library(lubridate)
library(here)
library(tidyverse)

# Create a tbl and collect it in gff in case anything fails later
gff <- get_table(dataset = "enso_gfw",
                 table = "gridded_ff_by_gear_country")

# Store the is_fishing and is_foreign columns as booleans
gff$is_foreign <- as.logical(gff$is_foreign)
gff$is_fishing <- as.logical(gff$is_fishing)

# Export the data as rds to keep the present characteristics
saveRDS(object = gff,
        file = here("raw_data", "gridded_ff_by_gear_country.rds"))
