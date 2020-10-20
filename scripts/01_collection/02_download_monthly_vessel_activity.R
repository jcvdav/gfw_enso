# This scripts downloads the gridded fising effort from Google BigQuery.
# The data was originally created within BigQUery using a SQL script
# shown in scripts/gridded_ff_by_gear_country

# Load packages
library(here)
library(connections)
library(bigrquery)
library(tidyverse)

con <- connection_open(bigquery(),
                       project = "emlab-gcp",
                       dataset = "jc_foreign_fishing",
                       billing = "emlab-gcp",
                       use_legacy_sql = FALSE,
                       allowLargeResults = TRUE)

# Create a tbl and collect it in gff in case anything fails later
panel <- tbl(con, "monthly_vessel_activity_pacific") %>% 
  collect()

# Export the data as rds to keep the present characteristics
saveRDS(object = panel,
        file = here("raw_data", "panel.rds"))
