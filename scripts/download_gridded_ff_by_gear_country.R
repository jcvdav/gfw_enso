library(dplyr)
library(dbplyr)
library(DBI)
library(magrittr)

BQc <- bigrquery::dbConnect(drv = bigrquery::bigquery(), 
                            project = "ucsb-gfw", 
                            dataset = "enso_gfw", 
                            allowLargeResults = TRUE)

DBI::dbListTables(BQc)

ff <- dplyr::tbl(BQc, "gridded_ff_by_gear_country") %>% 
  collect()

saveRDS(ff, file = here::here("raw_data", "gridded_ff_by_gear_country.rds"))