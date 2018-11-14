library(dplyr)
library(dbplyr)
library(DBI)
library(magrittr)

BQc <- bigrquery::dbConnect(drv = bigrquery::bigquery(), 
                            project = "ucsb-gfw", 
                            dataset = "enso_gfw", 
                            allowLargeResults = TRUE)

DBI::dbListTables(BQc)

ff <- dplyr::tbl(BQc, "foreign_fishing_year_month_season_vessel") %>% 
  collect()

saveRDS(ff, file = here::here("raw_data", "foreign_fishing_year_month_season_vessel.rds"))