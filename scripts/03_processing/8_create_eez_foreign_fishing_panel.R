

# Load libraries
library(here)
library(sf)
library(tidyverse)


# Load data
## Read fishing_effort panel
effort <- readRDS(here("data", "eez_fishing_effort_panel.rds"))

##read indices
nino34 <- read.csv(here("data","all_indices.csv"),
                   stringsAsFactors = F) %>% 
  select(year, month = month_n, nino34anom) %>% 
  mutate(nino34anom_l1 = lag(nino34anom),
         nino34anom_l2 = lag(nino34anom, 2))

##read eez treat
eez_treat <- st_read(here("data", "teleconnected_eez.gpkg")) %>% 
  st_set_geometry(NULL) %>% 
  select(eez_id = MRGID,
         eez_iso3 = ISO_Ter1,
         status_c = status,
         area = Area) %>% 
  mutate(eez_iso3 = as.character(eez_iso3),
         status = status_c == "TE")


## combine the data
eez_fishing_effort_treatment_panel <- 
  effort %>% 
  left_join(nino34, by = c("year", "month")) %>% 
  left_join(eez_treat, by = c("eez_id", "eez_iso3")) %>% 
  drop_na(status) %>% 
  mutate(date = lubridate::date(paste(year, month, 1, sep = "-")))

# export
saveRDS(object = eez_fishing_effort_treatment_panel,
        file = here("data", "eez_fishing_effort_treatment_panel.rds"))

