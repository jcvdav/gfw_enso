
# ###########################################
# This script reads in the WDPA dataset and
# filters it to keep only MPAs that we want
# to analyse
# ###########################################

# Started writing in 03/07/19
# By: JCVD
# Updated in 22/08/19
# By; JCVD

# Load libraries
library(startR)
library(here)
library(sf)
library(tidyverse)

# Load the WDPA dataset

wdpa <- read_sf(dsn = here("raw_data", "spatial", "WDPA_Jan2019"),
                layer = "WDPA_Jan2019_marine-shapefile-polygons") %>% 
  # st_set_geometry(NULL) %>% 
  filter(MARINE > 1) %>% 
  mutate(LSMPA = (REP_M_AREA > 3e4) * 1,
         RECENT = (STATUS_YR > 2010) * 1,
         STRICT = (IUCN_CAT %in% c("I", "Ia", "Ib", "II")) * 1)

write.csv(x = wdpa,
          file = here("data", "wdpa_filtered.csv"),
          row.names = F)
