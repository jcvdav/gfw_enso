# Identify teleconnected EEZs

# Load packages
library(here)
library(startR)
library(raster)
library(sf)
library(tidyverse)

# Load variables and parameters used everywhere
source(here("scripts", "2_processing", "0_setup.R"))

# Load treatment regions
treatment_pixels_lonlat <- raster(here("data", "treatment_pixels_lonlat.tif"))

eez <- st_read(dsn = here("raw_data", "spatial", "EEZ"),
               layer = "eez_v10") %>% 
  st_simplify(dTolerance = 0.01) %>% 
  filter(!st_is_empty(.)) %>%
  select(MRGID, GeoName, ISO_Ter1) %>% 
  st_cast("POLYGON") %>% 
  mutate(Area = st_area(.))

beginCluster(n = n_cores)
eez_treat <- raster::extract(x = treatment_pixels_lonlat,
                     y = as_Spatial(eez),
                     fun = mean,
                     na.rm = T,
                     df = T,
                     sp = T)
endCluster()

eez2 <- eez %>% 
  left_join(eez_treat@data)
