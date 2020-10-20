
# Load packages
library(here)
library(raster)
library(sf)
library(tidyverse)

# Ammend extract function
extract <- raster::extract

# Load variables and parameters used everywhere
source(here("scripts", "2_processing", "0_setup.R"))

## Load data
# Treatment regions
treatment_pixels_beh <- raster(here("data", "treatment_pixels_beh.tif"))


## Read WDPA data
wdpa_clean <- read_sf(dsn = here("data", "wdpa_clean.gpkg"))
# Convert to spatial object to use in raster::extract
wdpa_sp <- as_Spatial(wdpa_clean)

# Extract treatment regions
wdpa_treatment_sp <- raster::extract(x = treatment_pixels_beh,
                                     y = wdpa_sp,
                                     fun = mean,
                                     na.rm = T,
                                     df = T,
                                     sp = T)

# Create a column for stauts
wdpa_treatment <- wdpa_treatment_sp@data %>%
  mutate(status = ifelse(treatment_pixels_beh >= 0.5, "TE", "WA"))

# Bring that column into the SF object
wdpa_treat <- wdpa_clean %>% 
  left_join(wdpa_treatment, by = c("WDPAID", "IUCN", "YEAR", "ISO3", "IUCN_INT", "ISO3_INT"))

# Save shapefile with teleconnections
st_write(obj = wdpa_treat,
         dsn = here("data", "teleconnected_wdpa.gpkg"),
         driver = "GPKG",
         delete_dsn = T)
