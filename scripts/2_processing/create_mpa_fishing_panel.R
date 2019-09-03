
# Load packages
library(here)
library(raster)
library(sf)
library(tidyverse)

# Ammend extract function
extract <- raster::extract

# Load variables and parameters used everywhere
source(here("scripts", "2_processing", "0_setup.R"))

# Layers in the rasters lose their names, so I'll recreate them
names <- expand.grid(month = c(1:12),
                     year = c(2012:2018)) %>% 
  as_tibble() %>% 
  mutate(date = paste(year, month, sep = ".")) %>% 
  pull(date)

## Load rasters
# Treatment regions
treatment_regions <- raster(here("data", "treatment_regions.tif"))
# Trawler effort
trawlers <- brick(here("data", "trawlers.tif"))
names(trawlers) <- names
# Seining effort
seiners <- brick(here("data", "seiners.tif"))
names(seiners) <- names

## Read WDPA data
wdpa <- read_sf(dsn = here("data", "wdpa_clean.gpkg"))
# Convert to spatial object to use in raster::extract
wdpa_sp <- as_Spatial(wdpa)

print("All data hasve been loaded")


## Extract data from rasters in parallel (sometimes)
# Extract treatment regions
wdpa_treatment_sp <- extract(x = treatment_regions,
                             y = wdpa_sp,
                             fun = mean,
                             na.rm = T,
                             df = T,
                             sp = T)

print("Treatment data have been extracted")

# Extract trawling effort
beginCluster(n = n_cores)
wdpa_trawling_sp <- extract(x = trawlers,
                            y = wdpa_sp,
                            fun = sum,
                            na.rm = T,
                            df = T,
                            sp = T)
endCluster()

print("Trawling data have been extracted")

# Extract seining effort
# beginCluster(n = n_cores)
# wdpa_seining_sp <- extract(x = seining,
#                             y = wdpa_sp,
#                             fun = mean,
#                             na.rm = T,
#                             df = T,
#                             sp = T)
# endCluster()

## Convert back to df, and then put together each extraction

wdpa_treatment <- wdpa_treatment_sp@data

wdpa_trawling <- wdpa_trawling_sp@data %>%
  as_tibble() %>% 
  gather(date, trawling, -c(WDPAID, IUCN, YEAR, ISO3, IUCN_INT, ISO3_INT)) %>% 
  mutate(date = str_remove(date, pattern = "X")) %>% 
  separate(col = date, sep = "\\.", into = c("year", "month"))

wdpa_panel <- left_join(wdpa_treatment,
                        wdpa_trawling,
                        by = c("WDPAID", "IUCN", "YEAR", "ISO3", "IUCN_INT", "ISO3_INT"))

# Write tha panel
write.csv(x = wdpa_panel,
          file = here("data", "wdpa_panel.csv"),
          row.names = F)


  
  
  
  
  
  
  
  





