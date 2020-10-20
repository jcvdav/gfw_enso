# Load packages
library(startR)
library(here)
library(raster)
library(furrr)
library(tidyverse)

# Load variables and parameters used everywhere
source(here("scripts", "2_processing", "0_setup.R"))
source(here("scripts", "0_functions", "my_rasterize.R"))
source(here("scripts", "0_functions", "rasterize_gear.R"))

# Read the base raster
base_raster <- raster(here("data", "base_raster.tif"))

# Read fishing effort
gridded_effort <- readRDS(here("raw_data", "gridded_ff_by_gear_country.rds")) %>% 
  filter(year < 2019)


print("Data have been loaded")

# Rasterize data
## Trawlers
rasterize_gear(x = gridded_effort,
               base_raster = base_raster,
               gear = "trawlers",
               n_cores = n_cores)

## Tuna purse seines
rasterize_gear(x = gridded_effort,
               base_raster = base_raster,
               gear = "tuna_purse_seines",
               n_cores = n_cores)

## Longlines
rasterize_gear(x = gridded_effort,
               base_raster = base_raster,
               gear = "drifting_longlines",
               n_cores = n_cores)

# END OF SCRIPT








