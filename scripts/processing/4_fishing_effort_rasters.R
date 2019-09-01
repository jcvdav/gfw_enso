# Download fishing effort and create rasters

# Load packages
library(startR)
library(here)
library(raster)
library(furrr)
library(tidyverse)

# Define projection
proj <- "+proj=longlat +datum=WGS84 +no_defs"



# Get the data


if(file.exists(here("data", "gridded_monthly_effort_by_gear.rds"))){
  # Download data from GBQ
  gridded_effort <- get_table(dataset = "enso_gfw",
                              table = "gfw_enso_gridded_monthly_effort_by_gear")
  
  # Export the data
  saveRDS(object = gridded_effort,
          file = here("data", "gridded_effort.rds"))
} else {
  # If the file exists, read it instead
  gridded_effort <- readRDS(here("data", "gridded_effort.rds"))
}

# Function to rasterize each month
my_rasterize <- function(x, res = 0.1, proj = "+proj=longlat +datum=WGS84 +no_defs") {
  # Create raster
  r <- rasterFromXYZ(x, res = res, crs = proj)
  
  # Adjust extent
  bb <- extent(-180, 180, -90, 90)
  r2 <- extend(r, bb)
  r3 <- crop(r2, as(bb, "SpatialPolygons"))
  
  return(r3)
}

# Set up parallelization
plan(multiprocess)

# Trawlers
gridded_effort_trawlers <- gridded_effort %>% 
  filter(best_vessel_class == "trawlers",
         year < 2018) %>% 
  select(year, month, x = lon_bin_center, y = lat_bin_center, hours) %>% 
  group_by(year, month) %>% 
  nest() %>%
  mutate(r = future_map(data, my_rasterize))

# Seiners
gridded_effort_seiners <- gridded_effort %>% 
  filter(!best_vessel_class == "trawlers",
         year < 2018) %>% 
  select(year, month, x = lon_bin_center, y = lat_bin_center, hours) %>% 
  group_by(year, month) %>% 
  nest() %>%
  mutate(r = future_map(data, my_rasterize))

# "Close" background workers
plan(sequential)

# Add names to each layer before exporting

trawlers <- brick(gridded_effort_trawlers$r)
saveRDS(trawlers,
        filename = here("data", "trawlers.rds"))


seiners <- brick(gridded_effort_seiners$r)
saveRDS(seiners,
        filename = here("data", "seiners.rds"))






