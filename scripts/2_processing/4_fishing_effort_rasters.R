# Download fishing effort and create rasters

# Load packages
library(startR)
library(here)
library(raster)
library(furrr)
library(tidyverse)

# Load variables and parameters used everywhere
source(here("scripts", "2_processing", "0_setup.R"))

# Get number of available cores - 1
n_cores <- parallel::detectCores() - 1


# Read the base raster
base_raster <- raster(here("data", "base_raster.tif"))

# Get the data
if(!file.exists(here("data", "gridded_effort.rds"))){
  # Download data from GBQ
  gridded_effort <- get_table(dataset = "enso_gfw",
                              table = "gfw_enso_gridded_monthly_effort_by_gear")
  
  # Export the data
  saveRDS(object = gridded_effort,
          file = here("data", "gridded_effort.rds"),
          version = 2)
} else {
  # If the file exists, read it instead
  gridded_effort <- readRDS(here("data", "gridded_effort.rds"))
}

print("Data have been loaded")

# Function to rasterize each month
my_rasterize <- function(x, res = 0.05, proj, base_raster) {
  # Create raster
  r <- rasterFromXYZ(x,
                     res = res,
                     crs = proj)
  
  # Adjust extent
  bb <- extent(-180, 180, -90, 90)
  r2 <- extend(r, bb)
  r3 <- crop(r2, as(bb, "SpatialPolygons"))
  r4 <- projectRaster(r3,
                      base_raster,
                      crs = proj_beh,
                      method = "ngb",
                      over = T)
  
  return(r3)
}


# Trawlers
if(!file.exists(here("data", "trawlers.tif"))){
  # Set up parallel processing
  plan(multiprocess, workers = n_cores)
  
  # Rasterize at the year-month level
  gridded_effort_trawlers <- gridded_effort %>% 
    filter(best_vessel_class == "trawlers",
           year < 2019) %>% 
    select(year, month, x = lon_bin_center, y = lat_bin_center, hours) %>% 
    group_by(year, month) %>% 
    nest() %>%
    mutate(r = future_map(data, my_rasterize,
                          proj = proj_lonlat,
                          base_raster = base_raster),
           name = paste(year, month, sep = "-"))
  
  # Close connections
  plan(sequential)
  
  print("Trawlers have been rasterized") 
  
  # Create a stack and export it
  trawlers <- stack(gridded_effort_trawlers$r)

  # Save the files
  writeRaster(x = trawlers,
              filename = here("data", "trawlers.tif"))
  
  print("Saved trawlers rasters")
  
  # Remove temp files from disk
  rm_raster_tmp()
}


# Seiners
if(!file.exists(here("data", "seiners.rds"))){
  # Set up parallel processing
  plan(multiprocess, workers = n_cores)
  
  # Rasterize at the year-month level
  gridded_effort_seiners <- gridded_effort %>% 
    filter(!best_vessel_class == "trawlers",
           year < 2019) %>% 
    select(year, month, x = lon_bin_center, y = lat_bin_center, hours) %>% 
    group_by(year, month) %>% 
    nest() %>%
    mutate(r = future_map(data, my_rasterize,
                          proj = proj_lonlat,
                          base_raster = base_raster),
           name = paste(year, month, sep = "-"))
  
  # Close connections
  plan(sequential)
  
  print("Seiners have been rasterized") 
  
  # Create a stack and export it
  seiners <- stack(gridded_effort_seiners$r)

  # Save the files
  writeRaster(x = seiners,
              filename = here("data", "seiners.tif"))
  
  print("Saved seiners raster")
  
  # Remove temp files from disk
  rm_raster_tmp()
}


# END OF SCRIPT








