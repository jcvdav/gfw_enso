# Download fishing effort and create rasters

# Load packages
library(startR)
library(here)
library(raster)
library(tidyverse)

# Load variables and parameters used everywhere
source(here("scripts", "processing", "0_setup.R"))


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

print("Data has been loaded")

# Function to rasterize each month
my_rasterize <- function(x, res = 0.05, proj) {
  # Create raster
  r <- rasterFromXYZ(x, res = res, crs = proj)
  
  # Adjust extent
  bb <- extent(-180, 180, -90, 90)
  r2 <- extend(r, bb)
  r3 <- crop(r2, as(bb, "SpatialPolygons"))
  
  return(r3)
}


# Trawlers
if(!file.exists(here("data", "trawlers.rds"))){
  # Rasterize at the year-month level
  gridded_effort_trawlers <- gridded_effort %>% 
    filter(best_vessel_class == "trawlers",
           year < 2019) %>% 
    select(year, month, x = lon_bin_center, y = lat_bin_center, hours) %>% 
    group_by(year, month) %>% 
    nest() %>%
    mutate(r = map(data, my_rasterize, proj = proj_lonlat),
           name = paste(year, month, sep = "-"))
  
  print("Trawlers have been rasterized") 
  
  # Create a stack and export it
  trawlers <- stack(gridded_effort_trawlers$r)
  # Add names
  names(trawlers) <- gridded_effort_trawlers$name
  # Save the files
  saveRDS(trawlers,
          file = here("data", "trawlers.rds"))
  writeRaster(x = trawlers,
              filename = here("data", "trawlers.grd"),
              format = "raster",
              overwrite = TRUE)
}


if(!file.exists(here("data", "seiners.rds"))){
  # Rasterize at the year-month level
  gridded_effort_seiners <- gridded_effort %>% 
    filter(!best_vessel_class == "trawlers",
           year < 2019) %>% 
    select(year, month, x = lon_bin_center, y = lat_bin_center, hours) %>% 
    group_by(year, month) %>% 
    nest() %>%
    mutate(r = map(data, my_rasterize, proj = proj_lonlat),
           name = paste(year, month, sep = "-"))
  
  print("Seiners have been rasterized") 
  
  # Create a stack and export it
  seiners <- stack(gridded_effort_seiners$r)
  # Add names
  names(seiners) <- gridded_effort_seiners$name
  # Save the files
  saveRDS(seiners,
          file = here("data", "seiners.rds"))
  writeRaster(x = seiners,
              filename = here("data", "seiners.grd"),
              format = "raster",
              overwrite = TRUE)
  
  print("Saved seiners raster")
}


# END OF SCRIPT








