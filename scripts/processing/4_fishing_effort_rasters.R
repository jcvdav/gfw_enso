# Download fishing effort and create rasters

# Load packages
library(startR)
library(here)
library(raster)
library(furrr)
library(tidyverse)

# Define projection
proj <- "+proj=longlat +datum=WGS84 +no_defs"

# Download data from GBQ
# gridded_effort <- get_table(dataset = "enso_gfw",
#                             table = "gfw_enso_gridded_monthly_effort_by_gear")

gridded_effort <- readRDS(here("data", "gridded_effort.rds"))

# Function to rasterize each month
my_rasterize <- function(x, res = 0.01, proj = "+proj=longlat +datum=WGS84 +no_defs") {
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

gridded_effort_trawlers <- gridded_effort %>% 
  filter(best_vessel_class == "trawlers",
         year == 2018) %>% 
  select(year, month, x = lon_bin_center, y = lat_bin_center, hours) %>% 
  group_by(year, month) %>% 
  nest() %>%
  mutate(r = future_map(data, my_rasterize))

# "Close" background workers
plan(sequential)

trawlers <- stack(gridded_effort_trawlers$r)

writeRaster(trawlers, filename = here("data", "trawlers.nc"))

# gridded_effort_seiners <- gridded_effort %>% 
#   filter(!best_vessel_class == "trawlers") %>% 
#   select(year, month, x = lon_bin_center, y = lat_bin_center, hours) %>% 
#   group_by(year, month) %>% 
#   nest() %>%
#   mutate(r = map(data, my_rasterize))


# bench <- microbenchmark::microbenchmark(
#   rasterize = r <- st_as_sf(gridded_effort_trawlers, coords = c("x", "y"), crs = proj) %>% 
#     rasterize(x = .,y = ra, field = "hours"),
#   fasterize = f <- rasterFromXYZ(gridded_effort_trawlers, res = 0.01, crs = proj),
#   unit = "ms"
# )
# 
# print(bench, digits = 3)







