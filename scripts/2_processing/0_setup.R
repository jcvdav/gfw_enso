# set up some variables and options

# Define CRS projections
## For unprojected lon lat data
proj_lonlat <- "+proj=longlat +datum=WGS84 +no_defs"
## World Behrmann (54017)
proj_beh <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# Geometry precisions
geometry_precision <- 500

# Temoporary raster locations
raster::rasterOptions(tmpdir = here::here("data", "raster_tmp"),
                      overwrite = T,
                      maxmemory = 12e+09,
                      memfrac = 0.8)

# Function to remove temp files from disk
rm_raster_tmp <- function(){
  # Get the files
  raster_tmp_files <- list.files(path = here::here("data", "raster_tmp"),
                                 pattern = "r_tmp")
  
  # Remove them
  file.remove(here("data", "raster_tmp", raster_tmp_files)) 
}
