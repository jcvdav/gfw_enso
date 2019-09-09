# set up some variables and options

# Define CRS projections
## For unprojected lon lat data
proj_lonlat <- "+proj=longlat +datum=WGS84 +no_defs"
## World Behrmann (54017)
proj_beh <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# Geometry precisions
geometry_precision <- 1000

# Temoporary raster locations
raster::rasterOptions(tmpdir = here::here("data", "raster_tmp"),
                      overwrite = T,
                      maxmemory = 12e+09,
                      memfrac = 0.8)

if(!file.exists(here::here("data", "base_raster.tif"))){
  ## Create extent object
  ext <- extent(-17700000, #xmin
                17720000, #xmax
                -7650000, #min
                7660000) #ymax
  
  # Create base raster
  base_raster <- raster(ext = ext,
                        res = 10000,
                        val = 1L,
                        crs = proj_beh)
  
  # Save the raster for future use
  writeRaster(x = base_raster,
              filename = here("data", "base_raster.tif"),
              overwrite = T)
}

# Function to remove temp files from disk
rm_raster_tmp <- function(){
  # Get the files
  raster_tmp_files <- list.files(path = here::here("data", "raster_tmp"),
                                 pattern = "r_tmp")
  
  # Remove them
  file.remove(here("data", "raster_tmp", raster_tmp_files)) 
}

# Detect cores for paralellization
n_cores <- parallel::detectCores() - 1