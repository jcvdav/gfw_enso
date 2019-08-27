
# ###########################################
# This script reads in the WDPA dataset and
# filters it to keep only MPAs that we want
# to analyse
# ###########################################

# Started writing in 03/07/19
# By: JCVD
# Updated in 27/08/19
# By; JCVD

# Load libraries
library(startR)
library(here)
library(raster)
library(sf)
library(fasterize)
library(tidyverse)

# UNEP-WCMC (2019), The World Database on Protected Areas (WDPA) statistics. Cambridge, UK: UNEP- WCMC. Accessed on: [24/08/2019].

# World Behrmann (54017)
proj <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
geometry_precision <- 1000

# Load the WDPA dataset
## Polygons
wdpa_polygons <- read_sf(dsn = here("raw_data", "spatial", "WDPA_marine"),
                layer = "WDPA_marine_polygons") %>% 
  filter(MARINE > 1,
         STATUS %in% c("Designated", "Inscribed", "Established"),
         !DESIG_TYPE %in% c("International", "Not Applicable")) %>% 
  st_set_precision(geometry_precision) %>%
  st_transform(crs = proj) %>% 
  st_set_precision(geometry_precision) %>% 
  select(WDPAID, WDPA_PID, PA_DEF, NAME, DESIG, IUCN_CAT, STATUS, STATUS_YR, ISO3)

## Points
wdpa_points <- read_sf(dsn = here("raw_data", "spatial", "WDPA_marine"),
                         layer = "WDPA_marine_points") %>% 
  filter(MARINE > 1,
         STATUS %in% c("Designated", "Inscribed", "Established"),
         !DESIG_TYPE %in% c("International", "Not Applicable")) %>% 
  filter(is.finite(REP_AREA)) %>% 
  st_transform(crs = proj) %>% 
  st_buffer(dist = sqrt((.$REP_AREA * 1e+06) / pi)) %>% 
  select(WDPAID, WDPA_PID, PA_DEF, NAME, DESIG, IUCN_CAT, STATUS, STATUS_YR, ISO3)

## Combine points and polygons
wdpa <- rbind(wdpa_polygons, wdpa_points) %>% 
  mutate(STRICT = (IUCN_CAT %in% c("I", "Ia", "Ib", "II"))) %>% 
  st_cast(to = "POLYGON") %>% 
  filter(!st_is_empty(.)) %>% 
  filter(STATUS_YR <= 2012)

# Save clean shapefile
st_write(obj = wdpa,
         dsn = here("data", "wdpa_clean"),
         layer = "wdpa_clean",
         driver = "ESRI Shapefile")

# Rasterization
## Extract dimensions of dataset
wdpa_bbox <- st_bbox(wdpa)

## Create extent object
wdpa_ext <- extent(wdpa_bbox[1], #xmin
                   wdpa_bbox[3], #xmax
                   wdpa_bbox[2], #min
                   wdpa_bbox[4]) #ymax

# Create base raster
base_raster <- raster(ext = wdpa_ext,
                      res = 10000,
                      val = 1L,
                      crs = proj)

# Create a raster
wdpa_raster <- fasterize(wdpa, base_raster)
names(wdpa_raster) <- "MPA"

# Save the raster
writeRaster(x = wdpa_raster,
            filename = here("data", "wdpa_raster.nc"),
            overwrite = T)



