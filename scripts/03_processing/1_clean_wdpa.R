
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

# Load variables and parameters used everywhere
source(here("scripts", "2_processing", "0_setup.R"))

# UNEP-WCMC (2019), The World Database on Protected Areas (WDPA) statistics. Cambridge, UK: UNEP- WCMC. Accessed on: [24/08/2019].
# Load the WDPA dataset
## Polygons
wdpa_polygons <- read_sf(dsn = here("raw_data", "spatial", "WDPA_marine"),
                layer = "WDPA_marine_polygons") %>% 
  filter(MARINE > 0,
         !str_detect(tolower(MANG_PLAN), "non-mpa"),
         STATUS %in% c("Designated", "Inscribed", "Established"),
         STATUS_YR <= 2012,
         !DESIG_TYPE %in% c("International", "Not Applicable")) %>% 
  mutate(no_take = (NO_TAKE == "All") | (NO_TAKE == "Part" & NO_TK_AREA > 0.75 * GIS_M_AREA)) %>% 
  filter(no_take,
         !WDPAID %in% c(309888, 478191)) %>% 
  st_set_precision(geometry_precision) %>%
  st_transform(crs = proj_beh) %>% 
  st_set_precision(geometry_precision) %>% 
  select(WDPAID, WDPA_PID, PA_DEF, NAME, DESIG, IUCN_CAT, STATUS, STATUS_YR, ISO3) %>% 
  lwgeom::st_make_valid()

# Before we rasterize, we need to create a dictionary for
# each column of interest. We want to know MPA id, IUCN Cat,
# and the country where it sits.

iucn_cats <- c("Ia"  = 1,
               "Ib"  = 1,
               "II"  = 2,
               "III" = 3,
               "IV"  = 4,
               "V"   = 5,
               "VI"  = 6)

## Modify our data
wdpa <- #rbind(wdpa_polygons, wdpa_points) %>% 
  wdpa_polygons %>% 
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
  filter(!st_is_empty(.)) %>% 
  rename(IUCN = IUCN_CAT,
         YEAR = STATUS_YR) %>% 
  group_by(WDPAID, IUCN, YEAR, ISO3) %>% 
  summarize(a = 1) %>% 
  select(-a) %>% 
  ungroup() %>% 
    mutate(IUCN_INT = iucn_cats[IUCN],
           ISO3_INT = countrycode::countrycode(sourcevar = ISO3,
                                               origin = "iso3c",
                                               destination = "iso3n")) %>% 
    st_cast(to = "POLYGON")
  
# Save clean shapefile
st_write(obj = wdpa,
         dsn = here("data", "wdpa_clean.gpkg"),
         driver = "GPKG",
         delete_dsn = T)

# Save a csv version without geometries
wdpa_csv <- wdpa %>% 
  st_set_geometry(NULL)

write.csv(x = wdpa_csv,
          file = here("data", "wdpa_clean.csv"),
          row.names = F)

# Read a reference raster
base_raster <- raster(here("data", "base_raster.tif"))


# Create a raster
## We will create a different raster for each column
## of interest in wdpa, and then stack them together


### First, just inside / outside MPA
wdpa_raster_is_mpa <- fasterize(sf = wdpa,
                                raster = base_raster,
                                background = NA)

### Now, one for WDPA id
wdpa_raster_wdpaid <- fasterize(sf = wdpa,
                                raster = base_raster,
                                field = "WDPAID",
                                background = NA)


### Now one for country ISO
wdpa_raster_iso <- fasterize(sf = wdpa,
                             raster = base_raster,
                             field = "ISO3_INT",
                             background = NA)

### Now one for IUCN category
wdpa_raster_iucn <- fasterize(sf = wdpa,
                              raster = base_raster,
                              field = "IUCN_INT",
                              background = NA)


# Save the rasters
writeRaster(x = wdpa_raster_is_mpa,
            filename = here("data", "wdpa_raster_is_mpa.tif"),
            overwrite = T)

writeRaster(x = wdpa_raster_wdpaid,
            filename = here("data", "wdpa_raster_wdpaid.tif"),
            overwrite = T)

writeRaster(x = wdpa_raster_iso,
            filename = here("data", "wdpa_raster_iso.tif"),
            overwrite = T)

writeRaster(x = wdpa_raster_iucn,
            filename = here("data", "wdpa_raster_iucn.tif"),
            overwrite = T)


# END OF SCRIPT
