
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
geometry_precision <- 500

# Load the WDPA dataset
## Polygons
wdpa_polygons <- read_sf(dsn = here("raw_data", "spatial", "WDPA_marine"),
                layer = "WDPA_marine_polygons") %>% 
  filter(MARINE > 0,
         !str_detect(tolower(MANG_PLAN), "non-mpa"),
         STATUS %in% c("Designated", "Inscribed", "Established"),
         STATUS_YR <= 2012,
         !DESIG_TYPE %in% c("International", "Not Applicable")) %>% 
  mutate(NO_TAKE = ifelse(WDPAID == 309888, "All", NO_TAKE),
         no_take = (NO_TAKE == "All") | (NO_TAKE == "Part" & NO_TK_AREA > 0.75 * GIS_M_AREA)) %>% 
  filter(no_take,
         !WDPAID %in% c(309888, 478191)) %>% 
  st_set_precision(geometry_precision) %>%
  st_transform(crs = proj) %>% 
  st_set_precision(geometry_precision) %>% 
  select(WDPAID, WDPA_PID, PA_DEF, NAME, DESIG, IUCN_CAT, STATUS, STATUS_YR, ISO3) %>% 
  lwgeom::st_make_valid()

## Points
wdpa_points <- read_sf(dsn = here("raw_data", "spatial", "WDPA_marine"),
                         layer = "WDPA_marine_points") %>% 
  filter(MARINE > 0,
         !str_detect(tolower(MANG_PLAN), "non-mpa"),
         STATUS %in% c("Designated", "Inscribed", "Established"),
         STATUS_YR <= 2012,
         !DESIG_TYPE %in% c("International", "Not Applicable")) %>% 
  filter(is.finite(REP_AREA),
         REP_M_AREA > 0.25) %>% 
  st_transform(crs = proj) %>% 
  st_buffer(dist = sqrt((.$REP_AREA * 1e6) / pi)) %>% 
  mutate(no_take = (NO_TAKE == "All") | (NO_TAKE == "Part" & NO_TK_AREA > 0.75 * REP_M_AREA)) %>%
  filter(no_take) %>% 
  select(WDPAID, WDPA_PID, PA_DEF, NAME, DESIG, IUCN_CAT, STATUS, STATUS_YR, ISO3)

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

## Combine points and polygons
wdpa <- #rbind(wdpa_polygons, wdpa_points) %>% 
  wdpa_polygons %>% 
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
  filter(!st_is_empty(.)) %>% 
  group_by(WDPAID, IUCN_CAT, STATUS_YR, ISO3) %>% 
  summarize(a = 1) %>% 
  select(-a) %>% 
  mutate(IUCN_CAT_int = iucn_cats[IUCN_CAT],
         ISO_int = countrycode::countrycode(sourcevar = ISO3,
                                            origin = "iso3c",
                                            destination = "iso3n")) %>% 
  st_cast(to = "POLYGON")

# Save clean shapefile
st_write(obj = wdpa,
         dsn = here("data"),
         layer = "wdpa_clean",
         driver = "ESRI Shapefile",
         delete_dsn = T)

# Save a csv version without geometries
wdpa_csv <- wdpa %>% 
  st_set_geometry(NULL)

write.csv(x = wdpa_csv,
          file = here("data", "wdpa_clean.csv"),
          row.names = F)

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
                      res = 1000,
                      val = 1L,
                      crs = proj)

# Create a raster
## We will create a different raster for each column
## of interest in wdpa, and then stack them together


### First, just inside / outside MPA
wdpa_raster_is_mpa <- fasterize(sf = wdpa,
                                raster = base_raster,
                                background = NA)
names(wdpa_raster_is_mpa) <- "is_mpa"

### Now, one for WDPA id
wdpa_raster_wdpaid <- fasterize(sf = wdpa,
                                raster = base_raster,
                                field = "WDPAID",
                                background = NA)
names(wdpa_raster_wdpaid) <- "WDPAID"


### Now one for country ISO
wdpa_raster_iso <- fasterize(sf = wdpa,
                             raster = base_raster,
                             field = "ISO_int",
                             background = NA)
names(wdpa_raster_iso) <- "ISO"

### Now one for IUCN category
wdpa_raster_iucn <- fasterize(sf = wdpa,
                              raster = base_raster,
                              field = "IUCN_CAT_int",
                              background = NA)
names(wdpa_raster_iucn) <- "IUCN_CAT"


# Save the rasters
writeRaster(x = wdpa_raster_is_mpa,
            filename = here("data", "wdpa_raster_is_mpa.tiff"),
            overwrite = T)

writeRaster(x = wdpa_raster_wdpaid,
            filename = here("data", "wdpa_raster_wdpaid.tiff"),
            overwrite = T)

writeRaster(x = wdpa_raster_iso,
            filename = here("data", "wdpa_raster_iso.tiff"),
            overwrite = T)

writeRaster(x = wdpa_raster_iucn,
            filename = here("data", "wdpa_raster_iucn.tiff"),
            overwrite = T)


# END OF SCRIPT
