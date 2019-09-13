# Identify teleconnected EEZs

# Load packages
library(here)
library(startR)
library(raster)
library(sf)
library(rnaturalearth)
library(tidyverse)

# Load variables and parameters used everywhere
source(here("scripts", "2_processing", "0_setup.R"))

# Load treatment regions
treatment_pixels_lonlat <- raster(here("data", "treatment_pixels_lonlat.tif"))

eez <- st_read(dsn = here("data", "eez_v10"),
               layer = "eez_v10") %>% 
  select(MRGID, GeoName, ISO_Ter1) %>% 
  mutate(Area = st_area(.)) %>% 
  filter(!st_is_empty(.))

eez_treat <- raster::extract(x = treatment_pixels_lonlat,
                             y = as_Spatial(eez),
                             fun = mean,
                             na.rm = T,
                             df = T,
                             sp = T)

prop_na <- function(x, ...) {
  n_na <- sum(is.na(x))
  n <- length(x)
  n_na / n
}

eez_isna <- raster::extract(x = treatment_pixels_lonlat,
                            y = as_Spatial(eez),
                            fun = prop_na,
                            df = T,
                            sp = T)

colnames(eez_isna@data) <- c("MRGID", "GeoName", "ISO_Ter1", "Area", "Prop_na")

eez2 <- eez %>% 
  left_join(eez_treat@data, by = c("MRGID", "GeoName", "ISO_Ter1", "Area")) %>% 
  left_join(eez_isna@data, by = c("MRGID", "GeoName", "ISO_Ter1", "Area")) %>%
  mutate(status = ifelse(treatment_pixels_lonlat >= 0.5, "TE", "WA"))

# Save teleconected shapefile
st_write(obj = eez2,
         dsn = here("data", "teleconnected_eez.gpkg"),
         driver = "GPKG",
         delete_dsn = T)

# Get a coastline
coast <- ne_countries(returnclass = "sf") %>% 
  sf::st_combine()

teleconnected_eez <- 
  ggplot() +
  geom_sf(data = eez2, aes(fill = status), color = "black") +
  geom_sf(data = coast, color = "black", ) +
  ggtheme_map() +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

ggsave(plot = teleconnected_eez,
       filename = here("docs", "img", "teleconnected_eez.png"),
       width = 6,
       height = 3)
