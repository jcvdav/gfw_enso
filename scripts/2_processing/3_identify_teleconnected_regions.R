# identify teleconnected regions

# Load packages
library(here)
library(raster)
library(tidyverse)

# Load variables and parameters used everywhere
source(here("scripts", "2_processing", "0_setup.R"))

# Read correlations dataframe
sst_df <- read.csv(here("data", "cor_sst_nino34_2deg.csv"))

# Read the base raster
base_raster <- raster(here("data", "base_raster.tif"))

# Calculate different definitions of regions
# tele_1 means that one month per year was teleconnected
# tele_3 means that 3 months per year were teleconnected...
treatment_regions_by_n_months <- sst_df %>% 
  group_by(x, y) %>% 
  summarize(tele = sum(tele)) %>% 
  ungroup() %>% 
  mutate(tele_1 = tele >= 1,
         tele_2 = tele >= 2,
         tele_3 = tele >= 3,
         tele_4 = tele >= 4,
         tele_5 = tele >= 5,
         tele_6 = tele >= 6,
         tele_7 = tele >= 7,
         tele_8 = tele >= 8,
         tele_9 = tele >= 9,
         tele_10 = tele >= 10,
         tele_11 = tele >= 11,
         tele_12 = tele >= 12) %>% 
  select(-tele) %>% 
  gather(months, tele_binary, -c(x, y)) %>% 
  mutate(months = as.numeric(str_remove(months, "tele_")))

# We'll go with te same definition as Hsiang,
# using 3 months as a definition for teleconnection
treatment_regions <- treatment_regions_by_n_months %>% 
  filter(months == 3) %>% 
  select(x, y, tele_binary) %>% 
  rasterFromXYZ(crs = proj_lonlat) %>% 
  projectRaster(base_raster,
                crs = proj_beh,
                method = "ngb",
                over = T)

# Save the raster
writeRaster(x = treatment_regions,
            filename = here("data", "treatment_regions.tif"),
            overwrite = T)

# END OF SCRIPT







  