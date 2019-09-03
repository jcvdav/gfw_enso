
if(!require(here)){
  install.packages("here")
  library(here)
}

#### SET UP ####
# Install packages
source("1_setup.R")

#### PROCESSING ####
# Clean WDPA polygons and rasterize them
source(here("scripts", "2_processing", "1_clean_wdpa.R"))
# Estimate correlation between ENSO and SST at 2 degree resolution
source(here("scripts", "2_processing", "2_cor_sst_nino34_2deg.R"))
# Identify teleconnected regions and create raster
source(here("scripts", "2_processing", "3_identify_teleconnected_regions.R"))
# Create a fishing effort raster
source(here("scripts", "2_processing", "4_fishing_effort_rasters.R"))


#### Analyses ####