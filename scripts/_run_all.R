
library(here)

#### PROCESSING ####
# Clean WDPA polygons and rasterize them
source(here("scripts", "processing", "1_clean_wdpa.R"))
# Estimate correlation between ENSO and SST at 2 degree resolution
source(here("scripts", "processing", "2_cor_sst_nino34_2deg.R"))
# Identify teleconnected regions and create raster
source(here("scripts", "processing", "3_identify_teleconnected_regions.R"))
# Create a fishing effort raster
source(here("scripts", "processing", "4_fishing_effort_rasters.R"))


#### Analyses ####