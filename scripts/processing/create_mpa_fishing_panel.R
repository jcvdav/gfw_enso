

treatment_regions <- raster(here("data", "treatment_regions.tif"))

wdpa <- read_sf(here("data", "wdpa_clean"))

my_extract <- function(pol, rast) {
  # Convert to spatial polygon
  pol <- as_Spatial(pol)
  
  # Perform extraction
  pol <- raster::extract(x = rast,
                         y = pol,
                         method = "simple",
                         small = T,
                         fun = mean,
                         na.rm = T,
                         df = T,
                         sp = T)
  
  # Convert spatial to sf again
  as(pol, "sf")
}


tic()
wdpa_teleconnected <- wdpa %>% 
  head(1000) %>% 
  my_extract(rast = treatment_regions) %>% 
  mutate(treatment_regions = ifelse(treatment_regions >= 0.5,
                                    "Teleconnected",
                                    "Not teleconnected"),
         tele = treatment_regions >= 0.5)
toc()


