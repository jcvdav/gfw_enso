
# Load packages
library(here)
library(raster)
library(sf)
library(tidyverse)

# Ammend extract function
extract <- raster::extract

# Load variables and parameters used everywhere
source(here("scripts", "2_processing", "0_setup.R"))

# Layers in the rasters lose their names, so I'll recreate them
names <- expand.grid(month = c(1:12),
                     year = c(2012:2018)) %>% 
  as_tibble() %>% 
  mutate(date = paste(year, month, sep = ".")) %>% 
  pull(date)

## Load rasters
# Treatment regions
treatment_regions <- raster(here("data", "treatment_regions.tif"))
# Trawler effort
trawlers <- brick(here("data", "trawlers.tif"))
names(trawlers) <- names
# Seining effort
seiners <- brick(here("data", "seiners.tif"))
names(seiners) <- names

## Read WDPA data
wdpa <- read_sf(dsn = here("data", "wdpa_clean.gpkg"))
# Convert to spatial object to use in raster::extract
wdpa_sp <- as_Spatial(wdpa)

print("All data hasve been loaded")


## Extract data from rasters in parallel (sometimes)
# Extract treatment regions
wdpa_treatment_sp <- extract(x = treatment_regions,
                             y = wdpa_sp,
                             fun = mean,
                             na.rm = T,
                             df = T,
                             sp = T)

print("Treatment data have been extracted")

# Extract trawling effort
beginCluster(n = n_cores)
wdpa_trawling_sp <- extract(x = trawlers,
                            y = wdpa_sp,
                            fun = sum,
                            na.rm = T,
                            df = T,
                            sp = T)
endCluster()

print("Trawling data have been extracted")

# Extract seining effort
# beginCluster(n = n_cores)
# wdpa_seining_sp <- extract(x = seining,
#                             y = wdpa_sp,
#                             fun = mean,
#                             na.rm = T,
#                             df = T,
#                             sp = T)
# endCluster()

## Convert back to df, and then put together each extraction

wdpa_treatment <- wdpa_treatment_sp@data

wdpa_trawling <- wdpa_trawling_sp@data %>%
  as_tibble() %>% 
  gather(date, trawling, -c(WDPAID, IUCN, YEAR, ISO3, IUCN_INT, ISO3_INT)) %>% 
  mutate(date = str_remove(date, pattern = "X")) %>% 
  separate(col = date, sep = "\\.", into = c("year", "month")) %>% 
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         date = lubridate::date(paste(year, month, 1, sep = "-")))












all_indices <- read.csv(here("data","all_indices.csv"),
                        stringsAsFactors = F) %>%
  mutate(season = case_when(month_n %in% c(12, 1, 2) ~ "winter",
                            month_n %in% c(3, 4, 5) ~ "spring",
                            month_n %in% c(6, 7, 8) ~ "summer",
                            TRUE ~ "fall"))
## Keep nino34 index, and only year-month columns
nino34 <- all_indices %>% 
  select(year, month = month_n, nino34anom)





area <- wdpa %>% 
  mutate(area = st_area(.)) %>% 
  st_set_geometry(NULL)




wdpa_panel <- wdpa_treatment %>% 
  left_join(wdpa_trawling,
            by = c("WDPAID", "IUCN", "YEAR", "ISO3", "IUCN_INT", "ISO3_INT")) %>% 
  left_join(nino34, by = c("year", "month")) %>% 
  left_join(area, by = c("WDPAID", "IUCN", "YEAR", "ISO3", "IUCN_INT", "ISO3_INT")) %>% 
  mutate(trawling_norm = trawling / area)

# Write tha panel
write.csv(x = wdpa_panel,
          file = here("data", "wdpa_panel.csv"),
          row.names = F)




  
panel %>%
  mutate(treatment_regions = (treatment_regions >= 0.5),
         date = lubridate::date(date)) %>%
  drop_na() %>%
  group_by(year, month, date, nino34anom, treatment_regions) %>%
  summarize(trawling = mean(trawling, na.rm = T)) %>%
  filter(year >= 2015) %>% 
  ggplot(aes(x = nino34anom, y = trawling, color = treatment_regions)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  startR::ggtheme_plot()

wdpa_panel %>%
  mutate(treatment_regions = (treatment_regions >= 0.5),
         date = lubridate::date(date)) %>%
  drop_na() %>%
  group_by(year, month, date, nino34anom, treatment_regions) %>%
  summarize(trawling = mean(trawling, na.rm = T)) %>%
  filter(year >= 2015) %>% 
  ggplot(aes(x = date, y = trawling, color = treatment_regions)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  startR::ggtheme_plot() +
  labs(y = "Effort (hours / m2)")


a <- wdpa_panel %>%
  mutate(treatment_regions = (treatment_regions >= 0.5) * 1) %>%
  drop_na() %>% 
  filter(year >= 2015)

lm1 <- lm(trawling_norm ~ nino34anom * treatment_regions, data = a)
lm2 <- lm(trawling_norm ~ nino34anom * treatment_regions + ISO3 , data = a)
lm3 <- lm(trawling_norm ~ nino34anom * treatment_regions + ISO3 + as.factor(year), data = a)
lm4 <- lm(trawling_norm ~ nino34anom * treatment_regions + ISO3 + as.factor(year) + as.factor(month), data = a)

stargazer::stargazer(list(lm1, lm2, lm3, lm4), type = "html", out = "prelim.html", omit = c("ISO", "month", "year"))
  
  
  
  
  
  





