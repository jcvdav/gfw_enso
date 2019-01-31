# Load packages
library(startR)
library(lubridate)
library(here)
library(raster)
library(magrittr)
library(tidyverse)

# Load indices
all_indices <- read.csv(here("data","all_indices.csv"),
                        stringsAsFactors = F) %>%
  mutate(season = case_when(month_n %in% c(12, 1, 2) ~ "winter",
                            month_n %in% c(3, 4, 5) ~ "spring",
                            month_n %in% c(6, 7, 8) ~ "summer",
                            TRUE ~ "fall"))
# Keep nino3 index after 2002, and only year-month columns
nino3test <- all_indices %>% 
  filter(year > 2002) %>% 
  select(year, month = month_n, nino34anom)

# Identify the names of all rasters, and extract their year
rasters <- paste0(here("raw_data",
                       "spatial",
                       "temp",
                       list.files(path = here("raw_data",
                                              "spatial",
                                              "temp"),
                                  pattern = "*.nc"))) %>% 
  tibble() %>% 
  magrittr::set_colnames(value = "path") %>% 
  mutate(year = str_extract(string = path, pattern = "\\d{4}")) %>% 
  mutate(year = as.numeric(year),
         month = rep(1:12, 15))

#print to check progress
print("Read all raster names.")
print("Proceeding to read all rasters...")

# Read all rasters as a stack of rasters, delete areas with land (T > 35) and reduce resolution by a factor of 25
r <- stack(rasters$path, varname = "sst4") %>%
  mask(mask = . > 35, maskvalue = T) %>%
  aggregate(fact = 2.4)

#print to check progress
print("Read all rasers.")
print("Proceeding to reproject rasters...")

r <- projectRaster(r, crs = crs(r), res = 0.1)

#print to check progress
print("Rastrers reprojected.")
print("Proceed to create the entire data.frame...")

# Delete heavy objects so that the next opperations fit in ram (about 14 GB needed)
rm(all_indices)
gc()

# From the raster names, generate layer names to join raster data witn nino3 data
layers <- paste(rasters$year, rasters$month, sep = "-")

# Convert matrix of SST into a tibble with columns year, month, sst, nino3.

sst_df <- coordinates(r) %>% 
  cbind(values(r)) %>% 
  as.data.frame()

rm(r)
gc()

sst_df %<>% 
  set_colnames(value = c("longitude", "latitude", layers)) %>%
  gather(date, sst, -c(longitude, latitude)) %>% 
  mutate(year = str_extract(string = date, pattern = "\\d{4}"),
         month  = str_extract(string = date, pattern = "-\\d{1,}"),
         month = str_remove(string = month, pattern = "-")) %>% 
  mutate(year = as.numeric(year),
         month = as.numeric(month)) %>% 
  drop_na() %>% 
  left_join(nino3test, by = c("year", "month"))

saveRDS(object = sst_df, file = here("data", "sst_df_whole.rds"))

#print to check progress
print("Whole data.frame created and saved.")
print("Proceed to create the correlations data.frame...")

# Then group by pixel and calculate the correlation between SST and nino 3 for every pixel.
sst_df %<>%
  group_by(longitude, latitude, month) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 3) %>%
  group_by(longitude, latitude, month) %>%
  summarize(mean_sst = mean(sst),
            r = cor.test(sst, nino3anom)$estimate,
            p = cor.test(sst, nino3anom)$p.value) %>% 
  ungroup() %>%  
  mutate(tele = ifelse((p < 0.1 & r > 0), 1, 0))

# Export RDS object
saveRDS(object = sst_df, file = here("data", "sst_nino3_df.rds"))

#print to check progress
print("Correlations data.frame created and saved.")
print("Proceed to plot...")

# Read the coastline
world_coastline <- rnaturalearth::ne_countries(returnclass = "sf")

# Monthly correlations between SST and nino3 index. Numbers above each pannel indicate the month (1 = Jan, 12 = Dec). Red zones indicate the pearson's correlation coefficient was > 0 and p < 0.1.
p <- ggplot() +
  ggtheme_map() +
  geom_sf(data = world_coastline, fill = "grey96", color = "grey40", size = .10) +
  geom_raster(data = sst_df, aes(x = longitude, y = latitude, fill = as.factor(tele))) +
  facet_wrap(~month, ncol = 3) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  theme(legend.position = "none")

# Export plot
ggsave(plot = p, filename = here("img", "cor_sst_nino3.pdf"), width = 6, height = 5)
