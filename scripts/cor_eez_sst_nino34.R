#rasterize eezs

library(here)
library(raster)
library(sf)
library(furrr)
library(tidyverse)

# Function to fix days in parallel
fix_dates <- function(x){
  x %>% 
    mutate(year = str_extract(string = date, pattern = "\\d{4}"),
           month  = str_extract(string = date, pattern = "-\\d{1,}"),
           month = str_remove(string = month, pattern = "-"),
           year = as.numeric(year),
           month = as.numeric(month))
}

# Load indices
all_indices <- read.csv(here("data","all_indices.csv"),
                        stringsAsFactors = F) %>%
  mutate(season = case_when(month_n %in% c(12, 1, 2) ~ "winter",
                            month_n %in% c(3, 4, 5) ~ "spring",
                            month_n %in% c(6, 7, 8) ~ "summer",
                            TRUE ~ "fall"))

# Keep nino34 index after 2002, and only year-month columns
nino34test <- all_indices %>% 
  filter(year > 2002) %>% 
  select(year, month = month_n, nino34anom)

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

r <- stack(rasters$path, varname = "sst4") %>% 
  aggregate(fact = 24)

# From the raster names, generate layer names to join raster data witn nino34 data
layers <- paste(rasters$year, rasters$month, sep = "-")

# Read exclusive economic zones
eez <- read_sf(dsn = here("raw_data", "spatial", "EEZ"),
               layer = "eez_v10")

# Convert eez to sp object for friendly extraction
eez_sp <- as_Spatial(eez)

# This produces a matrix with one row per
# eez and one column per raster image (year, month)
v <- extract(r,
             eez_sp,
             method = "simple",
             fun = mean,
             na.rm = T)

# This combines the eez and monthly temperatures into a
# tidy data.frame
eez_sst_nino34 <- eez %>% 
  select(ISO_Ter1) %>% 
  st_set_geometry(NULL) %>% 
  cbind(v) %>% 
  magrittr::set_colnames(value = c("ISO_Ter1", layers)) %>% 
  gather(date, sst, -ISO_Ter1) %>% 
  split(.$date) %>%
  future_map(drop_na) %>% 
  future_map_dfr(fix_dates) %>% 
  left_join(nino34test, by = c("year", "month"))

ggplot(eez_sst_nino34,
       aes(x = lubridate::date(paste(year, month, 15, sep = "-")), y = sst, group = ISO_Ter1)) +
  geom_line()

eez_sst_nino34_cor_df <- eez_sst_nino34 %>% 
  group_by(ISO_Ter1, month) %>%
  summarize(mean_sst = mean(sst),
            r = cor.test(sst, nino34anom)$estimate,
            p = cor.test(sst, nino34anom)$p.value) %>% 
  ungroup() %>%  
  mutate(tele = ifelse((p < 0.1 & r > 0), 1, 0))

# I can simplify the EEZs and then add the df above
# simple_eez <- eez %>%
#   st_simplify(dTolerance = 1)
# 
# Then I can produce a plot for the correlation between nino34
# and sst for all the months of january, for example:
# simple_eez %>% 
#   select(ISO_Ter1) %>% 
#   left_join(eez_sst_nino34_cor_df, by = "ISO_Ter1") %>% 
#   filter(month == 1) %>% 
#   ggplot(aes(fill = r)) + 
#   geom_sf() +
#   scale_fill_viridis_c(option = "A")

write.csv(x = eez_sst_nino34_cor_df,
          file = here("data", "eez_sst_nino34_cor_df.csv"),
          row.names = F)

