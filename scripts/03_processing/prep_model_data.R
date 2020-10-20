########################
#   prep_model_data   ##
########################


# Load packages
library(startR)
library(raster)
library(here)
library(tidyverse)

# Analyses here
sst_nino34_cor_df <- readRDS(here::here("data", "sst_nino34_cor_df.rds"))

treatment_regions <- sst_nino34_cor_df %>% 
  group_by(longitude, latitude) %>% 
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
  gather(months, tele_binary, -c(longitude, latitude)) %>% 
  mutate(months = as.numeric(str_extract(string = months, pattern = "\\d+")))

treatment_3 <- treatment_regions %>% 
  filter(months == 3) %>% 
  select(longitude, latitude, tele_binary) %>% 
  rasterFromXYZ() %>% 
  as.data.frame(xy = T) %>% 
  magrittr::set_colnames(value = c("longitude", "latitude", "tele_binary")) %>% 
  as_tibble() %>% 
  mutate(latitude = round(latitude, digits = 2),
         longitude = round(longitude, digits = 2))

# Read the gridded effort data (see scripts/download_gridded_ff_by_gear_country)
gridded_ff <- readRDS(file = here("raw_data", "gridded_ff_by_gear_country.rds")) %>% 
  mutate(fishing = fishing == "TRUE",
         top_countries = top_countries == "TRUE")

# nino34 index
nino34 <- read.csv(here::here("data","all_indices.csv"),
                   stringsAsFactors = F)%>%
  mutate(season = case_when(month_n %in% c(12, 1, 2) ~ "winter",
                            month_n %in% c(3, 4, 5) ~ "spring",
                            month_n %in% c(6, 7, 8) ~ "summer",
                            TRUE ~ "fall")) %>% 
  filter(year > 2010) %>% 
  mutate(date = lubridate::date(date)) %>% 
  select(year, month = month_n, date, nino34anom)

# Create model data for purse seiners
model_data_ps <- gridded_ff %>% 
  filter(foreign,
         fishing,
         best_label == "tuna_purse_seines") %>%
  left_join(nino34, by = c("year", "month", "date")) %>% 
  left_join(treatment_3, by = c("longitude", "latitude")) %>% 
  rename(treated = tele_binary) %>% 
  mutate(treated = treated * 1,
         hours2 = log(hours + sqrt(1 + hours ^ 2))) %>% #Hyperbolic transformation
  mutate(month = as.factor(month))

# Create model data for longliners
model_data_ll <- gridded_ff %>% 
  filter(foreign,
         fishing,
         !best_label == "tuna_purse_seines") %>%
  left_join(nino34, by = c("year", "month", "date")) %>% 
  left_join(treatment_3, by = c("longitude", "latitude")) %>% 
  rename(treated = tele_binary) %>% 
  mutate(treated = treated * 1,
         hours2 = log(hours + sqrt(1 + hours ^ 2))) %>% #Hyperbolic transformation
  mutate(month = as.factor(month))

## Save the daa
saveRDS(object = model_data_ps,
        file = here("data", "model_data_ps"))

saveRDS(object = model_data_ll,
        file = here("data", "model_data_ll"))

