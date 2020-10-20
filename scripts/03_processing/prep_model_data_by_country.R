###################################
#   prep_model_data_by_country   ##
###################################


# Load packages
library(startR)
library(raster)
library(here)
library(tidyverse)

# Analyses here
eez_sst_nino34_cor_df <- read.csv(here::here("data", "eez_sst_nino34_cor_df.csv"),
                                  stringsAsFactors = F)

treatment_regions <- eez_sst_nino34_cor_df %>% 
  group_by(ISO_Ter1) %>% 
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
  gather(months, tele_binary, -ISO_Ter1) %>% 
  mutate(months = as.numeric(str_extract(string = months, pattern = "\\d+")))

treatment_3 <- treatment_regions %>% 
  filter(months == 3) %>% 
  select(ISO_Ter1, tele_binary)

# Read the effort data 
country_ff <- readRDS(file = here("raw_data", "ff_by_gear_country.rds")) %>% 
  mutate(fishing = fishing == "TRUE",
         top_countries = top_countries == "TRUE") %>% 
  mutate(date = lubridate::date(paste(year, month, 1, sep = "-")))

# nino34 index
nino34 <- read.csv(here::here("data", "all_indices.csv"),
                   stringsAsFactors = F)%>%
  mutate(season = case_when(month_n %in% c(12, 1, 2) ~ "winter",
                            month_n %in% c(3, 4, 5) ~ "spring",
                            month_n %in% c(6, 7, 8) ~ "summer",
                            TRUE ~ "fall")) %>% 
  filter(year > 2010) %>% 
  mutate(date = lubridate::date(date)) %>% 
  select(year, month = month_n, date, nino34anom)

# Create model data for purse seiners
model_data_ps <- country_ff %>% 
  filter(foreign,
         # fishing,
         best_vessel_class == "tuna_purse_seines") %>% 
  group_by(year, month, date, eez_iso3, top_countries) %>%
  mutate(total_hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(hours_pct = hours / total_hours) %>% 
  filter(fishing) %>% 
  left_join(nino34, by = c("year", "month", "date")) %>% 
  left_join(treatment_3, by = c("eez_iso3" = "ISO_Ter1")) %>% 
  rename(treated = tele_binary) %>% 
  mutate(treated = treated * 1,
         hours2 = log(hours + sqrt(1 + hours ^ 2))) %>% #Hyperbolic transformation
  mutate(month = as.factor(month))

# Create model data for longliners
model_data_ll <- country_ff %>% 
  filter(foreign,
         # fishing,
         !best_vessel_class == "tuna_purse_seines") %>%
  group_by(year, month, date, eez_iso3, top_countries) %>%
  mutate(total_hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(hours = hours / total_hours) %>% 
  filter(fishing) %>% 
  left_join(nino34, by = c("year", "month", "date")) %>% 
  left_join(treatment_3, by = c("eez_iso3" = "ISO_Ter1")) %>% 
  rename(treated = tele_binary) %>% 
  mutate(treated = treated * 1,
         hours2 = log(hours + sqrt(1 + hours ^ 2))) %>% #Hyperbolic transformation
  mutate(month = as.factor(month))

## Save the daa
saveRDS(object = model_data_ps,
        file = here("data", "model_data_ps.rds"))

saveRDS(object = model_data_ll,
        file = here("data", "model_data_ll.rds"))

