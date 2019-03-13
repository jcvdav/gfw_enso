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
  disaggregate(fact = 10) %>% 
  as.data.frame(xy = T) %>% 
  magrittr::set_colnames(value = c("longitude", "latitude", "tele_binary")) %>% 
  as_tibble() %>% 
  mutate(latitude = round(latitude, digits = 2),
         longitude = round(longitude, digits = 2))

# Read the gridded effort data (see scripts/download_gridded_ff_by_gear_country)
gridded_ff <- readRDS(file = here("raw_data", "gridded_ff_by_gear_country.rds"))

# nino34 index
nino34 <- read.csv(here::here("data","all_indices.csv"),
                   stringsAsFactors = F)%>%
  mutate(season = case_when(month_n %in% c(12, 1, 2) ~ "winter",
                            month_n %in% c(3, 4, 5) ~ "spring",
                            month_n %in% c(6, 7, 8) ~ "summer",
                            TRUE ~ "fall")) %>% 
  filter(year > 1950) %>% 
  mutate(date = lubridate::date(date)) %>% 
  select(year, month = month_n, date, nino34anom)

model_data <- gridded_ff %>% 
  filter(is_foreign) %>% 
  left_join(nino34, by = c("year", "month", "date")) %>% 
  left_join(treatment_3, by = c("longitude", "latitude")) %>% 
  rename(treated = tele_binary) %>% 
  mutate(treated = treated * 1,
         log_hours = log(hours + 1)) %>% #log transformation
  mutate(month = as.factor(month))

model_data2 <- model_data %>% 
  filter(best_label %in% c("drifting_longliness", "tuna_purse_seines")) %>% 
  drop_na(treated) %>% 
  group_by(year, month, date, eez_iso3, nino34anom, treated, best_label) %>% 
  summarize(hours = sum(hours)) %>% 
  ungroup() %>% 
  mutate(treated = treated == 1,
         region = ifelse(treated, "ENSO-teleconnected", "Not connected"))

ggplot(data = model_data2, mapping = aes(x = nino34anom, y = hours, color = region, group = treated)) +
  geom_smooth(method = "loess") +
  geom_smooth(method = "lm", linetype = "dashed", se = F) +
  # facet_wrap(~best_label, scales = "free_y", ncol = 1) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "NINO34 anom", y = "Foreign hours")

fit_all <- function(data){
  
  models <- list(base = "hours ~ nino34anom",
                 country = "hours ~ nino34anom + eez_iso3",
                 country_month = "hours ~ nino34anom + eez_iso3 + month")
  
  model_frame <- tibble(model = models) %>%
    mutate(model_name = names(model),
           model = map(model, as.formula),
           fit = map(model, ~lm(., data = data), data = data))
}

model1 <- model_data2 %>% 
  group_by(treated) %>% 
  nest() %>% 
  fit_all()















