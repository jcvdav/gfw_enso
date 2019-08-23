
# Load packages
library(startR)
library(lubridate)
library(here)
library(raster)
library(tidyverse)

# Set working parameters
proj <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
check <- FALSE # Set to TRUE to run plots at the end

# Load data ############################
## NINO indices
all_indices <- read.csv(here("data","all_indices.csv"),
                        stringsAsFactors = F) %>%
  mutate(season = case_when(month_n %in% c(12, 1, 2) ~ "winter",
                            month_n %in% c(3, 4, 5) ~ "spring",
                            month_n %in% c(6, 7, 8) ~ "summer",
                            TRUE ~ "fall"))

## Load SST data
r <- brick("raw_data/spatial/sst.mnmean.nc")


# Data processing ############################
## Keep nino34 index, and only year-month columns
nino34test <- all_indices %>% 
  select(year, month = month_n, nino34anom)

## Find name of rasters of interest
## (rasters after 1870)
brick_info <- tibble(name = names(r)) %>% 
  mutate(name_n = str_remove(name, "X")) %>% 
  separate(name_n, sep = "\\.", into = c("year", "month", "day")) %>% 
  filter(year > 1869)

## Extract rasters of interest and reproject
r_int <- r[[brick_info$name]] %>% 
  rotate() %>% 
  projectRaster(crs = proj)

## Create a data.frame and calculate
## correlations for every month and pixel
sst_df <- as.data.frame(r_int, xy = T) %>% 
  drop_na() %>% 
  gather(name, sst, -c(x, y)) %>% 
  mutate(name_n = str_remove(name, "X")) %>% 
  separate(name_n, sep = "\\.", into = c("year", "month", "day")) %>% 
  mutate_at(c("year", "month"), as.numeric) %>% 
  left_join(nino34test, by = c("year", "month")) %>% 
  group_by(x, y, month) %>%
  summarize(mean_sst = mean(sst),
            r = cor.test(sst, nino34anom)$estimate,
            p = cor.test(sst, nino34anom)$p.value) %>% 
  ungroup() %>%  
  mutate(tele = ifelse((p < 0.05 & r > 0), 1, 0))

# Export the data
write.csv(x = sst_df,
          file = here("data", "cor_sst_nino34_2deg.csv"),
          row.names = F)

if(check){
  # Mean monthly SST
  ggplot(sst_df, aes(x = x, y = y, fill = mean_sst)) +
    geom_raster() +
    facet_wrap(~month) +
    coord_equal() + 
    scale_fill_gradientn(colours = colorRamps::matlab.like(20)) + 
    ggtheme_map()
  
  # Correlation by month
  ggplot(sst_df, aes(x = x, y = y, fill = r)) +
    geom_raster() +
    facet_wrap(~month) +
    coord_equal() + 
    scale_fill_gradientn(colours = colorRamps::matlab.like(20)) + 
    ggtheme_map()
  
  # p-value in correlation
  ggplot(sst_df, aes(x = x, y = y, fill = p)) +
    geom_raster() +
    facet_wrap(~month) +
    coord_equal() + 
    scale_fill_gradientn(colours = colorRamps::matlab.like(20)) + 
    ggtheme_map()
  
  # Binary for teleconnection
  ggplot(sst_df, aes(x = x, y = y, fill = tele)) +
    geom_raster() +
    facet_wrap(~month) +
    coord_equal() + 
    scale_fill_gradientn(colours = colorRamps::matlab.like(20)) +
    ggtheme_map()
  
  # Correlation for significant correlations only
  ggplot(sst_df %>% mutate(r = ifelse(p > 0.05, NA, r)), aes(x = x, y = y, fill = r)) +
    geom_raster() +
    facet_wrap(~month) +
    coord_equal() + 
    scale_fill_gradientn(colours = colorRamps::matlab.like(20)) +
    ggtheme_map() 
}

