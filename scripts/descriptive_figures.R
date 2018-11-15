# This script produces a series of descriptive figures for our data.
# Figures are exported to the /img folder in the git repository. 
# Some of these are used in our manuscript aither as main figures or
# supplementary information.

# Load libraries
suppressPackageStartupMessages({
  library(startR)
  library(lubridate)
  library(here)
  library(tidyverse)
})



# Read the gridded effort data (see scripts/download_gridded_ff_by_gear_country)
gridded_ff <- readRDS(file = here("raw_data", "gridded_ff_by_gear_country.rds"))

# Read the coastline
world_coastline <- rnaturalearth::ne_countries(returnclass = "sf")

#GFW data
# Fishing effort (hours) by gear and foreign fishing
p <- gridded_ff %>% 
  filter(hours > 0) %>% 
  group_by(best_label_short, is_foreign, latitude, longitude) %>% 
  summarize(hours = sum(hours, na.rm = T)) %>% 
  ggplot() +
  ggtheme_map() +
  geom_raster(aes(x = longitude, y = latitude, fill = hours)) +
  scale_fill_viridis_c(trans = "log10") +
  facet_grid(is_foreign~best_label_short) +
  theme_dark() +
  geom_sf(data = world_coastline, fill = "black") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "FF and non-FF by gear", x = "", y = "", fill = "Fishing hours")

ggsave(plot = p, filename = here("img", "fishing_raster.pdf"), width = 6, height = 4.5)

#ENSO data

nino3 <- read.csv(here::here("data","all_indices.csv"),
                  stringsAsFactors = F)%>%
  mutate(season = case_when(month_n %in% c(12, 1, 2) ~ "winter",
                            month_n %in% c(3, 4, 5) ~ "spring",
                            month_n %in% c(6, 7, 8) ~ "summer",
                            TRUE ~ "fall")) %>% 
  filter(year > 1950) %>% 
  mutate(date = lubridate::date(date)) %>% 
  select(year, month = month_n, date, nino3, nino3anom)

# Timeseries of nino3 index (detrended) for A) The entire length and B) timespan matching GFW data
plot1 <- nino3 %>%
  ggplot(aes(x = date, y = nino3anom)) +
  ggtheme_plot() +
  geom_rect(aes(xmin = date("2012-01-01"), xmax = date("2018-01-01"), ymin = -Inf, ymax = Inf), fill = "lightgray") +
  geom_text(x = date("2014-12-01"), y = 3.2, label = "GFW") +
  geom_line(aes(color = nino3anom)) +
  scale_color_gradientn(colours = colorRamps::blue2red(20)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "nino3", color = "nino3")

plot2 <- nino3 %>% 
  filter(year > 2011) %>% 
  ggplot(aes(x = date, y = nino3anom)) +
  ggtheme_plot() +
  geom_line(aes(color = nino3anom)) +
  scale_color_gradientn(colours = colorRamps::blue2red(20)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "nino3", color = "nino3")

p <- cowplot::plot_grid(plot1, plot2, ncol = 1, labels = "AUTO")

ggsave(plot = p, filename = here("img", "nino_gfw.pdf"), width = 6, height = 4.5)


# Mean monthly SST
sst_df_whole <- readRDS(here::here("data", "sst_df_whole.rds"))

p <- sst_df_whole %>% 
  group_by(month, longitude, latitude) %>% 
  summarize(sst = mean(sst, na.rm = T)) %>%
  ggplot() +
  ggtheme_map() +
  geom_sf(data = world_coastline, fill = "grey96", color = "grey40", size = .10) +
  geom_raster(aes(x = longitude, y = latitude, fill = sst)) +
  facet_wrap(~month, ncol = 3) +
  scale_fill_gradientn(colours = colorRamps::matlab.like(50))

ggsave(plot = p, filename = here("img", "monthly_sst.pdf"), width = 6, height = 5)

rm(sst_df_whole)

#Trends in fishing hours by gear and foreign
p <- gridded_ff %>% 
  group_by(date, is_foreign, best_label_short) %>% 
  summarize(hours = sum(hours)) %>% 
  left_join(nino3, by = "date") %>% 
  ggplot() +
  ggtheme_plot() +
  geom_line(aes(x = date, y = hours, group = is_foreign, color = nino3anom)) +
  scale_color_gradientn(colours = colorRamps::blue2red(20)) +
  facet_wrap(is_foreign~best_label_short, scales = "free") +
  labs(x = "Date", y = "hours", color = "nino3")

ggsave(plot = p, filename = here("img", "trends_by_gear.pdf"), width = 6, height = 4.5)



































