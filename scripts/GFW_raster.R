#####################
##    GFW_raster   ##
#####################


####################################################
# Create a figure of foreign and not foreign fishing
# effort for tuna purse seiners
####################################################

# Load packages
library(startR)
library(raster)
library(lubridate)
library(here)
library(tidyverse)

# Read the coastline
world_coastline <- rnaturalearth::ne_countries(returnclass = "sf")
world_coastline2 <- rnaturalearth::ne_coastline(returnclass = "sf")

# Read the fishing effort
gridded_ff <- readRDS(file = here::here("raw_data", "gridded_ff_by_gear_country.rds")) %>% 
  filter(best_label %in% c("drifting_longlines", "set_longlines", "drifting_longlines|set_longlines", "tuna_purse_seines", "other_purse_seines", "purse_seines")) %>% 
  mutate(date = date(paste(year, month, "01", sep = "/")),
         best_label = ifelse(str_detect(best_label, "purse"), "purse_seines", best_label))

# Plot the raster
p <- gridded_ff %>% 
  filter(hours > 0) %>% 
  group_by(is_foreign, latitude, longitude) %>% 
  summarize(hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(is_foreign = ifelse(is_foreign, "Foreign", "Local")) %>% 
  ggplot() +
  ggtheme_map() +
  theme_dark() +
  geom_raster(aes(x = longitude, y = latitude, fill = hours)) +
  geom_sf(data = world_coastline, fill = "black") +
  facet_wrap(~is_foreign, ncol = 2) +
  scale_fill_viridis_c(trans = "log10") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "", fill = "Fishing hours")

# Save figure
ggsave(p,
       file = here("writing", "img", "GFW_raster.pdf"),
       width = 9,
       height = 6)
