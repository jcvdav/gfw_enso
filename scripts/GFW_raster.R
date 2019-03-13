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
gridded_ff <- readRDS(file = here::here("raw_data", "gridded_ff_by_gear_country.rds"))

# Plot the raster
p <- gridded_ff %>% 
  filter(hours > 0) %>% 
  group_by(foreign, latitude, longitude) %>% 
  summarize(hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(foreign = ifelse(foreign, "Foreign", "Local")) %>% 
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
