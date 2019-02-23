##########################
##    teleconnected    ##
##########################


####################################################
# This script produces the figures where each
# panel shows a teleconnection by number of months
# and by month, as well as the figure where
# only three months are shown
####################################################

library(startR)
library(here)
library(tidyverse)

# Load data
sst_nino34_cor_df <- readRDS(here::here("data", "sst_nino34_cor_df.rds"))

# Create a dataset with
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

# Read the coastline
world_coastline <- rnaturalearth::ne_countries(returnclass = "sf")
world_coastline2 <- rnaturalearth::ne_coastline(returnclass = "sf")

## ALL PANELS
# ENSO teleconnection depending on number of months. Number above figures indicate the minimum number of months for which a particular parcel was correlated to nino3 (red). For example, the panel 6 indicates that all red regions where SST showed a positive ( r > 0) and significant (p < 0.1) correlation with nino3 index for at least 6 months.
p <- ggplot() +
  ggtheme_map() +
  geom_raster(data = treatment_regions,
              aes(x = longitude, y = latitude, fill = as.factor(tele_binary*1))) +
  facet_wrap(~months, ncol = 3) +
  geom_sf(data = world_coastline, color = "transparent") +
  geom_sf(data = world_coastline2, color = "black") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")

ggsave(plot = p, filename = here("writing", "img", "cor_sst_nino34_var.pdf"),
       width = 6,
       height = 5)


## ALL PANELS 2
# Monthly correlations between SST and nino34 index. Numbers above each pannel indicate the month (1 = Jan, 12 = Dec). Red zones indicate the pearson's correlation coefficient was > 0 and p < 0.1.
p2 <- ggplot() +
  ggtheme_map() +
  geom_raster(data = sst_nino34_cor_df,
              aes(x = longitude, y = latitude, fill = as.factor(tele))) +
  facet_wrap(~month, ncol = 3) +
  geom_sf(data = world_coastline, color = "transparent") +
  geom_sf(data = world_coastline2, color = "black") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")

# Export plot
ggsave(plot = p2,
       filename = here("writing", "img", "cor_sst_nino34.pdf"),
       width = 6,
       height = 5)


# JUST THREE MONTHS
# ENSO-Teleconnected marine regions. Red and blue indicate the when where SST showed a positive (r < 0) and significant (p \textless{} 0.1) correlation with NINO3 index for at least 3 months. White areas represent areas where SST data is no available.

p3 <- filter(treatment_regions, months == 3) %>% 
  ggplot() +
  ggtheme_map() +
  geom_raster(aes(x = longitude, y = latitude, fill = as.factor(tele_binary*1))) +
  geom_sf(data = world_coastline, color = "transparent") +
  geom_sf(data = world_coastline2, color = "black") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")

# Export plot
ggsave(plot = p3,
       filename = here("writing", "img", "teleconnected_3months.pdf"),
       width = 6,
       height = 5)
