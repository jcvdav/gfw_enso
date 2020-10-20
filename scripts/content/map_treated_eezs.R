#########################
#   map_treated_eezs    #
#########################

library(startR)
library(lubridate)
library(cowplot)
library(here)
library(sf)
library(tidyverse)

## Load data

# Load exclusive eocnomic zones
eezs <- read_sf(dsn = here("raw_data", "spatial", "EEZ"),
                layer = "eez_v10") %>% 
  select(ISO_Ter1)

## Load eez sst correlation data
eez_sst_nino34_cor_df <- read.csv(here::here("data", "eez_sst_nino34_cor_df.csv"),
                                  stringsAsFactors = F)

# Polygon for the nino3.4 region
nino4_region <- tibble(lon = c(170, 120, 120, 170),
                       lat = c(5, 5, -5, -5))

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
  select(year, month = month_n, nino34anom) %>% 
  mutate(date = lubridate::date(paste(year, month, 1, sep = "-")))

## Modify data
# Identify treated EEZs by number of months
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

# Identify EEZs where SST for at least 3 months per year being explained by NINO34ANOM
treatment_3 <- treatment_regions %>% 
  filter(months == 3) %>% 
  mutate(group = ifelse(tele_binary, "Teleconnected", "Weakly affected")) %>% 
  select(ISO_Ter1, tele_binary, group)

# Create plotting data
eezs_connected <- eezs %>% 
  left_join(treatment_3, by = "ISO_Ter1")

map <- ggplot() +
  geom_sf(data = eezs_connected,
          aes(fill = group),
          color = "black", size = 0.1) +
  geom_polygon(data = nino4_region,
               mapping = aes(x = lon, y = lat),
               fill = "transparent",
               color = "black",
               linetype = "dashed",
               size = 1) +
  scale_fill_brewer(palette = "Set1") +
  ggtheme_map() +
  theme(legend.position = "None")

nino34 <- ggplot(data = nino34test) +
  geom_rect(aes(xmin = date("2012-01-01"),
                xmax = date("2018-12-01"),
                ymin = -Inf, ymax = Inf),
            fill = "lightgray") +
  geom_line(aes(x = date,
                y = nino34anom,
                color = nino34anom),
            size = 1) +
  scale_color_gradientn(colours = colorRamps::matlab.like(10)) +
  ggtheme_plot() +
  guides(color = guide_colorbar(title = "NINO3.4\nAnomaly",
                                frame.colour = "black",
                                ticks.colour = "black")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Date", y = "NINO3.4\nAnomaly")

p <- plot_grid(map, nino34, ncol = 1, labels = "AUTO", rel_heights = c(2, 1))

ggsave(p,
       filename = here("writing", "img", "map_treated_eezs.png"),
       width = 6,
       height = 5)



















