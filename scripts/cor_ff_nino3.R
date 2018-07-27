

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
})

# Load gridded fishing effort, filter by gear and calculate the proportion of fishing for every pixel (proportion of all fishing in a given month)
gridded_ff <- read_csv(file = here("raw_data", "GFW", "gridded_ff_by_gear_country.csv"),
                       col_types = cols(), progress = F) %>% 
  filter(best_label %in% c("drifting_longlines")) %>% 
  mutate(lon_bin_center = round(lon_bin_center),
         lat_bin_center = round(lat_bin_center)) %>% 
  group_by(year, month) %>% 
  mutate(total_hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  filter(is_foreign) %>% # filter(!is_foreign) %>% # Use this instead to see not foreign fishing (high seas)
  mutate(prop_hours = hours / total_hours) %>% 
  group_by(year, month, lon_bin_center, lat_bin_center) %>% 
  summarize(hours = sum(hours))

# Load indices
all_indices <- read.csv(here("data","all_indices.csv"),
                        stringsAsFactors = F) %>% 
  filter(year > 2011) %>% 
  select(year, month = month_n, nino3anom)

# Put indices and fishing effort together
effort_cor <- gridded_ff %>% 
  left_join(all_indices, by = c("year", "month")) %>% 
  group_by(lat_bin_center, lon_bin_center) %>% 
  summarize(r = cor(hours, nino3anom, use = "pairwise.complete.obs")) %>% 
  ungroup()

# Plot everything
ggplot(effort_cor) +
  geom_sf(data = World, fill = "black") +
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = r)) +
  theme_dark() +
  scale_fill_viridis_c()
