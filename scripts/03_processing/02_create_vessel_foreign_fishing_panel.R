############# MAKE PANEL #############
## This script makes the panel
######################################


# Set up
library(tidyverse)

# Load indices data
all_indices <- read.csv("data/all_indices.csv", stringsAsFactors = F) %>% 
  select(year, month = month_n, nino34anom) %>% 
  mutate(nino34anom_l1 = lag(nino34anom, 1),
         nino34anom_l2 = lag(nino34anom, 2),
         nino34anom_l3 = lag(nino34anom, 3))

# Load vessel data and modify
data <- readRDS(here("raw_data", "panel.rds")) %>% 
  group_by(year, month, ssvid) %>% 
  mutate(total_fishing_hours = sum(fishing_hours),
         total_fishing_kilowatthours = sum(fishing_kilowatthours),
         foreign_fishing_hours_share = fishing_hours / total_fishing_hours,
         foreign_fishing_kilowatthours_share = fishing_kilowatthours / total_fishing_kilowatthours) %>% 
  ungroup() %>% 
  filter(is_foreign) %>% 
  rename(foreign_fishing_hours = fishing_hours,
         foreign_fishing_kilowatthours = fishing_kilowatthours)

vessel_info <- data %>% 
  select(ssvid, gear, flag) %>% 
  distinct()

vessel_tracks <- data %>% 
  select(year, month, ssvid, contains("hours")) %>% 
  complete(ssvid, nesting(year, month), fill = list(foreign_fishing_hours = 0,
                                                    foreign_fishing_hours_share = 0,
                                                    foreign_fishing_kilowatthours = 0,
                                                    foreign_fishing_kilowatthours_share = 0))

panel <- vessel_tracks %>% 
  left_join(vessel_info, by = c("ssvid")) %>% 
  left_join(all_indices, by = c("year", "month")) %>% 
  mutate(year = as.factor(year),
         month = as.factor(month),
         ssvid = as.factor(ssvid))
  

# Export data
saveRDS(panel, file = file.path(project_path, "processed_data", "vessel_foreign_fishing_panel.rds"))

