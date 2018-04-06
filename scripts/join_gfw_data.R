##########################################################
# Joining all gfw data
# Fri Apr 06 10:47:56 2018 ------------------------------
##########################################################

# The GFW data is exported as 49 different csv files. The
# data was obtained with the query in
# "TEST_illegal_fishing_effort_CHN.sql". I will use a sparklyr
# connection to map the data, put it together, and export
# it to a serialized *.rds format

## Load packages
library(sparklyr)
library(dplyr)
library(here)
library(purrr)

## Seasonal

all_indices <- read.csv("data/all_indices.csv", stringsAsFactors = F)%>%
  mutate(season = case_when(month_n %in% c(12, 1, 2) ~ "winter",
                            month_n %in% c(3, 4, 5) ~ "spring",
                            month_n %in% c(6, 7, 8) ~ "summer",
                            TRUE ~ "fall"))

wanted_gears <- c("purse_seines", "trawlers", "drifting_longlines")

season <- read.csv("raw_data/GFW/CHN_year_season_vessel_foreign.csv", stringsAsFactors = F)

nino3_by_season <- all_indices %>% 
  group_by(year, season) %>% 
  summarize(nino3anom = mean(nino3anom)) %>% 
  ungroup()

season_nino3 <- nino3_by_season %>% 
  select(year, season, nino3anom) %>% 
  right_join(season, by = c("year", "season"))

season_nino3 %>% 
  filter(geartype %in% wanted_gears,
         is_foreign,
         fishing) %>% 
  group_by(year, season, geartype, nino3anom) %>% 
  summarize(hours = sum(hours)) %>% 
  ungroup() %>% 
  ggplot(aes(x = nino3anom, y = hours, color = geartype)) +
  geom_point() +
  theme_minimal()
  

## Monthly hours

month <- read.csv("raw_data/GFW/CHN_year_season_month_vessel_foreign.csv", stringsAsFactors = F)

month_nino3 <- all_indices %>% 
  select(year, month = month_n, nino3anom)

month_indices <- left_join(month, month_nino3, by = c("year", "month"))

month_indices %>% 
  filter(is_foreign,
         fishing,
         geartype %in% wanted_gears) %>%
  group_by(year, month, geartype, season, nino3anom) %>% 
  summarize(hours = sum(hours)) %>% 
  ungroup() %>% 
  ggplot(aes(x = nino3anom, y = hours, color = geartype, shape = season)) +
  geom_point() +
  theme_bw() +
  facet_wrap(season~geartype, scales = "free_y", ncol = 3) +
  ggtitle("hours")

## Vessels by month

month_indices %>% 
  filter(is_foreign,
         fishing,
         geartype %in% wanted_gears) %>%
  group_by(year, month, mmsi, geartype, season, nino3anom) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(year, month, geartype, season, nino3anom) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = nino3anom, y = n, color = geartype, shape = season)) +
  geom_point() +
  theme_bw() +
  facet_wrap(season~geartype, scales = "free_y", ncol = 3) +
  ggtitle("vessels")



model_data <- month_indices %>% 
  filter(is_foreign,
         fishing,
         geartype %in% wanted_gears) %>%
  group_by(year, month, mmsi, geartype, season, nino3anom) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(year, month, geartype, season, nino3anom) %>% 
  summarize(n = n()) %>% 
  ungroup()

lm(log(n) ~ nino3anom + season + geartype, data = model_data) %>% 
  lmtest::coeftest(x = ., vcov = sandwich::vcovHC(x = ., "HC2")) %>% 
  broom::tidy()


CHN
TAIWAN
Japan
South North Korea
Australia
NZ

handle with care:
  US
  Spanish
  Portugal














