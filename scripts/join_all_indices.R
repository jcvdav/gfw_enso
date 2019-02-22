##########################################################
# Joining all oceanographic indices
# Fri Apr 06 09:48:09 2018 ------------------------------
##########################################################

## Load packages
library(tidyverse) # Load the tidyverse libraries
library(lubridate) #Date management
library(here) # Load the here package

here <- here::here

## Process data
# All files have the same structure.
# First I read them in, then assign proper column names, and gather
# the table to have a column for year, a column for month, and a
# column for the index.

nino3 <- read.table(here("raw_data", "indices","nino3.long.data")) %>% 
  magrittr::set_colnames(c("year", month.name)) %>% 
  gather(month, nino3, -year)

nino3anom <- read.table(here("raw_data", "indices","nino3.long.anom.data")) %>% 
  magrittr::set_colnames(c("year", month.name)) %>% 
  gather(month, nino3anom, -year)

nino34 <- read.table(here("raw_data", "indices","nino34.long.data")) %>% 
  magrittr::set_colnames(c("year", month.name)) %>% 
  gather(month, nino34, -year)

nino3anom <- read.table(here("raw_data", "indices","nino34.long.anom.data")) %>% 
  magrittr::set_colnames(c("year", month.name)) %>% 
  gather(month, nino34anom, -year)

nino4 <- read.table(here("raw_data", "indices","nino4.long.data")) %>% 
  magrittr::set_colnames(c("year", month.name)) %>% 
  gather(month, nino4, -year)

nino4anom <- read.table(here("raw_data", "indices","nino4.long.anom.data")) %>% 
  magrittr::set_colnames(c("year", month.name)) %>% 
  gather(month, nino4anom, -year)

soi <- read.table(here("raw_data", "indices","soi_3dp.dat")) %>% 
  magrittr::set_colnames(c("year", month.name, "anual")) %>% 
  gather(month, soi, -c(year, anual)) %>% 
  select(-anual)

# I now join the data to have each index for every year-month

all_indices <- nino3 %>% 
  left_join(nino3anom, by = c("year", "month")) %>% 
  left_join(nino4, by = c("year", "month")) %>% 
  left_join(nino4anom, by = c("year", "month")) %>% 
  left_join(soi, by = c("year", "month")) %>% 
  mutate(date = paste0("01", month, year, sep = "/"),
         date = dmy(date),
         month_n = month(date)) %>% 
  select(year, month, month_n, date, everything()) %>% 
  arrange(year, month_n)

# Export the data
write.csv(all_indices, file = here("data","all_indices.csv"), row.names = F)
