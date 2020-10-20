##########################################################
# Joining all oceanographic indices

##########################################################

## Load packages
library(lubridate) #Date management
library(here) # Load the here package
library(tidyverse) # Load the tidyverse libraries


## Process data
# All files have the same structure.
# First I read them in, then assign proper column names, and gather
# the table to have a column for year, a column for month, and a
# column for the index.

nino34 <- read.table(here("raw_data", "indices","nino34.long.data.txt")) %>% 
  magrittr::set_colnames(c("year", month.name)) %>% 
  gather(month, nino34, -year) %>% 
  filter(nino34 > -99)

nino34anom <- read.table(here("raw_data", "indices","nino34.long.anom.data.txt")) %>% 
  magrittr::set_colnames(c("year", month.name)) %>% 
  gather(month, nino34anom, -year) %>% 
  filter(nino34 > -99)

# I now join the data to have each index for every year-month
all_indices <- nino34 %>% 
  left_join(nino34anom, by = c("year", "month")) %>% 
  mutate(date = paste0("01", month, year, sep = "/"),
         date = dmy(date),
         month_n = month(date)) %>% 
  select(year, month, month_n, date, everything()) %>% 
  arrange(year, month_n)

# Export the data
write.csv(all_indices,
          file = here("data","all_indices.csv"),
          row.names = F)
