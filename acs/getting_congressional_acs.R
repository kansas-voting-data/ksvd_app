## Description: Fetch, Organize, and Export Congressional District Data for KSVD
## Author: Mark White, markhwhiteii@gmail.com
library(acs)
library(tidyverse)
library(stringr)
source("get_proportions.R")
source("get_totals.R")

key <- read_csv("acs_variable_key.csv")

ks_geo <- geo.make(state = "KS", congressional.district = "*")

dat_congress <- acs.fetch(2015, 5, ks_geo, variable = key$variable_code) %>% 
  estimate() %>% 
  as.data.frame() %>% 
  rownames_to_column("congressional") %>% 
  mutate(congressional = str_sub(congressional, 15, 24)) %>% 
  gather("variable", "value", -congressional) %>% 
  full_join(key, ., by = c("variable_code" = "variable")) %>% 
  group_by(congressional, variable_composite) %>% 
  summarise(value = sum(value))

acs_congress_totals <- get_totals(dat_congress, TRUE)
acs_congress_proportions <- get_proportions(dat_congress, TRUE)

write.csv(acs_congress_totals, 
          "../app/Data/acs_congress_totals.csv", row.names = FALSE)
write.csv(acs_congress_proportions, 
          "../app/Data/acs_congress_proportions.csv", row.names = FALSE)
