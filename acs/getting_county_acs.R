## Description: Fetch, Organize, and Export County Data for KSVD
## Author: Mark White, markhwhiteii@gmail.com
library(acs)
library(tidyverse)
library(stringr)
source("get_proportions.R")
source("get_totals.R")

key <- read_csv("acs_variable_key.csv")

ks_geo <- geo.make(state = "KS", county = "*")

dat_county <- acs.fetch(2015, 5, ks_geo, variable = key$variable_code) %>% 
  estimate() %>% 
  as.data.frame() %>% 
  rownames_to_column("county") %>% 
  mutate(county = gsub(" .*$", "", county)) %>% 
  gather("variable", "value", -county) %>% 
  full_join(key, ., by = c("variable_code" = "variable")) %>% 
  group_by(county, variable_composite) %>% 
  summarise(value = sum(value))

acs_county_totals <- get_totals(dat_county, TRUE)
acs_county_proportions <- get_proportions(dat_county, TRUE)

write.csv(acs_county_totals, 
          "../app/Data/acs_county_totals.csv", row.names = FALSE)
write.csv(acs_county_proportions, 
          "../app/Data/acs_county_proportions.csv", row.names = FALSE)
