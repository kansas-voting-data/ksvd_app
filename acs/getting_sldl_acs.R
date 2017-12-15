## Description: Fetch, Organize, and Export State House District Data for KSVD
## Author: Mark White, markhwhiteii@gmail.com
library(acs)
library(tidyverse)
library(stringr)
source("get_proportions.R")
source("get_totals.R")

key <- read_csv("acs_variable_key.csv")

ks_geo <- geo.make(state = "KS", state.legislative.district.lower = "*")

dat_sldl <- acs.fetch(2015, 5, ks_geo, variable = key$variable_code) %>% 
  estimate() %>% 
  as.data.frame() %>% 
  rownames_to_column("sldl") %>% 
  mutate(sldl = trimws(gsub("[^0-9a-zA-Z ]", "", str_sub(sldl, 12, 24)))) %>% 
  gather("variable", "value", -sldl) %>% 
  full_join(key, ., by = c("variable_code" = "variable")) %>% 
  group_by(sldl, variable_composite) %>% 
  summarise(value = sum(value))

acs_sldl_totals <- get_totals(dat_sldl, TRUE)
acs_sldl_proportions <- get_proportions(dat_sldl, TRUE)

write.csv(acs_sldl_totals, 
          "../app/Data/acs_sldl_totals.csv", row.names = FALSE)
write.csv(acs_sldl_proportions, 
          "../app/Data/acs_sldl_proportions.csv", row.names = FALSE)
