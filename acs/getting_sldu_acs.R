## Description: Fetch, Organize, and Export State Senate District Data for KSVD
## Author: Mark White, markhwhiteii@gmail.com
library(acs)
library(tidyverse)
library(stringr)
source("get_proportions.R")
source("get_totals.R")

key <- read_csv("acs_variable_key.csv")

ks_geo <- geo.make(state = "KS", state.legislative.district.upper = "*")

dat_sldu <- acs.fetch(2015, 5, ks_geo, variable = key$variable_code) %>% 
  estimate() %>% 
  as.data.frame() %>% 
  rownames_to_column("sldu") %>% 
  mutate(sldu = trimws(str_sub(sldu, 14, 24))) %>% 
  gather("variable", "value", -sldu) %>% 
  full_join(key, ., by = c("variable_code" = "variable")) %>% 
  group_by(sldu, variable_composite) %>% 
  summarise(value = sum(value))

acs_sldu_totals <- get_totals(dat_sldu, TRUE)
acs_sldu_proportions <- get_proportions(dat_sldu, TRUE)

write.csv(acs_sldu_totals, 
          "../ksvd_app/Data/acs_sldu_totals.csv", row.names = FALSE)
write.csv(acs_sldu_proportions, 
          "../ksvd_app/Data/acs_sldu_proportions.csv", row.names = FALSE)
