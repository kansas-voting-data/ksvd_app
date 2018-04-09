## Description: Reorganize Registration Trends Data from Athena for KSVD
## Author: Mark White, markhwhiteii@gmail.com
library(tidyverse)
library(lubridate)

## county
county <- read_csv("raw_county_registration.csv") %>% 
  filter(desc_party != "desc_party") %>% 
  filter(complete.cases(.)) %>% 
  transmute(
    desc_party = desc_party,
    county = db_logid,
    date = ymd(paste(year, month, day, sep = "-")),
    count = count
  )

## us congress
cg <- read_csv("raw_cg_registration.csv") %>% 
  filter(desc_party != "desc_party") %>% 
  filter(complete.cases(.)) %>% 
  transmute(
    desc_party = desc_party,
    district_cg = as.numeric(gsub("[^0-9]", "", district_cg)),
    date = ymd(paste(year, month, day, sep = "-")),
    count = count
  )

## kansas senate
ks <- read_csv("raw_ks_registration.csv") %>% 
  filter(desc_party != "desc_party") %>% 
  filter(complete.cases(.)) %>% 
  transmute(
    desc_party = desc_party,
    district_ks = as.numeric(gsub("[^0-9]", "", district_ks)),
    date = ymd(paste(year, month, day, sep = "-")),
    count = count
  )

## kansas house
kr <- read_csv("raw_kr_registration.csv") %>% 
  filter(desc_party != "desc_party") %>% 
  filter(complete.cases(.)) %>% 
  transmute(
    desc_party = desc_party,
    district_kr = as.numeric(gsub("[^0-9]", "", district_kr)),
    date = ymd(paste(year, month, day, sep = "-")),
    count = count
  )

## write to file
dfs <- list(county = county, cg = cg, ks = ks, kr = kr)
for (i in names(dfs)) {
  write_csv(dfs[[i]], paste0("../app/Data/", i, "_long.csv"))
}
