## Description: Organize and Export Election Results Data
## Author: Mark White, markhwhiteii@gmail.com
library(tidyverse)
library(stringr)
files <- list.files()[1:6]
names <- c("gen12", "pri12", "gen14", "pri14", "gen16", "pri16")
election <- rep(c("General","Primary"),3)
year <- c(2012, 2012, 2014, 2014, 2016, 2016)
for (i in 1:6){
  assign(names[i], mutate(read_csv(files[i]), 
                          election = election[i], year = year[i]))
}
dat <- bind_rows(gen12, pri12, gen14, pri14, gen16, pri16) %>% 
  mutate(candidate = trimws(str_replace_all(candidate, "\\s+", " ")))

write_csv(dat, "../app/Data/election_results.csv")

## NOTE: cleaning of candidate names (e.g., middle initials) were done manually
