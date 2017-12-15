## Description: Organize and Export Election Turnout Data
## Author: Mark White, markhwhiteii@gmail.com
library(tidyverse)
files <- list.files()[1:3]
year <- c(2012, 2014, 2016)
names <- c("gn12", "gn14", "gn16")
for (i in 1:3){
  assign(names[i], mutate(read_csv(files[i]), year = year[i]))
}
dat <- bind_rows(gn12, gn14, gn16) %>% 
  mutate(county = trimws(county),
         prop_votes_provisional = round(provisional_votes/total_votes_cast, 3))
var_order <- c("year", "county", "registered_voters", "total_votes_cast", 
               "prop_turnout", "advance_votes", "prop_votes_advance", 
               "provisional_votes", "prop_votes_provisional")
dat <- dat[, var_order]

write_csv(dat, "../app/Data/election_turnout.csv")
