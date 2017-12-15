## Description: Fetch, Organize, and Export Statewide Data for KSVD
## Author: Mark White, markhwhiteii@gmail.com
library(acs)
library(tidyverse)

key <- read_csv("acs_variable_key.csv")

ks_geo <- geo.make(state = "KS")

for (y in c(2012:2016)) {
  key[ , paste0("yr", y)] <- c(estimate(acs.fetch(
    y, 1, ks_geo, variable = key$variable_code
  )))
}

state_totals <- key %>%
  select(-variable_name, -variable_code) %>% 
  gather(year, value, yr2012:yr2016) %>% 
  mutate(year = as.numeric(substr(year, 3, 6))) %>% 
  group_by(variable_composite, year) %>% 
  summarise(value = sum(value)) %>% 
  spread(variable_composite, value) %>% 
  transmute(
    year = year,
    `Cash Assistance/SNAP - With` = casnap_yes,
    `Cash Assistance/SNAP - Without` = casnap_not,
    `Citizen - Born Abroad` = citizen_abroad,
    `Citizen - Naturalized` = citizen_naturalized,
    `Citizen - U.S. Born` = citizen_usborn,
    `Citizen - U.S. Islands` = citizen_usislands,
    `Citizen - Not` = citizen_not,
    `Disability - Yes` = disability_yes_male + disability_yes_female, 
    `Disability - No` = disability_no_male + disability_no_female,
    `Education - Less than 9th Grade` = education_9thless,
    `Education - 9th to 12th Grade, No Diploma` = education_nohsdiploma,
    `Education - High School Diploma or Equivalent` = education_highschool,
    `Education - Some College, No Degree` = education_somecollege,
    `Education - Associate's Degree` = education_associates,
    `Education - Bachelor's Degree` = education_bachelors,
    `Education - Graduate or Professional Degree` = education_gradprof,
    `Voting Age Population (Gender) - Male` = vap_sex_male,
    `Voting Age Population (Gender) - Female` = vap_sex_female,
    `Voting Age Population (Citizenship) - Citizen` = citizen_vap,
    `Voting Age Population (Citizenship) - Not Citizen` = citizen_not_vap,
    `Voting Age Population (Race) - Asian` = vap_race_asian,
    `Voting Age Population (Race) - Black/African-American` = vap_race_black,
    `Voting Age Population (Race) - Hispanic or Latino/a` = vap_race_hispanic,
    `Voting Age Population (Race) - Multiracial` = vap_race_multi,
    `Voting Age Population (Race) - Native American/American Indian` = 
      vap_race_americanindian,
    `Voting Age Population (Race) - Pacific Islander` = vap_race_pacificislander,
    `Voting Age Population (Race) - White/European-American` = vap_race_white,
    `Voting Age Population (Race) - Other Race` = vap_race_other,
    `Health Insurance - With` = healthins_yes,
    `Health Insurance - Without` = healthins_not,
    `Language at Home - English Only` = language_english,
    `Language at Home - Spanish, Always or Sometimes` = language_spanish,
    `Language at Home - Asian/Pacific, Always or Sometimes` = 
      language_asianpacific,
    `Language at Home - Other Indo-European, Always or Sometimes` = 
      language_indoeuro,
    `Language at Home - Other, Always or Sometimes` = 
      language_other,
    `Poverty - Below 100% of Poverty Level` = poverty_100pctbelow,
    `Poverty - 100% to 149% of Poverty Level` = poverty_100to149pct,
    `Poverty - 150% or Above Poverty Level` = poverty_150pctabove
  ) %>% 
  gather("Variable", "Value", -1) %>% 
  separate(Variable, c("Variable", "col"), sep = " - ")

state_proportions <- key %>%
  select(-variable_name, -variable_code) %>% 
  gather(year, value, yr2012:yr2016) %>% 
  mutate(year = as.numeric(substr(year, 3, 6))) %>% 
  group_by(variable_composite, year) %>% 
  summarise(value = sum(value)) %>% 
  spread(variable_composite, value) %>% 
  transmute(
    year = year,
    `Cash Assistance/SNAP - With` = casnap_yes / (casnap_not + casnap_yes),
    `Cash Assistance/SNAP - Without` = casnap_not / 
      (casnap_not + casnap_yes),
    `Citizen - Born Abroad` = citizen_abroad / 
      (citizen_abroad + citizen_naturalized + citizen_not + 
         citizen_usborn + citizen_usislands),
    `Citizen - Naturalized` = citizen_naturalized / 
      (citizen_abroad + citizen_naturalized + citizen_not + 
         citizen_usborn + citizen_usislands),
    `Citizen - U.S. Born` = citizen_usborn / 
      (citizen_abroad + citizen_naturalized + citizen_not + 
         citizen_usborn + citizen_usislands),
    `Citizen - U.S. Islands` = citizen_usislands / 
      (citizen_abroad + citizen_naturalized + citizen_not + 
         citizen_usborn + citizen_usislands),
    `Citizen - Not` = citizen_not / 
      (citizen_abroad + citizen_naturalized + citizen_not + 
         citizen_usborn + citizen_usislands),
    `Disability - Yes` = (disability_yes_male + disability_yes_female) / 
      (disability_yes_male + disability_yes_female + 
         disability_no_male + disability_no_female), 
    `Disability - No` = (disability_no_male + disability_no_female) / 
      (disability_yes_male + disability_yes_female + 
         disability_no_male + disability_no_female),
    `Education - Less than 9th Grade` = education_9thless / 
      (education_9thless + education_nohsdiploma + education_highschool + 
         education_somecollege + education_associates + 
         education_bachelors + education_gradprof),
    `Education - 9th to 12th Grade, No Diploma` = education_nohsdiploma / 
      (education_9thless + education_nohsdiploma + education_highschool + 
         education_somecollege + education_associates + 
         education_bachelors + education_gradprof),
    `Education - High School Diploma or Equivalent` = education_highschool / 
      (education_9thless + education_nohsdiploma + education_highschool + 
         education_somecollege + education_associates + 
         education_bachelors + education_gradprof),
    `Education - Some College, No Degree` = education_somecollege / 
      (education_9thless + education_nohsdiploma + education_highschool + 
         education_somecollege + education_associates + 
         education_bachelors + education_gradprof),
    `Education - Associate's Degree` = education_associates / 
      (education_9thless + education_nohsdiploma + education_highschool + 
         education_somecollege + education_associates + 
         education_bachelors + education_gradprof),
    `Education - Bachelor's Degree` = education_bachelors / 
      (education_9thless + education_nohsdiploma + education_highschool + 
         education_somecollege + education_associates + 
         education_bachelors + education_gradprof),
    `Education - Graduate or Professional Degree` = education_gradprof / 
      (education_9thless + education_nohsdiploma + education_highschool + 
         education_somecollege + education_associates + 
         education_bachelors + education_gradprof),
    `Voting Age Population (Gender) - Male` = vap_sex_male /
      (vap_sex_male + vap_sex_female),
    `Voting Age Population (Gender) - Female` = vap_sex_female / 
      (vap_sex_male + vap_sex_female),
    `Voting Age Population (Citizenship) - Citizen` = citizen_vap /
      (citizen_vap + citizen_not_vap),
    `Voting Age Population (Citizenship) - Not Citizen` = citizen_not_vap /
      (citizen_vap + citizen_not_vap),
    `Voting Age Population (Race) - Asian` = vap_race_asian / 
      (vap_race_asian + vap_race_americanindian + vap_race_black +
         vap_race_hispanic + vap_race_multi + vap_race_other +
         vap_race_pacificislander + vap_race_white),
    `Voting Age Population (Race) - Black/African-American` = vap_race_black / 
      (vap_race_asian + vap_race_americanindian + vap_race_black +
         vap_race_hispanic + vap_race_multi + vap_race_other +
         vap_race_pacificislander + vap_race_white),
    `Voting Age Population (Race) - Hispanic or Latino/a` = vap_race_hispanic / 
      (vap_race_asian + vap_race_americanindian + vap_race_black +
         vap_race_hispanic + vap_race_multi + vap_race_other +
         vap_race_pacificislander + vap_race_white),
    `Voting Age Population (Race) - Multiracial` = vap_race_multi / 
      (vap_race_asian + vap_race_americanindian + vap_race_black +
         vap_race_hispanic + vap_race_multi + vap_race_other +
         vap_race_pacificislander + vap_race_white),
    `Voting Age Population (Race) - Native American/American Indian` = 
      vap_race_americanindian / 
      (vap_race_asian + vap_race_americanindian + vap_race_black + 
         vap_race_hispanic + vap_race_multi + vap_race_other + 
         vap_race_pacificislander + vap_race_white),
    `Voting Age Population (Race) - Pacific Islander` = vap_race_pacificislander / 
      (vap_race_asian + vap_race_americanindian + vap_race_black +
         vap_race_hispanic + vap_race_multi + vap_race_other +
         vap_race_pacificislander + vap_race_white),
    `Voting Age Population (Race) - White/European-American` = vap_race_white / 
      (vap_race_asian + vap_race_americanindian + vap_race_black +
         vap_race_hispanic + vap_race_multi + vap_race_other +
         vap_race_pacificislander + vap_race_white),
    `Voting Age Population (Race) - Other Race` = vap_race_other / 
      (vap_race_asian + vap_race_americanindian + vap_race_black +
         vap_race_hispanic + vap_race_multi + vap_race_other +
         vap_race_pacificislander + vap_race_white),
    `Health Insurance - With` = healthins_yes / 
      (healthins_not + healthins_yes),
    `Health Insurance - Without` = healthins_not / 
      (healthins_not + healthins_yes),
    `Language at Home - English Only` = 
      language_english / 
      (language_asianpacific + language_english + 
         language_indoeuro + language_other + language_spanish),
    `Language at Home - Spanish, Always or Sometimes` = 
      language_spanish / 
      (language_asianpacific + language_english + 
         language_indoeuro + language_other + language_spanish),
    `Language at Home - Asian/Pacific, Always or Sometimes` = 
      language_asianpacific / 
      (language_asianpacific + language_english + 
         language_indoeuro + language_other + language_spanish),
    `Language at Home - Other Indo-European, Always or Sometimes` = 
      language_indoeuro / 
      (language_asianpacific + language_english + 
         language_indoeuro + language_other + language_spanish),
    `Language at Home - Other, Always or Sometimes` = 
      language_other / 
      (language_asianpacific + language_english + 
         language_indoeuro + language_other + language_spanish),
    `Poverty - Below 100% of Poverty Level` = poverty_100pctbelow / 
      (poverty_100pctbelow + poverty_100to149pct + poverty_150pctabove),
    `Poverty - 100% to 149% of Poverty Level` = poverty_100to149pct / 
      (poverty_100pctbelow + poverty_100to149pct + poverty_150pctabove),
    `Poverty - 150% or Above Poverty Level` = poverty_150pctabove / 
      (poverty_100pctbelow + poverty_100to149pct + poverty_150pctabove)
  ) %>% 
  gather("Variable", "Value", -1) %>% 
  separate(Variable, c("Variable", "col"), sep = " - ")

write_csv(state_proportions, "../ksvd_app/data/acs_state_proportions.csv")
write_csv(state_totals, "../ksvd_app/data/acs_state_totals.csv")
