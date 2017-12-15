#' Calculate Proportions for KSVD Variables of Interest
#' 
#' This function takes a data frame returned from the \code{getting_*.R} 
#' files and returns the proportions for variables of interest for the 
#' Kansas Voting Data application.
#' 
#' @param acs_df A data frame returned from the \code{getting_*.R} files
#' @param locale Logical. If \code{TRUE}, it is grouped by the locale
#' of interest (e.g., county, congressional district). If \code{FALSE},
#' it is grouped by state.
#' 
get_proportions <- function(acs_df, locale) {
  location_col <- names(acs_df)[1]
  varname_col <- names(acs_df)[2]
  if (locale == TRUE) {
    acs_df <- acs_df %>% 
      group_by(.dots = c(location_col, varname_col)) %>% 
      summarise(value = sum(value)) %>% 
      group_by(.dots = location_col) %>% 
      spread(variable_composite, value)
  } else if (locale == FALSE) {
    acs_df <- acs_df %>% 
      group_by(.dots = varname_col) %>% 
      summarise(value = sum(value)) %>% 
      spread(variable_composite, value)
  }
  acs_df %>% 
    transmute(
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
}
