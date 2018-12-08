setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/census_data")
library(dplyr)
library(asciiSetupReader)
library(tidyr)

admissions   <- clean_and_agg_data("corrections_prison_admissions_1991_2016")
release      <- clean_and_agg_data("corrections_prison_releases_1991_2016")
incarcerated <- clean_and_agg_data("corrections_year_end_pop_1991_2016")


race_fix <- c("Black, non-Hispanic"             = "black",
              "White, non-Hispanic"             = "white",
              "Missing"                         = "other_or_unknown",
              "Other race\\(s\\), non-Hispanic" = "other_or_unknown",
              "Hispanic, any race"              = "hispanic" )


offenses_fix <- c("Aggravated or simple assault" = "aggravated_or_simple_assault",
                  "Drugs \\(includes possession, distribution, trafficking, other\\)" = "drugs",
                  "Larceny"                 = "theft",
                  "Missing"                 = "other_unknown",
                  "Motor vehicle theft"     = "motor_vehicle_theft",
                  "Murder \\(including non-negligent manslaughter\\)" = "murder",
                  "Negligent manslaughter"  = "negligent_manslaughter",
                  "Other property offenses" = "other_property_offenses",
                  "Other violent offenses"  = "other_violent_offenses",
                  "Other/unspecified"       = "other_unknown",
                  "Public order"            = "public_order",
                  "Rape/sexual assault"     = "rape_or_sexual_assault",
                  "Robbery"                 = "robbery")
clean_and_agg_data <- function() {
  data <-
    spss_ascii_reader(paste0(file, ".txt"),
                      paste0(file, ".sps")) %>%
    dplyr::rename(sex = SEX_OF_INMATE,
                  year = YEAR_DATA_WERE_SUBMITTED_TO_NCRP,
                  race = RACE_HISPANIC_ETHNICITY_OF_INMATE,
                  offense = DETAILED_CATEGORIZATION_OF_MOST_SERIOUS_SENTENCED_OFFENSE,
                  state = STATE_WITH_CUSTODY_OF_INMATE) %>%
    dplyr::select(sex,
                  year,
                  race,
                  offense,
                  state) %>%
    dplyr::mutate(race    = stringr::str_replace_all(race, race_fix),
                  offense = stringr::str_replace_all(offense, offenses_fix)) %>%
    dplyr::mutate_all(tolower)


  dplyr::group_by(sex,
                  year,
                  race,
                  state,
                  offense) %>%
    dplyr::count()

  total_total_counts <-
    data %>%
    dplyr::select(-sex,
                  -race) %>%
    dplyr::mutate(offense = paste0("total_total_", offense)) %>%
    dplyr::group_by(year,
                    state,
                    offense) %>%
    dplyr::count() %>%
    spread(offense, n)

  sex_total_counts <-
    data %>%
    dplyr::select(-race) %>%
    dplyr::mutate(sex = paste0(sex, "_total")) %>%
    dplyr::group_by(year,
                    sex,
                    state,
                    offense) %>%
    dplyr::count() %>%
    unite(temp, sex, offense) %>%
    spread(temp, n)

  race_total_counts <-
    data %>%
    dplyr::select(-sex) %>%
    dplyr::mutate(race = paste0("total_", race)) %>%
    dplyr::group_by(year,
                    race,
                    state,
                    offense) %>%
    dplyr::count() %>%
    unite(temp, race, offense) %>%
    spread(temp, n)

  race_sex_counts <-
    data %>%
    dplyr::group_by(year,
                    race,
                    sex,
                    state,
                    offense) %>%
    dplyr::count() %>%
    unite(temp, sex, race, offense) %>%
    spread(temp, n)

  all_data <-
    total_total_counts %>%
    dplyr::left_join(sex_total_counts) %>%
    dplyr::left_join(race_total_counts) %>%
    dplyr::left_join(race_sex_counts)

  rm(data); gc()
  return(all_data)
}