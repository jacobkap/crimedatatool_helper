setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/raw_data")
load("prisoners_census.rda")
prisoners_census <-
  prisoners_census %>%
  dplyr::select(-matches("indian|asian"))

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/census_data")
library(dplyr)
library(asciiSetupReader)
library(tidyr)
library(fastDummies)

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

clean_and_agg_data <- function(file, type) {
  data <-
    spss_ascii_reader(paste0(file, ".txt"),
                      paste0(file, ".sps"),
                      keep_columns = c("SEX",
                                       "RPTYEAR",
                                       "STATE",
                                       "OFFDETAIL",
                                       "RACE")) %>%
    dplyr::rename(sex     = SEX_OF_INMATE,
                  year    = YEAR_DATA_WERE_SUBMITTED_TO_NCRP,
                  race    = RACE_HISPANIC_ETHNICITY_OF_INMATE,
                  offense = DETAILED_CATEGORIZATION_OF_MOST_SERIOUS_SENTENCED_OFFENSE,
                  state   = STATE_WITH_CUSTODY_OF_INMATE) %>%
    dplyr::select(sex,
                  year,
                  race,
                  offense,
                  state) %>%
    dplyr::mutate(race    = stringr::str_replace_all(race, race_fix),
                  offense = stringr::str_replace_all(offense, offenses_fix),
                  race    = tolower(race),
                  offense = tolower(offense),
                  sex     = tolower(sex))

  # Total everything
  total_total_total_counts <-
    data %>%
    dplyr::select(-sex,
                  -race,
                  -offense) %>%
    dplyr::group_by(year,
                    state) %>%
    dplyr::count() %>%
    dplyr::rename(total_total_total = n)

  # Sex totals
  total_total_sex_counts <-
    data %>%
    dplyr::select(-race,
                  -offense) %>%
    dplyr::group_by(year,
                    state,
                    sex) %>%
    dplyr::count() %>%
    spread(sex, n) %>%
    dplyr::rename(total_total_female = female,
                  total_total_male = male)

  # Sex-Race totals
  total_race_sex_counts <-
    data %>%
    dplyr::select(-offense) %>%
    dplyr::group_by(year,
                    state,
                    race,
                    sex) %>%
    dplyr::count() %>%
    unite(temp, race, sex) %>%
    spread(temp, n) %>%
    dplyr::rename_at(vars(matches("male|female|total")), funs(paste0("total_", .)))

  # Race totals
  total_total_race_counts <-
    data %>%
    dplyr::select(-sex,
                  -offense) %>%
    dplyr::group_by(year,
                    state,
                    race) %>%
    dplyr::count() %>%
    spread(race, n) %>%
    dplyr::rename(total_black_total            = black,
                  total_hispanic_total         = hispanic,
                  total_other_or_unknown_total = other_or_unknown,
                  total_white_total            = white)

  # Crime totals
  total_crime_counts <-
    data %>%
    dplyr::select(-sex,
                  -race) %>%
    dplyr::mutate(offense = paste0(offense, "_total_total")) %>%
    dplyr::group_by(year,
                    state,
                    offense) %>%
    dplyr::count() %>%
    spread(offense, n)

  # Sex-crime total
  sex_total_counts <-
    data %>%
    dplyr::select(-race) %>%
    dplyr::mutate(sex = paste0("total_", sex)) %>%
    dplyr::group_by(year,
                    sex,
                    state,
                    offense) %>%
    dplyr::count() %>%
    unite(temp, offense, sex) %>%
    spread(temp, n)

  # Race-crime total
  race_total_counts <-
    data %>%
    dplyr::select(-sex) %>%
    dplyr::mutate(race = paste0(race, "_total")) %>%
    dplyr::group_by(year,
                    race,
                    state,
                    offense) %>%
    dplyr::count() %>%
    unite(temp, offense, race) %>%
    spread(temp, n)

  race_sex_counts <-
    data %>%
    dplyr::group_by(year,
                    race,
                    sex,
                    state,
                    offense) %>%
    dplyr::count() %>%
    unite(temp, offense, race, sex) %>%
    spread(temp, n)

  data <-
    total_total_total_counts %>%
    dplyr::left_join(total_total_sex_counts) %>%
    dplyr::left_join(total_total_race_counts ) %>%
    dplyr::left_join(total_race_sex_counts) %>%
    dplyr::left_join(total_crime_counts) %>%
    dplyr::left_join(sex_total_counts) %>%
    dplyr::left_join(race_total_counts) %>%
    dplyr::left_join(race_sex_counts)

  col_to_na <- function(col) {
    col <- rep(NA, length(col))
  }
#  Adds in the states (and total national jurisdictions) that aren't included in the data
  temp <- data[1:5, ]
  temp$state <- c( "Connecticut",
                   "Vermont",
                   "State Prison Total",
                   "US Prison Total",
                   "Federal Prison Total")
  temp <-
    temp %>%
    dplyr::mutate_if(is.numeric, col_to_na)

  data <- dplyr::bind_rows(data, temp)

  ordered_names <- sort(names(data))
  ordered_names <- ordered_names[!ordered_names %in% c("state", "year")]
  ordered_names <- paste0(ordered_names, "_", type)

  data <-
    data %>%
    fastDummies::dummy_rows(select_columns = c("year", "state")) %>%
    dplyr::rename_at(vars(matches("male|female|total")), funs(paste0(., "_", type))) %>%
    dplyr::select(state,
                  year,
                  one_of(ordered_names)) %>%
    dplyr::left_join(prisoners_census)

  gc(); Sys.sleep(10)
  return(data)
}


save_as_CSVs <- function(data, type) {
  for (selected_state in sort(unique(data$state))) {
    temp <-
      data %>%
      dplyr::filter(state %in% selected_state)

    save_state     <- unique(temp$state)
    save_state     <- gsub(" ", "_", save_state)

    readr::write_csv(temp,
                     path = paste0(save_state, "_", type, "_crime_prisoners.csv"))
  }
}

