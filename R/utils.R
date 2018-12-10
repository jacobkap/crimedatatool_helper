load("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/crosswalk_agencies.rda")
library(tidyverse)
library(data.table)
library(fastDummies)
library(splitstackshape)

make_all_na <- function(col) {
  col <- NA
}

dummy_rows_missing_years <- function(data, type = "arrest") {
  missing_years <- min(data$year):max(data$year)
  missing_years <- missing_years[!missing_years %in% data$year]

  if (length(missing_years) > 0) {

    temp <- data
    if (type == "arrest") {
    temp <- temp[temp$number_of_months_reported %in% 12,]
    }
    temp <- temp[1, ]
    temp <- splitstackshape::expandRows(temp,
                                        count = length(missing_years),
                                        count.is.col = FALSE)
    temp$year <- missing_years
    temp <-
      temp %>%
      dplyr::mutate_at(vars(-one_of("year", "agency", "state", "ORI")),
                       make_all_na)

    data <-
      data %>%
      dplyr::bind_rows(temp) %>%
      dplyr::arrange(desc(year)) %>%
      dplyr::mutate(year = as.character(year))
  }


  return(data)
}

na_non_12_month_rows <- function(data) {

  temp <-
    data %>%
    dplyr::filter(!number_of_months_reported  %in% 12 & year >= 1972) %>%
    dplyr::mutate_at(vars(-one_of("year",
                                  "agency",
                                  "state",
                                  "ORI",
                                  "population",
                                  "female_employees_officers",
                                  "male_employees_officers",
                                  "total_employees_officers",
                                  "female_employees_civilians",
                                  "male_employees_civilians",
                                  "total_employees_civilians",
                                  "female_employees_total",
                                  "male_employees_total",
                                  "total_employees_total")),
                     make_all_na)

  data <-
    data %>%
    dplyr::filter(number_of_months_reported  == 12 | year < 1972) %>%
    dplyr::bind_rows(temp) %>%
    dplyr::arrange(desc(year))

  return(data)
}

simpleCap <- function(word) {
  word <- tolower(word)
  split_word <- strsplit(word, " ")[[1]]
  split_word <- paste(toupper(substring(split_word, 1,1)),
                      substring(split_word, 2),
                      sep = "",
                      collapse = " ")
  split_word <- gsub(" Of ", " of ", split_word)
}

make_numeric <- function(x) {
  x <- suppressWarnings(readr::parse_number(x))
  return(x)
}

starting_cols <- c("agency",
                   "year",
                   "state",
                   "population",
                   "ORI")

ucr_to_drop <- c("state_abb",
                 "ORI9",
                 "FIPS_state_code",
                 "FIPS_county_code",
                 "months_reported",
                 "fips_state_county_code",
                 "fips_place_code",
                 "fips_state_place_code",
                 "division",
                 "core_city_indication",
                 "covered_by_code",
                 "population_1",
                 "county_1",
                 "msa_1",
                 "population_2",
                 "county_2",
                 "msa_2",
                 "population_3",
                 "county_3",
                 "msa_3",
                 "followup_indication",
                 "special_mailing_group",
                 "special_mailing_address",
                 "mailing_address_line_1",
                 "mailing_address_line_2",
                 "mailing_address_line_3",
                 "mailing_address_line_4",
                 "agency_type",
                 "agency_subtype_1",
                 "agency_subtype_2",
                 "group_number",
                 "agency_state_name",
                 "agency_name",
                 "field_office",
                 "total_population",
                 "city_sequence_number",
                 "last_update",
                 "field_office",
                 "zip_code")

arrests_to_drop <- c("ori9",
                     "agency_name",
                     "state_abb",
                     "fips_state_code",
                     "fips_county_code",
                     "fips_state_county_code",
                     "fips_place_code",
                     "fips_state_place_code",
                     "agency_type",
                     "agency_subtype_1",
                     "agency_subtype_2",
                     "group",
                     "geographic_division",
                     "suburban_agency",
                     "core_city",
                     "covered_by_another_agency")

