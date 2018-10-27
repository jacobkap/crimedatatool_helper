load("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/crosswalk_agencies.rda")
library(tidyverse)
library(data.table)

simpleCap <- function(word) {
  word <- tolower(word)
  split_word <- strsplit(word, " ")[[1]]
  split_word <- paste(toupper(substring(split_word, 1,1)),
                      substring(split_word, 2),
                      sep = "",
                      collapse = " ")
  split_word <- gsub("Of", "of", split_word)
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

