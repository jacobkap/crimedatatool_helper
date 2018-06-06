load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/offenses_known/ucr_offenses_known_yearly_1960_2016.rda")
load("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/crosswalk_agencies.rda")
library(tidyverse)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse = " ")
}

ucr <-
  ucr_offenses_known_yearly_1960_2016 %>%
  dplyr::filter(months_reported == "december is the last month reported",
                !state %in% c("guam", "canal zone", "puerto rico", "virgin islands")) %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::mutate(population = (as.numeric(population_1) +
                                as.numeric(population_2) +
                                as.numeric(population_3)),
                agency = tolower(agency)) %>%
  dplyr::select(-one_of(to_drop)) %>%
  dplyr::rename(ORI = ori,
                ORI9 = ori9,
                FIPS_state_code = fips_state_code,
                FIPS_county_code = fips_county_code) %>%
  dplyr::select(agency,
                year,
                state,
                population,
                ORI,
                ORI9,
                FIPS_state_code,
                FIPS_county_code,
                everything())
ucr$agency <- sapply(ucr$agency, simpleCap)
ucr$state <- sapply(ucr$state, simpleCap)
ucr$state <- gsub(" Of ", " of ", ucr$state)


setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/offenses")
for (state_val in unique(ucr$state)) {
  temp <-
    ucr %>%
    filter(state == state_val)
  write_csv(temp, path = paste0("offenses_", state_val, ".csv"))
}



to_drop <- c("state_abb",
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
             "zip_code")

