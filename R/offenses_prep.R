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
                !state %in% c("guam",
                              "canal zone",
                              "puerto rico",
                              "virgin islands")) %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA",
                ori    != "FL01394") %>%
  dplyr::mutate(population = (as.numeric(population_1) +
                                as.numeric(population_2) +
                                as.numeric(population_3)),
                agency = tolower(agency)) %>%
  dplyr::rename(ORI = ori,
                ORI9 = ori9,
                FIPS_state_code = fips_state_code,
                FIPS_county_code = fips_county_code) %>%
  dplyr::select(-one_of(to_drop)) %>%
  dplyr::select(starting_cols,
                everything())

z = ucr[!duplicated(ucr$ORI),]
z$temp <- paste(z$agency, z$state)
z = z[duplicated(z$temp),]
ucr <- ucr[!ucr$ORI %in% z$ORI, ]
ucr$agency <- sapply(ucr$agency, simpleCap)
ucr$state <- sapply(ucr$state, simpleCap)
ucr$state <- gsub(" Of ", " of ", ucr$state)


setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/offenses")
ucr <- data.table::data.table(ucr)
for (selected_ori in unique(ucr$ORI)) {
  temp   <- ucr[ORI %in% selected_ori]
  state  <- unique(temp$state)
  agency <- unique(temp$agency)
  state  <- gsub(" ", "_", state)
  agency <- gsub(" |:", "_", agency)
  agency <- gsub("/", "_", agency)
  agency <- gsub("_+", "_", agency)
  readr::write_csv(temp,
                   path = paste0(state, "_", agency, ".csv"))

}

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/offenses")
for (selected_state in unique(ucr$state)) {
  temp   <- ucr[state %in% selected_state]
  agency <- unique(temp$agency)
  agency <- jsonlite::toJSON(agency, pretty = TRUE)
  write(agency, paste0(selected_state, "_agency_choices.json"))

}

starting_cols <- c("agency",
                   "year",
                   "state",
                   "population",
                   "ORI")

to_drop <- c("state_abb",
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

