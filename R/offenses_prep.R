load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/offenses_known/offenses_known_yearly_1960_2017.rda")
source(here::here('R/utils.R'))

ucr <-
  offenses_known_yearly_1960_2017 %>%
  dplyr::filter(number_of_months_reported %in% 12) %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA",
                ori    != "FL01394") %>%
  dplyr::mutate(agency = tolower(agency)) %>%
  dplyr::rename(ORI    = ori) %>%
  dplyr::select(starting_cols,
                dplyr::matches("act|clr|unfound|officer"))

rm(offenses_known_yearly_1960_2017); gc()

ucr <- remove_duplicate_capitalize_names(ucr)


setwd(here::here("data/offenses"))
make_agency_csvs(ucr)
make_state_agency_choices(ucr)
make_largest_agency_json(ucr)
