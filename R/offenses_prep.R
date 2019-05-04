load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/offenses_known/offenses_known_yearly_1960_2017.rda")
source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/utils.R')

ucr <-
  offenses_known_yearly_1960_2017 %>%
  dplyr::filter(number_of_months_reported %in% 12) %>%
                # !state %in% c("guam",
                #               "canal zone",
                #               "puerto rico",
                #               "virgin islands")) %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA",
                ori    != "FL01394") %>%
  dplyr::mutate(agency = tolower(agency)) %>%
  dplyr::rename(ORI    = ori) %>%
  dplyr::select(starting_cols,
                dplyr::matches("act|clr|unfound|officer"))

rm(offenses_known_yearly_1960_2017); gc()

z = ucr[!duplicated(ucr$ORI),]
z$temp <- paste(z$agency, z$state)
z = z[duplicated(z$temp),]
ucr <- ucr[!ucr$ORI %in% z$ORI, ]
ucr$agency <- sapply(ucr$agency, simpleCap)
ucr$state  <- sapply(ucr$state, simpleCap)


setwd(here::here("data/offenses"))
make_agency_csvs(ucr)
make_state_agency_choices(ucr)
make_largest_agency_json(ucr)
