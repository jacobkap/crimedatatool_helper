offenses_known_yearly_1960_2022 <- readRDS("F:/ucr_data_storage/clean_data/offenses_known/offenses_known_yearly_1960_2022.rds")
table(offenses_known_yearly_1960_2022$year,
      offenses_known_yearly_1960_2022$number_of_months_missing)
source(here::here('R/utils.R'))


offenses_known_yearly_1960_2022 <-
  offenses_known_yearly_1960_2022 %>%
  dplyr::filter(number_of_months_missing %in% 0)

offenses_known_yearly_1960_2022 <-
  offenses_known_yearly_1960_2022 %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA",
                ori    != "FL01394") %>%
  dplyr::mutate(agency = tolower(agency)) %>%
  dplyr::rename(ORI    = ori) %>%
  dplyr::select(starting_cols,
                dplyr::matches("act|clr|unfound|officer"))

offenses_known_yearly_1960_2022$agency <- gsub("\\(|\\)", "", offenses_known_yearly_1960_2022$agency)
offenses_known_yearly_1960_2022$agency <- gsub("\\/", "-", offenses_known_yearly_1960_2022$agency)
offenses_known_yearly_1960_2022 <- remove_duplicate_capitalize_names(offenses_known_yearly_1960_2022)
# FIxes NA issue in 2022.
offenses_known_yearly_1960_2022$state[offenses_known_yearly_1960_2022$ORI %in% "DEDEA01"] <- "Delaware"


setwd(here("data/offenses"))
make_agency_csvs(offenses_known_yearly_1960_2022)
make_largest_agency_json(offenses_known_yearly_1960_2022)
make_state_agency_choices(offenses_known_yearly_1960_2022)

