offenses_known_yearly_1960_2021 <- readRDS("D:/ucr_data_storage/clean_data/offenses_known/offenses_known_yearly_1960_2021.rds")
source(here::here('R/utils.R'))


offenses_known_yearly_1960_2017 <-
  offenses_known_yearly_1960_2021 %>%
  filter(year %in% 1960:2017) %>%
  dplyr::filter(number_of_months_missing %in% 0)

offenses_known_yearly_2018_2021 <-
  offenses_known_yearly_1960_2021 %>%
  filter(year %in% 2018:2021) %>%
  dplyr::filter(last_month_reported %in% "december")

offenses_known_yearly_1960_2021 <-
  offenses_known_yearly_1960_2017 %>%
  bind_rows(offenses_known_yearly_2018_2021) %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA",
                ori    != "FL01394") %>%
  dplyr::mutate(agency = tolower(agency)) %>%
  dplyr::rename(ORI    = ori) %>%
  dplyr::select(starting_cols,
                dplyr::matches("act|clr|unfound|officer"))

offenses_known_yearly_1960_2021$agency <- gsub("\\(|\\)", "", offenses_known_yearly_1960_2021$agency)
offenses_known_yearly_1960_2021 <- remove_duplicate_capitalize_names(offenses_known_yearly_1960_2021)


setwd(here::here("data/offenses"))
make_agency_csvs(offenses_known_yearly_1960_2021)
make_largest_agency_json(offenses_known_yearly_1960_2021)
make_state_agency_choices(offenses_known_yearly_1960_2021)

