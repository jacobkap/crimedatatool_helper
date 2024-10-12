offenses_known_yearly_1960_2023 <- readRDS("F:/ucr_data_storage/clean_data/offenses_known/offenses_known_yearly_1960_2023.rds")
table(offenses_known_yearly_1960_2023$year,
      offenses_known_yearly_1960_2023$number_of_months_missing)
table(offenses_known_yearly_1960_2023$year,
      offenses_known_yearly_1960_2023$last_month_reported)

source(here::here('R/utils.R'))


offenses_known_yearly_1960_2023 <-
  offenses_known_yearly_1960_2023 %>%
  dplyr::filter(last_month_reported %in% "december")

offenses_known_yearly_1960_2023 <-
  offenses_known_yearly_1960_2023 %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA",
                ori    != "FL01394") %>%
  dplyr::mutate(agency = tolower(agency)) %>%
  dplyr::rename(ORI    = ori) %>%
  dplyr::select(all_of(starting_cols),
                dplyr::matches("act|clr|unfound|officer"))

offenses_known_yearly_1960_2023$agency <- gsub("\\(|\\)", "", offenses_known_yearly_1960_2023$agency)
offenses_known_yearly_1960_2023$agency <- gsub("\\/", "-", offenses_known_yearly_1960_2023$agency)
offenses_known_yearly_1960_2023 <- remove_duplicate_capitalize_names(offenses_known_yearly_1960_2023)
# Fxes NA issue
offenses_known_yearly_1960_2023$state[offenses_known_yearly_1960_2023$ORI %in% "DEDEA01"] <- "Delaware"


setwd(here("data/offenses"))
make_agency_csvs(offenses_known_yearly_1960_2023)
make_largest_agency_json(offenses_known_yearly_1960_2023)
make_state_agency_choices(offenses_known_yearly_1960_2023)

