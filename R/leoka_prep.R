load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/LEOKA/leoka_yearly_1960_2017.rda")
source(here::here('R/utils.R'))


police <- reorder_police(police_yearly_1960_2017)
rm(police_yearly_1960_2017); gc();

police <- remove_duplicate_capitalize_names(police)
police <-
  police %>%
  dplyr::filter(number_of_months_reported %in% 12) %>%
  dplyr::select(-number_of_months_reported)

setwd(here::here("data/police"))
make_agency_csvs(police)
make_state_agency_choices(police)
make_largest_agency_json(police)
