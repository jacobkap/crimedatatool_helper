source(here::here('R/utils.R'))
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/arrests/ucr_arrests_yearly_all_crimes_totals_race_1974_2016.rda")
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/arrests/ucr_arrests_yearly_all_crimes_totals_sex_1974_2016.rda")

ucr_arrests_yearly_all_crimes_totals_sex_1974_2018 <-
  ucr_arrests_yearly_all_crimes_totals_sex_1974_2016 %>%
  dplyr::filter(number_of_months_reported %in% 12)
gc()
common_names <- names(ucr_arrests_yearly_all_crimes_totals_sex_1974_2016)[names(ucr_arrests_yearly_all_crimes_totals_sex_1974_2016) %in% names(ucr_arrests_yearly_all_crimes_totals_race_1974_2016)]
common_names <- common_names[!common_names %in% c("ori", "year")]

ucr_arrests_yearly_all_crimes_totals_race_1974_2016 <-
  ucr_arrests_yearly_all_crimes_totals_race_1974_2016 %>%
  dplyr::filter(number_of_months_reported %in% 12) %>%
  dplyr::select(-one_of(common_names))
gc()

arrests <- combine_sex_race_arrests(ucr_arrests_yearly_all_crimes_totals_sex_1974_2016,
                                    ucr_arrests_yearly_all_crimes_totals_race_1974_2016,
                                    crosswalk_agencies)

rm(ucr_arrests_yearly_all_crimes_totals_sex_1974_2016); gc()
rm(ucr_arrests_yearly_all_crimes_totals_race_1974_2016); gc()


arrests <-
  arrests %>%
  dplyr::select(agency,
                ORI,
                year,
                state,
                population,
                dplyr::everything())

arrests <- remove_duplicate_capitalize_names(arrests)

setwd(here::here("data/arrests"))
make_agency_csvs(arrests)
make_state_agency_choices(arrests)
make_largest_agency_json(arrests)
