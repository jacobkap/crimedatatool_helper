source(here::here('R/utils.R'))
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/arrests/ucr_arrests_yearly_all_crimes_totals_race_1974_1989.rda")
ucr_arrests_yearly_all_crimes_totals_race_1974_1989 <-
  ucr_arrests_yearly_all_crimes_totals_race_1974_1989 %>%
  filter(number_of_months_reported %in% 12) %>%
  select(-matches("num_months"))
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/arrests/ucr_arrests_yearly_all_crimes_totals_sex_1974_1989.rda")
ucr_arrests_yearly_all_crimes_totals_sex_1974_1989 <-
  ucr_arrests_yearly_all_crimes_totals_sex_1974_1989 %>%
  filter(number_of_months_reported %in% 12) %>%
  select(-matches("num_months"))
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/arrests/ucr_arrests_yearly_all_crimes_totals_race_1990_2018.rda")
ucr_arrests_yearly_all_crimes_totals_race_1990_2018 <-
  ucr_arrests_yearly_all_crimes_totals_race_1990_2018 %>%
  filter(number_of_months_reported %in% 12) %>%
  select(-matches("num_months"))
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/arrests/ucr_arrests_yearly_all_crimes_totals_sex_1990_2018.rda")
ucr_arrests_yearly_all_crimes_totals_sex_1990_2018 <-
  ucr_arrests_yearly_all_crimes_totals_sex_1990_2018 %>%
  filter(number_of_months_reported %in% 12) %>%
  select(-matches("num_months"))


ucr_sex <-
  ucr_arrests_yearly_all_crimes_totals_sex_1974_1989 %>%
  bind_rows(ucr_arrests_yearly_all_crimes_totals_sex_1990_2018)
gc()
rm(ucr_arrests_yearly_all_crimes_totals_sex_1974_1989,
   ucr_arrests_yearly_all_crimes_totals_sex_1990_2018)
gc()
common_names <- names(ucr_sex)[names(ucr_sex) %in% names(ucr_arrests_yearly_all_crimes_totals_race_1974_1989)]
common_names <- common_names[!common_names %in% c("ori", "year")]

ucr_race <-
  ucr_arrests_yearly_all_crimes_totals_race_1974_1989 %>%
  bind_rows(ucr_arrests_yearly_all_crimes_totals_race_1990_2018) %>%
  dplyr::select(-one_of(common_names))
rm(ucr_arrests_yearly_all_crimes_totals_race_1974_1989,
   ucr_arrests_yearly_all_crimes_totals_race_1990_2018)
gc()

arrests <- combine_sex_race_arrests(ucr_sex,
                                    ucr_race,
                                    crosswalk_agencies)

rm(ucr_sex,
   ucr_race)
gc()



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
rm(arrests)