source('~/crimedatatool_helper/R/utils.R')
setwd("D:/ucr_data_storage/clean_data/arrests")
load("ucr_arrests_yearly_all_crimes_race_1974_1994.rda")
ucr_arrests_yearly_all_crimes_race_1974_1994 <-
  ucr_arrests_yearly_all_crimes_race_1974_1994 %>%
  filter(number_of_months_reported %in% 12) %>%
  select(-matches("num_months"))
load("ucr_arrests_yearly_all_crimes_sex_1974_1994.rda")
ucr_arrests_yearly_all_crimes_sex_1974_1994 <-
  ucr_arrests_yearly_all_crimes_sex_1974_1994 %>%
  filter(number_of_months_reported %in% 12) %>%
  select(-matches("num_months"))
load("ucr_arrests_yearly_all_crimes_race_1995_2018.rda")
ucr_arrests_yearly_all_crimes_race_1995_2018 <-
  ucr_arrests_yearly_all_crimes_race_1995_2018 %>%
  filter(number_of_months_reported %in% 12) %>%
  select(-matches("num_months"))
load("ucr_arrests_yearly_all_crimes_sex_1995_2018.rda")
ucr_arrests_yearly_all_crimes_sex_1995_2018 <-
  ucr_arrests_yearly_all_crimes_sex_1995_2018 %>%
  filter(number_of_months_reported %in% 12) %>%
  select(-matches("num_months"))


ucr_sex <-
  ucr_arrests_yearly_all_crimes_sex_1974_1994 %>%
  bind_rows(ucr_arrests_yearly_all_crimes_sex_1995_2018)
gc()
rm(ucr_arrests_yearly_all_crimes_sex_1974_1994,
   ucr_arrests_yearly_all_crimes_sex_1995_2018)
gc()
common_names <- names(ucr_sex)[names(ucr_sex) %in% names(ucr_arrests_yearly_all_crimes_race_1974_1994)]
common_names <- common_names[!common_names %in% c("ori", "year")]

ucr_race <-
  ucr_arrests_yearly_all_crimes_race_1974_1994 %>%
  bind_rows(ucr_arrests_yearly_all_crimes_race_1995_2018) %>%
  dplyr::select(-one_of(common_names))
rm(ucr_arrests_yearly_all_crimes_race_1974_1994,
   ucr_arrests_yearly_all_crimes_race_1995_2018)
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

setwd("~/crimedatatool_helper/data/arrests")
make_agency_csvs(arrests)
make_state_agency_choices(arrests)
make_largest_agency_json(arrests)
rm(arrests)