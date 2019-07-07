load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/LEOKA/leoka_yearly_1960_2017.rda")
source(here::here('R/utils.R'))


leoka <- reorder_leoka(leoka_yearly_1960_2017)
rm(leoka_yearly_1960_2017); gc();

leoka <- remove_duplicate_capitalize_names(leoka)


setwd(here::here("data/leoka"))
leoka_make_agency_csvs(leoka)
make_state_agency_choices(leoka)
make_largest_agency_json(leoka)
