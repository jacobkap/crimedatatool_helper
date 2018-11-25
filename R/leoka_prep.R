load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/LEOKA/leoka_yearly_1960_2017.rda")
source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/utils.R')


leoka <-
  leoka_yearly_1960_2017 %>%
  dplyr::filter(!state %in% c("guam",
                              "canal zone",
                              "puerto rico",
                              "virgin islands")) %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA") %>%
  dplyr::rename(ORI               = ori) %>%
  dplyr::select(starting_cols,
                number_of_months_reported,
                dplyr::matches("employees"),
                dplyr::matches("killed"),
                dplyr::matches("assaults_with_injury"),
                dplyr::matches("assaults_no_injury"),
                dplyr::matches("total_assault"),
                dplyr::matches("ambush"),
                dplyr::matches("oth_arrest"),
                dplyr::matches("burglary"),
                dplyr::matches("deranged"),
                dplyr::matches("disturbance"),
                dplyr::matches("prisoner"),
                dplyr::matches("riot"),
                dplyr::matches("robbery"),
                dplyr::matches("susp_pers"),
                dplyr::matches("traffic"),
                dplyr::matches("all_other"),
                -dplyr::matches("assist|alone|clear|veh"))
rm(leoka_yearly_1960_2017);

z = leoka[!duplicated(leoka$ORI),]
z$temp <- paste(z$agency, z$state)
z = z[duplicated(z$temp),]
leoka <- leoka[!leoka$ORI %in% z$ORI, ]
leoka$agency      <- sapply(leoka$agency, simpleCap)
leoka$state       <- sapply(leoka$state, simpleCap)
leoka$state       <- gsub(" Of ", " of ", leoka$state)

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/leoka")
leoka <- data.table::data.table(leoka)
for (selected_ori in sort(unique(leoka$ORI))) {
  temp   <- leoka[ORI %in% selected_ori]

  if (any(temp$number_of_months_reported %in% 12)) {

  temp   <- na_non_12_month_rows(temp)
  temp   <- dummy_rows_missing_years(temp)
  temp$number_of_months_reported <- NULL

  state  <- unique(temp$state)
  agency <- unique(temp$agency)
  state  <- gsub(" ", "_", state)
  agency <- gsub(" |:", "_", agency)
  agency <- gsub("/", "_", agency)
  agency <- gsub("_+", "_", agency)

  readr::write_csv(temp,
                   path = paste0(state, "_", agency, ".csv"))
  }
}

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/leoka")
for (selected_state in unique(leoka$state)) {
  temp   <- leoka[state %in% selected_state]
  agency <- unique(temp$agency)
  agency <- jsonlite::toJSON(agency, pretty = FALSE)
  write(agency, paste0(selected_state, "_agency_choices.json"))

}

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/leoka")
largest_agency <- leoka %>%
  dplyr::group_by(state) %>%
  dplyr::top_n(1, population) %>%
  dplyr::select(state, agency)
largest_agency <- jsonlite::toJSON(largest_agency, pretty = TRUE)
write(largest_agency, "largest_agency_choices.json")

rm(leoka); gc()


