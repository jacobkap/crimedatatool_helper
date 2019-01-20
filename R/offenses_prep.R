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
ucr$state <- sapply(ucr$state, simpleCap)
ucr$state <- gsub(" Of ", " of ", ucr$state)


setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/offenses")
ucr <- data.table::data.table(ucr)
pb <- txtProgressBar(min = 0, max = length(unique(ucr$ORI)), style = 3)
for (i in 1:length(unique(ucr$ORI))) {
  selected_ori = unique(ucr$ORI)[i]
  temp   <- ucr[ORI %in% selected_ori]
  temp   <- dummy_rows_missing_years(temp, type = "crime")

  state  <- unique(temp$state)
  agency <- unique(temp$agency)
  state  <- gsub(" ", "_", state)
  agency <- gsub(" |:", "_", agency)
  agency <- gsub("/", "_", agency)
  agency <- gsub("_+", "_", agency)
  readr::write_csv(temp,
                   path = paste0(state, "_", agency, ".csv"))

  setTxtProgressBar(pb, i)    # update progress bar
}
close(pb)

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/offenses")
for (selected_state in unique(ucr$state)) {
  temp   <- ucr[state %in% selected_state]
  agency <- unique(temp$agency)
  agency <- jsonlite::toJSON(agency, pretty = FALSE)
  write(agency, paste0(selected_state, "_agency_choices.json"))
}

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/offenses")
largest_agency <- ucr %>%
  dplyr::group_by(state) %>%
  dplyr::top_n(1, population) %>%
  dplyr::select(state, agency)
largest_agency <- jsonlite::toJSON(largest_agency, pretty = TRUE)
write(largest_agency, "largest_agency_choices.json")
rm(ucr); gc()
