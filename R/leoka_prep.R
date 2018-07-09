load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/LEOKA/leoka_yearly_1975_2016.rda")
source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/utils.R')
library(tidyverse)


leoka <-
  leoka_yearly_1975_2016 %>%
  dplyr::filter(!state %in% c("guam",
                              "canal zone",
                              "puerto rico",
                              "virgin islands"),
                record_indicator != "not updated, no police employee data") %>%
  dplyr::mutate(months_reported = paste(jan_month_indicator,
                                        feb_month_indicator,
                                        mar_month_indicator,
                                        apr_month_indicator,
                                        may_month_indicator,
                                        jun_month_indicator,
                                        jul_month_indicator,
                                        aug_month_indicator,
                                        sep_month_indicator,
                                        oct_month_indicator,
                                        nov_month_indicator,
                                        dec_month_indicator)) %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA") %>%
  dplyr::rename(ORI               = ori) %>%
  dplyr::select(starting_cols,
                "total_officers",
                "female_employees_officers",
                "male_employees_officers",
                "total_civilians",
                "female_employees_civilians",
                "male_employees_civilians",
                "officers_killed_accident",
                "officers_killed_felony",
                "assault_injury_total",
                "assault_injury_gun",
                "assault_injury_knife",
                "assault_injury_hand_feet",
                "assault_injury_other",

                "assault_no_injury_total",
                "assault_no_injury_gun",
                "assault_no_injury_knife" ,
                "assault_no_injury_hand_feet" ,
                "assault_no_injury_other",

                "total_assault_total",
                "total_assault_gun",
                "total_assault_knife",
                "total_assault_hand_feet",
                "total_assault_other",

                "all_oth_total_assault",
                "ambush_total_assault",
                "att_oth_arrest_total_assault",
                "burglary_total_assault",
                "civil_disorder_total_assault",
                "cust_prisoners_total_assault",
                "deranged_total_assault",
                "disturbance_total_assault",
                "robbery_total_assault",
                "susp_pers_total_assault",
                "traffic_total_assault",
                "months_reported")

assault_columns <- grep("assault|killed", names(leoka), value = TRUE)
for (col in assault_columns) {
  leoka[, col][grepl("not reported|deleted", leoka$months_reported)] <- NA
}
leoka$months_reported <- NULL



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
  state  <- unique(temp$state)
  agency <- unique(temp$agency)
  state  <- gsub(" ", "_", state)
  agency <- gsub(" |:", "_", agency)
  agency <- gsub("/", "_", agency)
  agency <- gsub("_+", "_", agency)
  data.table::fwrite(temp,
                     file = paste0(state, "_", agency, ".csv"))
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


