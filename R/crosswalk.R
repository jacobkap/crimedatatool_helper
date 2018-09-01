source('C:/Users/user/Dropbox/R_project/crime_data/R_code/crosswalk.R')
source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/utils.R')
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/offenses_known/ucr_offenses_known_yearly_1960_2016.rda")
library(dplyr)
ucr_offenses_known_yearly_1960_2016 <-
  ucr_offenses_known_yearly_1960_2016 %>%
  dplyr::filter(year %in% 2016) %>%
  dplyr::select(ori,
                total_population) %>%
  dplyr::rename(population = total_population)

library(readr)
crosswalk <- read_merge_crosswalks()

crosswalk$agency_type           <- NULL
crosswalk$agency_subtype_1      <- NULL
crosswalk$agency_subtype_2      <- NULL
crosswalk$fips_county_code      <- NULL
crosswalk$fips_state_code       <- NULL
crosswalk$fips_state_place_code <- NULL
crosswalk$fips_place_code       <- NULL

crosswalk$crosswalk_agency_name <- sapply(crosswalk$crosswalk_agency_name,
                                          simpleCap)
crosswalk$census_name <-           sapply(crosswalk$census_name,
                                          simpleCap)

crosswalk$crosswalk_agency_name <- gsub(",", "",
                                        crosswalk$crosswalk_agency_name)
crosswalk$census_name           <- gsub(",", "",
                                        crosswalk$census_name)

crosswalk <-
  crosswalk %>%
  dplyr::left_join(ucr_offenses_known_yearly_1960_2016) %>%
  dplyr::arrange(desc(population))
crosswalk$population[is.na(crosswalk$population)]                         <- ""
crosswalk$ori9[is.na(crosswalk$ori9)]                                     <- ""
crosswalk$fips_state_county_code[is.na(crosswalk$fips_state_county_code)] <- ""
crosswalk$crosswalk_agency_name[is.na(crosswalk$crosswalk_agency_name)]   <- ""
crosswalk$census_name[is.na(crosswalk$census_name)]                       <- ""

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data")
write_csv(crosswalk, path = "crosswalk.csv")
