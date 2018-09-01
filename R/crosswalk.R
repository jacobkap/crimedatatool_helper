source('C:/Users/user/Dropbox/R_project/crime_data/R_code/crosswalk.R')
source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/utils.R')
library(readr)
crosswalk <- read_merge_crosswalks(pop = TRUE)

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
  dplyr::arrange(desc(population))

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data")
write_csv(crosswalk, path = "crosswalk.csv")
