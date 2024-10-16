source('E:/Dropbox/R_project/crime_data/R/crosswalk.R')
source(here::here('R/utils.R'))
offenses_known_yearly_1960_2023 <- readRDS("F:/ucr_data_storage/clean_data/offenses_known/offenses_known_yearly_1960_2023.rds") %>%
  dplyr::filter(year %in% 2023) %>%
  dplyr::select(ori,
                population)

crosswalk <- read_merge_crosswalks()

crosswalk$agency_type           <- NULL
crosswalk$agency_subtype_1      <- NULL
crosswalk$agency_subtype_2      <- NULL
crosswalk$fips_county_code      <- NULL
crosswalk$fips_state_code       <- NULL
crosswalk$fips_state_place_code <- NULL
crosswalk$fips_place_code       <- NULL

crosswalk$crosswalk_agency_name <- sapply(crosswalk$crosswalk_agency_name,
                                          str_to_title)
crosswalk$census_name <-           sapply(crosswalk$census_name,
                                          str_to_title)

crosswalk$crosswalk_agency_name <- gsub(",", "",
                                        crosswalk$crosswalk_agency_name)
crosswalk$census_name           <- gsub(",", "",
                                        crosswalk$census_name)

crosswalk <-
  crosswalk %>%
  dplyr::left_join(offenses_known_yearly_1960_2023) %>%
  dplyr::arrange(desc(population))
crosswalk$population[is.na(crosswalk$population)]                         <- ""
crosswalk$ori9[is.na(crosswalk$ori9)]                                     <- ""
crosswalk$fips_state_county_code[is.na(crosswalk$fips_state_county_code)] <- ""
crosswalk$crosswalk_agency_name[is.na(crosswalk$crosswalk_agency_name)]   <- ""
crosswalk$census_name[is.na(crosswalk$census_name)]                       <- ""

crosswalk %>% sample_n(3)
crosswalk <-
  crosswalk %>%
  select(ori,
         ori9,
         crosswalk_agency_name,
         census_name,
         population,
         fips_state_county_code,
         state = address_state) %>%
  mutate(state = toupper(state))
crosswalk %>% sample_n(3)

setwd("~/crimedatatool_helper/data")
write_csv(crosswalk, file = "crosswalk.csv")
