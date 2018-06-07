load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/ASR/asr_drug_liquor_crimes_1980_2015.rda")
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/ASR/asr_violent_or_sex_crimes_1980_2015.rda")
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/ASR/asr_financial_crimes_1980_2015.rda")
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/ASR/asr_index_crimes_1980_2015.rda")
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/ASR/asr_other_crimes_1980_2015.rda")

load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/ASR/asr_simple_1980_2015.rda")
load("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/crosswalk_agencies.rda")
library(tidyverse)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

prep_arrests(asr_drug_liquor_crimes_1980_2015)
prep_arrests(asr_violent_or_sex_crimes_1980_2015)
prep_arrests(asr_financial_crimes_1980_2015)
prep_arrests(asr_other_crimes_1980_2015)
prep_arrests(asr_index_crimes_1980_2015)

prep_arrests <- function(data) {
  setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/arrests")
  data$agency_name <- sapply(data$agency_name, simpleCap)
  data$state       <- sapply(data$state, simpleCap)
  data$state       <- gsub(" Of ", " of ", data$state)

  crimes <- grep("_tot_arrests", names(data), value = TRUE)
  crimes <- gsub("_tot_arrests", "", crimes)
  for (state_val in unique(data$state)) {
    for (crime in crimes) {
      cols_to_keep <- c(keep_cols,
                        grep(crime, names(data), value = TRUE))
      temp <-
        data %>%
        dplyr::filter(state == state_val) %>%
        dplyr::rename(ORI               = ori,
                      ORI9              = ori9,
                      FIPS_state_code   = fips_state_code,
                      FIPS_county_code  = fips_county_code,
                      FIPS_state_county_code  = fips_state_county_code,
                      agency            = agency_name) %>%
        dplyr::select(cols_to_keep)

      if (state_val != "Guam")
      write_csv(temp, path = paste0("arrests_", state_val, "_", crime, ".csv"))
    }
  }
}


keep_cols <- c("agency",
               "year",
               "state",
               "state_abb",
               "population",
               "ORI",
               "ORI9",
               "FIPS_state_code",
               "FIPS_county_code",
               "FIPS_state_county_code")


# Make state-agency json
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/ASR/asr_simple_1980_2015.rda")
z = asr_simple_1980_2015[, c("state", "agency_name", "ori")]
names(z) <- gsub("agency_name", "agency", names(z))
z = z[!duplicated(z$ori), ]
z$agency <- sapply(z$agency, simpleCap)
z$state       <- sapply(z$state, simpleCap)
z$state       <- gsub(" Of ", " of ", z$state)
library(jsonlite)
z <- toJSON(z, pretty=TRUE)
write(z, "arrests_state_agencies.json")
