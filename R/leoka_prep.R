load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/LEOKA/leoka_yearly_1975_2015.rda")
library(tidyverse)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

data <-
  leoka_yearly_1975_2015 %>%
  dplyr::rename(agency            = agency_name,
                ORI               = ori,
                ORI9              = ori9,
                FIPS_state_code   = fips_state_code,
                FIPS_county_code  = fips_county_code,
                FIPS_state_county_code  = fips_state_county_code) %>%
  dplyr::select("agency",
         "ORI",
         "ORI9",
         "year",
         "state",
         "population",
         "FIPS_state_code",
         "FIPS_county_code",
         "FIPS_state_county_code",
         "total_officers",
         "male_employees_officers",
         "female_employees_officers",
         "total_civilians",
         "male_employees_civilians",
         "female_employees_civilians",
         "officers_killed_felony",
         "officers_killed_accident",
         dplyr::starts_with("total_assault_"),
         dplyr::ends_with("_total_assault")
         )

prep_leoka(data)

prep_leoka <- function(data) {
  setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/leoka")
  data$agency      <- sapply(data$agency, simpleCap)
  data$state       <- sapply(data$state, simpleCap)
  data$state       <- gsub(" Of ", " of ", data$state)

  for (state_val in unique(data$state)) {
    temp <-
      data %>%
      dplyr::filter(state == state_val)

    if (!state_val %in% c("Guam", "Canal Zone", "Virgin Islands", "Puerto Rico")) {
      readr::write_csv(temp, path = paste0("arrests_", state_val, ".csv"))
    }
  }
}
