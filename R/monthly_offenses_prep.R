source(here::here('R/utils.R'))
library(haven)
library(dplyr)

states <- c(tolower(state.name), "district of columbia")
for (selected_state in states) {
  final <- data.frame()
  setwd("C:/Users/user/Dropbox/R_project/crime_data/clean_data/offenses_known")
  for (year in 1960:2017) {
    load(paste0("offenses_known_monthly_", year, ".rda"))
    temp <- get(paste0("offenses_known_monthly_", year))
    rm(list = paste0("offenses_known_monthly_", year))

    temp <-
      temp %>%
      dplyr::filter(number_of_months_reported %in% 12,
                    state %in% selected_state) %>%
      dplyr::left_join(crosswalk_agencies, by = "ori") %>%
      dplyr::filter(agency != "NANA",
                    ori    != "FL01394") %>%
      dplyr::mutate(agency = tolower(agency)) %>%
      dplyr::select(-year) %>%
      dplyr::rename(ORI  = ori,
                    year = date) %>%
      dplyr::select(starting_cols,
                    dplyr::matches("act|clr|unfound|officer"),
                    -dplyr::matches("card"))

    final <- dplyr::bind_rows(final, temp)
    rm(temp); gc();
  }


  z = final[!duplicated(final$ORI), ]
  z$temp <- paste(z$agency, z$state)
  z = z[duplicated(z$temp), ]
  final <- final[!final$ORI %in% z$ORI, ]
  final$agency <- sapply(final$agency, simpleCap)
  final$state  <- sapply(final$state, simpleCap)


  setwd(here::here("data/offenses_monthly"))
  make_agency_csvs(final, yearly = FALSE)
  message(selected_state)
}
setwd(here::here("data/offenses"))
files <- list.files()
files <- files[grep("choices", files)]
  file.copy(files, paste0(here::here("data/offenses_monthly/")))
}