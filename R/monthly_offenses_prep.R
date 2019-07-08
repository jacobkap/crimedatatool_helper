source(here::here('R/utils.R'))

for (year in 1960:2017) {
  setwd("C:/Users/user/Dropbox/R_project/crime_data/clean_data/offenses_known")

  load(paste0("offenses_known_monthly_", year, ".rda"))
  temp <- get(paste0("offenses_known_monthly_", year))
  rm(list = paste0("offenses_known_monthly_", year))

  temp <-
    temp %>%
    dplyr::filter(number_of_months_reported %in% 12) %>%
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

  save_monthly_state_temp(temp, start_year = 1960, type = "offenses")
  rm(temp); gc()
  message(year)
}



make_monthly_agency_csvs(type = "offenses")

setwd(here::here("data/offenses"))
files <- list.files(pattern = "largest_agency_choices")
file.copy(files, paste0(here::here("data/offenses_monthly/")))
