source(here::here('R/utils.R'))


for (year in 1972:2017) {
  setwd("C:/Users/user/Dropbox/R_project/crime_data/clean_data/LEOKA")
  load(paste0("leoka_monthly_", year, ".rda"))
  temp <- get(paste0("leoka_monthly_", year))
  rm(list = paste0("leoka_monthly_", year))
  temp <-
    temp %>%
    dplyr::filter(number_of_months_reported %in% 12) %>%
    dplyr::select(-year) %>%
    dplyr::rename(year = date)


  temp <- reorder_leoka(temp)

  save_monthly_state_temp(temp, start_year = 1972, type = "leoka")
  rm(temp); gc()
  message(year)

}


make_monthly_agency_csvs(type = "leoka")

setwd(here::here("data/leoka"))
files <- list.files(pattern = "largest_agency_choices")
file.copy(files, paste0(here::here("data/leoka_monthly/")))
