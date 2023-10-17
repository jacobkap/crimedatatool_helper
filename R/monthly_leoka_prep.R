source(here::here("R/utils.R"))


for (year in 1972:2022) {
  setwd("F:/ucr_data_storage/clean_data/LEOKA")
  temp <- readRDS(paste0("leoka_monthly_", year, ".rds"))

  temp <-
    temp %>%
    dplyr::filter(number_of_months_reported %in% 12) %>%
    dplyr::select(-year) %>%
    dplyr::rename(year = date)


  temp <- reorder_police(temp)

  save_monthly_state_temp(temp, start_year = 1972, type = "leoka")
  rm(temp)
  gc()
  message(year)
}


make_monthly_agency_csvs(type = "police")
setwd(here::here("data/police"))
files <- list.files(pattern = "largest_agency_choices")
file.copy(files, here::here("data/police_monthly/"), overwrite = TRUE)
