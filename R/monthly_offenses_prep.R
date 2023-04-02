source(here::here('R/utils.R'))

for (year in 1960:2021) {
  setwd("D:/ucr_data_storage/clean_data/offenses_known")
  temp <- readRDS(paste0("offenses_known_monthly_", year, ".rds"))

  temp <-
    temp %>%
    dplyr::filter(number_of_months_missing %in% 0)

  if (year %in% 2018:2021) {
    temp <-
      temp %>%
      dplyr::filter(last_month_reported %in% "december")
  }

 temp <-
   temp %>%
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
file.copy(files, paste0(here::here("data/offenses_monthly/")), overwrite = TRUE)
