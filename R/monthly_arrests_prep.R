source(here::here('R/utils.R'))


for (year in 1974:2018) {
  setwd("C:/Users/user/Dropbox/R_project/crime_data/clean_data/arrests")
  load(paste0("ucr_arrests_monthly_all_crimes_totals_sex_", year, ".rda"))
  temp_sex <- get(paste0("ucr_arrests_monthly_all_crimes_totals_sex_", year))
  temp_sex <-
    temp_sex  %>%
    select(-matches("num_months"))
  rm(list = paste0("ucr_arrests_monthly_all_crimes_totals_sex_", year))

  load(paste0("ucr_arrests_monthly_all_crimes_totals_race_", year, ".rda"))
  temp_race <- get(paste0("ucr_arrests_monthly_all_crimes_totals_race_", year))
  rm(list = paste0("ucr_arrests_monthly_all_crimes_totals_race_", year))
  temp_race <-
    temp_race  %>%
    select(-matches("num_months"))


  temp_sex <-
    temp_sex %>%
    dplyr::filter(number_of_months_reported %in% 12) %>%
    dplyr::mutate(date = paste0(year, "-", month, "-1"),
                  date = lubridate::ymd(date)) %>%
    dplyr::select(-year) %>%
    dplyr::rename(year = date)
  common_names <- names(temp_sex)[names(temp_sex) %in% names(temp_race)]
  common_names <- common_names[!common_names %in% c("ori", "year")]

  temp_race <-
    temp_race %>%
    dplyr::filter(number_of_months_reported %in% 12) %>%
    dplyr::mutate(date = paste0(year, "-", month, "-1"),
                  date = lubridate::ymd(date)) %>%
    dplyr::select(-year,
                  -one_of(common_names)) %>%
    dplyr::rename(year = date)

  temp <- combine_sex_race_arrests(temp_sex,
                                   temp_race,
                                   crosswalk_agencies) %>%
    dplyr::filter(!is.na(ORI)) %>%
    dplyr::select(agency,
                  ORI,
                  year,
                  state,
                  population,
                  dplyr::everything())

  temp$year <- as.character(temp$year)
  rm(temp_race)
  rm(temp_sex)


  save_monthly_state_temp(temp, start_year = 1974, type = "arrests")
  rm(temp); gc(); Sys.sleep(1)
  message(year)
}



make_monthly_agency_csvs(type = "arrests")

setwd(here::here("data/arrests"))
files <- list.files(pattern = "largest_agency_choices")
file.copy(files, paste0(here::here("data/arrests_monthly/")), overwrite = TRUE)
