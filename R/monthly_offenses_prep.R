source('R/utils.R')

final <- data.frame()
for (year in 1960:2021) {
  setwd("E:/ucr_data_storage/clean_data/offenses_known")
  temp <- readRDS(paste0("offenses_known_monthly_", year, ".rds"))

  temp <-
    temp %>%
    dplyr::filter(number_of_months_missing %in% 0)


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
 final <- bind_rows(final, temp)
 message(year)
}


make_state_agency_choices(final)
files <- list.files(pattern = "largest_agency_choices")
setwd(here("data/offenses_monthly"))
file.copy(files, paste0(here::here("data/offenses_monthly/")), overwrite = TRUE)
make_state_agency_choices(final)
make_agency_csvs(final, type = "month")