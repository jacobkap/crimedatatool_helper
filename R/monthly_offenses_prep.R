source("R/utils.R")

final <- data.frame()
for (year in 1960:2023) {
  setwd("F:/ucr_data_storage/clean_data/offenses_known")
  temp <- readRDS(paste0("offenses_known_monthly_", year, ".rds"))

  temp <-
    temp %>%
    dplyr::filter(number_of_months_missing %in% 0)


  temp <-
    temp %>%
    dplyr::left_join(crosswalk_agencies, by = "ori") %>%
    dplyr::filter(
      agency != "NANA",
      ori != "FL01394"
    ) %>%
    dplyr::mutate(agency = tolower(agency)) %>%
    dplyr::select(-year) %>%
    dplyr::rename(
      ORI = ori,
      year = date
    ) %>%
    dplyr::select(
      starting_cols,
      dplyr::matches("act|clr|unfound|officer"),
      -dplyr::matches("card")
    )
  final <- bind_rows(final, temp)
  message(year)
}


final$agency <- gsub("\\(|\\)", "", final$agency)
final$agency <- gsub("\\/", "-", final$agency)
final <- remove_duplicate_capitalize_names(final)
# Fixes NA issue in 2021.
final$state[final$ORI %in% "DEDEA01"] <- "Delaware"

setwd(here("data/offenses_monthly"))
make_agency_csvs(final, type = "month")

setwd(here("data/offenses"))
files <- list.files(pattern = "agency_choices")
files
file.copy(files, paste0(here::here("data/offenses_monthly/")), overwrite = TRUE)
