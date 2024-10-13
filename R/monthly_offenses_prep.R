source("R/utils.R")
offenses_known_monthly_1960_2023 <- readRDS("F:/ucr_data_storage/clean_data/combined_years/srs/offenses_known_monthly_1960_2023.rds") %>%
  dplyr::filter(last_month_reported %in% "december") %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA",
                ori    != "FL01394") %>%
  dplyr::mutate(agency = tolower(agency),
                year = date) %>%
  dplyr::rename(ORI    = ori) %>%
  dplyr::select(all_of(starting_cols),
                dplyr::matches("act|clr|unfound|officer")) %>%
  mutate(agency = gsub("\\(|\\)", "", agency),
         agency = gsub("\\/", "-", agency))


offenses_known_monthly_1960_2023 <- remove_duplicate_capitalize_names(offenses_known_monthly_1960_2023)
# Fxes NA issue
offenses_known_monthly_1960_2023$state[offenses_known_monthly_1960_2023$ORI %in% "DEDEA01"] <- "Delaware"


setwd(here("data/offenses_monthly"))
make_agency_csvs(offenses_known_monthly_1960_2023, type = "month")

setwd(here("data/offenses"))
files <- list.files(pattern = "agency_choices")
files
file.copy(files, paste0(here::here("data/offenses_monthly/")), overwrite = TRUE)
