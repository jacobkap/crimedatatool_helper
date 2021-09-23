police <- readRDS("D:/ucr_data_storage/clean_data/LEOKA/leoka_yearly_1960_2020.rds")
source(here::here('R/utils.R'))


police <- reorder_police(police)
police$agency <- gsub("\\(|\\)", "", police$agency)
police <- remove_duplicate_capitalize_names(police)


police_employees_only <-
  police %>%
  dplyr::filter(!number_of_months_reported %in% 12) %>%
  dplyr::select(agency,
                state,
                ORI,
                population,
                year,
                female_employees_civilians,
                female_employees_officers,
                female_employees_total,
                male_employees_civilians,
                male_employees_officers,
                male_employees_total,
                total_employees_civilians,
                total_employees_officers,
                total_employees_total)

police <-
  police %>%
  dplyr::filter(number_of_months_reported %in% 12) %>%
  dplyr::select(-number_of_months_reported) %>%
  dplyr::bind_rows(police_employees_only) %>%
  dplyr::arrange(ORI,
                 desc(year))

setwd(here::here("data/police"))
make_agency_csvs(police)
make_state_agency_choices(police)
make_largest_agency_json(police)
