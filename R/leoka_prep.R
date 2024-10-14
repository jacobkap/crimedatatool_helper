get_leoka_data <- function(type, crosswalk_data) {
  if (type %in% "year") {
    police <- readRDS("F:/ucr_data_storage/clean_data/LEOKA/leoka_yearly_1960_2023.rds") %>%
      filter(!ori %in% "FL01394")
  } else {
    police <- readRDS("F:/ucr_data_storage/clean_data/combined_years/srs/leoka_monthly_1960_2023.rds") %>%
      filter(!ori %in% "FL01394") %>%
      mutate(year = date)
  }

  police        <- fix_missing_states(police)
  police        <- fix_ori(police)
  police        <- reorder_police(police, crosswalk_data)
  police$agency <- gsub("\\(|\\)", "", police$agency)
  police        <- remove_duplicate_capitalize_names(police)


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
                   desc(year)) %>%
    dplyr::filter(!state %in% "NANA",
                  !ORI %in% "GAUSMO4")
  rm(police_employees_only); gc()

  sort(unique(police$state))
  unique(police$ORI[police$state %in% 98])
  unique(police$agency[police$state %in% 98])
  police$state[police$ORI %in% "DCPPD00"] <- "District of Columbia"
  police$state[police$ORI %in% "DCZPP00"] <- "District of Columbia"
  police$state[police$ORI %in% "DEDEA01"] <- "Delaware"
  police$state[police$ORI %in% "MDPPD00"] <- "District of Columbia"
  police$state[police$ORI %in% "VAPPD00"] <- "Virginia"
  police$agency[police$agency %in% "USPark Police"] <- "US Park Police"
  print(sort(unique(police$state)))

  if (type %in% "year") {
    setwd(here::here("data/police"))
    make_agency_csvs(police)
    make_state_agency_choices(police)
    make_largest_agency_json(police)
  } else {
    setwd(here("data/police_monthly"))
    make_agency_csvs(police, type = "month")

    setwd(here::here("data/police"))
    files <- list.files(pattern = "largest_agency_choices")
    file.copy(files, here::here("data/police_monthly/"), overwrite = TRUE)
  }

}
