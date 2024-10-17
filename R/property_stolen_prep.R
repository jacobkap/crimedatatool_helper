get_property_stolen_data <- function(type, crosswalk_data) {
  if (type %in% "year") {
    property_stolen <- readRDS("F:/ucr_data_storage/clean_data/combined_years/srs/property_stolen_and_recovered_yearly_1960_2023.rds")
  } else {
    property_stolen <- readRDS("F:/ucr_data_storage/clean_data/combined_years/srs/property_stolen_and_recovered_monthly_1960_2023.rds") %>%
      mutate(year = date)
  }


  property_stolen <-
    property_stolen %>%
    fix_missing_states() %>%
    fix_ori() %>%
    dplyr::filter(!state %in% c("guam",
                                "virgin islands",
                                "canal zone",
                                "american samoa",
                                "bahamas",
                                "puerto rico"),
                  !ori %in% c("FL01394",
                              "OKDI001"),
                  !is.na(state)) %>%
    dplyr::left_join(crosswalk_data) %>%
    dplyr::mutate(agency = tolower(agency)) %>%
    keep_most_common_agency_name()


  inflation_adjust <- data.frame(year = sort(unique(property_stolen$year)), price = 1)
  inflation_adjust$in_2023_dollars <- adjust_for_inflation(inflation_adjust$price,
                                                           inflation_adjust$year,
                                                           "US",
                                                           to_date = 2023)


  property_stolen$agency[is.na(property_stolen$agency)] <- property_stolen$agency_name[is.na(property_stolen$agency)]

  property_stolen <-
    property_stolen %>%
    dplyr::rename(ORI    = ori) %>%
    dplyr::select(all_of(starting_cols),
                  dplyr::matches("offense|auto|value")) %>%
    mutate(agency = gsub("\\(|\\)", "", agency),
           agency = gsub("\\/", "-", agency)) %>%
    left_join(inflation_adjust)

  value_cols <- grep("value", names(property_stolen), value = TRUE)
  property_stolen <- data.frame(property_stolen)
  for (value_col in value_cols) {
    property_stolen$temp <- property_stolen[, value_col]
    property_stolen$temp <- round(property_stolen$temp * property_stolen$in_2023_dollars, 0)
    property_stolen[, value_col] <- property_stolen$temp
    property_stolen$temp <- NULL
  }


  property_stolen <- remove_duplicate_capitalize_names(property_stolen)
  print(sort(unique(property_stolen$state), na.last = TRUE))

  if (type %in% "year") {
    setwd(here("data/property_stolen"))
    make_agency_csvs(property_stolen)
    make_largest_agency_json(property_stolen)
    make_state_agency_choices(property_stolen)
  } else {
    setwd(here("data/property_stolen_monthly"))
    make_agency_csvs(property_stolen, type = "month")

    setwd(here("data/property_stolen"))
    files <- list.files(pattern = "agency_choices")
    files
    file.copy(files, paste0(here::here("data/property_stolen_monthly/")), overwrite = TRUE)
  }
}
