get_arson_data <- function(type, crosswalk_data) {
  if (type %in% "year") {
    arson <- readRDS("E:/ucr_data_storage/clean_data/arson/arson_yearly_1979_2024.rds")
  } else {
    files <- list.files(path = "E:/ucr_data_storage/clean_data/arson/", pattern = "monthly.*rds$", full.names = TRUE)
    arson <- vector("list", length = length(files))
    for (i in 1:length(files)) {
      temp <- readRDS(files[i])
      arson[[i]] <- temp
      rm(temp)
      message(files[i])
    }
    arson <- data.table::rbindlist(arson) %>%
      as.data.frame() %>%
      mutate(year = date)
    gc()
  }


  arson <-
    arson %>%
    fix_missing_states() %>%
    fix_ori() %>%
    dplyr::filter(last_month_reported %in% "december",
                  !state %in% c("guam",
                                "virgin islands"),
                  !ori %in% "FL01394") %>%
    dplyr::left_join(crosswalk_data) %>%
    dplyr::mutate(agency = tolower(agency)) %>%
    keep_most_common_agency_name()


  inflation_adjust <- data.frame(year = sort(unique(arson$year)), price = 1)
  inflation_adjust$in_current_dollars <- adjust_for_inflation(inflation_adjust$price,
                                                           inflation_adjust$year,
                                                           "US",
                                                           to_date = 2023)


  arson$agency[is.na(arson$agency)] <- arson$agency_name[is.na(arson$agency)]

  arson <-
    arson %>%
    dplyr::rename(ORI    = ori) %>%
    dplyr::select(all_of(starting_cols),
                  dplyr::matches("act|clear|unfound|estimated_damage")) %>%
    mutate(agency = gsub("\\(|\\)", "", agency),
           agency = gsub("\\/", "-", agency)) %>%
    left_join(inflation_adjust) %>%
    mutate(
      estimated_damage_single_occupancy = round(estimated_damage_single_occupancy * in_current_dollars, 0),
      estimated_damage_other_residential = round(estimated_damage_other_residential * in_current_dollars, 0),
      estimated_damage_storage = round(estimated_damage_storage * in_current_dollars, 0),
      estimated_damage_industrial = round(estimated_damage_industrial * in_current_dollars, 0),
      estimated_damage_other_commercial = round(estimated_damage_other_commercial * in_current_dollars, 0),
      estimated_damage_community_public = round(estimated_damage_community_public * in_current_dollars, 0),
      estimated_damage_all_other_structures = round(estimated_damage_all_other_structures * in_current_dollars, 0),
      estimated_damage_total_structures = round(estimated_damage_total_structures * in_current_dollars, 0),
      estimated_damage_motor_vehicles = round(estimated_damage_motor_vehicles * in_current_dollars, 0),
      estimated_damage_other_mobile = round(estimated_damage_other_mobile * in_current_dollars, 0),
      estimated_damage_total_mobile = round(estimated_damage_total_mobile * in_current_dollars, 0),
      estimated_damage_all_other = round(estimated_damage_all_other * in_current_dollars, 0),
      estimated_damage_grand_total = round(estimated_damage_grand_total * in_current_dollars, 0))

  names(arson) <- gsub("all_other$", "all_other_arsons", names(arson))


  arson <- remove_duplicate_capitalize_names(arson)
  print(sort(unique(arson$state)))
  arson$price <- NULL
  arson$in_current_dollars <- NULL
  if (type %in% "year") {
    setwd(here("data/arson"))
    make_agency_csvs(arson)
    make_largest_agency_json(arson)
    make_state_agency_choices(arson)
  } else {
    setwd(here("data/arson_monthly"))
    make_agency_csvs(arson, type = "month")

    setwd(here("data/arson"))
    files <- list.files(pattern = "agency_choices")
    files
    file.copy(files, paste0(here::here("data/arson_monthly/")), overwrite = TRUE)
  }
  rm(arson); gc()
}
