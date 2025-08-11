get_offenses_data <- function(type, crosswalk_data) {
  if (type %in% "year") {
    offenses_known <- readRDS("E:/ucr_data_storage/clean_data/offenses_known/offenses_known_yearly_1960_2024.rds")
  } else {

    files <- list.files(path = "E:/ucr_data_storage/clean_data/offenses_known/", pattern = "monthly.*rds$", full.names = TRUE)
    offenses_known <- vector("list", length = length(files))
    for (i in 1:length(files)) {
      temp <- readRDS(files[i])
      offenses_known[[i]] <- temp
      rm(temp)
      message(files[i])
    }
    offenses_known <- data.table::rbindlist(offenses_known) %>%
      as.data.frame() %>%
      mutate(year = date)
    gc()
  }

  offenses_known <-
    offenses_known %>%
    fix_missing_states() %>%
    fix_ori() %>%
    dplyr::filter(last_month_reported %in% "december") %>%
    dplyr::left_join(crosswalk_data) %>%
    dplyr::filter(agency != "NANA",
                  ori    != "FL01394") %>%
    dplyr::mutate(agency = tolower(agency)) %>%
    dplyr::rename(ORI    = ori) %>%
    dplyr::select(all_of(starting_cols),
                  dplyr::matches("act|clear|unfound|officer")) %>%
    mutate(agency = gsub("\\(|\\)", "", agency),
           agency = gsub("\\/", "-", agency))

  offenses_known <- remove_duplicate_capitalize_names(offenses_known)
  # Fxes NA issue
  offenses_known$state[offenses_known$ORI %in% "DEDEA01"] <- "Delaware"
  print(sort(unique(offenses_known$state)))

  if (type %in% "year") {
    setwd(here("data/offenses"))
    make_agency_csvs(offenses_known)
    make_largest_agency_json(offenses_known)
    make_state_agency_choices(offenses_known)
  } else {
    setwd(here("data/offenses_monthly"))
    make_agency_csvs(offenses_known, type = "month")

    setwd(here("data/offenses"))
    files <- list.files(pattern = "agency_choices")
    files
    file.copy(files, paste0(here::here("data/offenses_monthly/")), overwrite = TRUE)
  }
  rm(offenses_known); gc()
}
