library(tidyverse)
library(data.table)
library(fastDummies)
library(splitstackshape)
library(readr)
library(here)
library(dplyr)
library(lubridate)
source(here("R/utils_objects.R"))
load(here("data/crosswalk_agencies.rda"))

states <- c(tolower(state.name), "district of columbia")

state_name <- c(state.name, "District of Columbia")
state_abb   <- c(state.abb, "DC")

fix_column_names <- function(names) {
  names <- tolower(names)
  names <- gsub(" |-|\\/|\\.", "_", names)
  names <- gsub("_+", "_", names)
  return(names)
}

remove_duplicate_capitalize_names <- function(data) {
  z = data[!duplicated(data$ORI),]
  z$temp <- paste(z$agency, z$state)
  z = z[duplicated(z$temp),]
  data <- data[!data$ORI %in% z$ORI, ]
  data$agency <- sapply(data$agency, simpleCap)
  data$state  <- sapply(data$state, simpleCap)

  return(data)
}

make_state_agency_choices <- function(data) {
  data <- data.table::as.data.table(data)
  for (selected_state in unique(data$state)) {
    temp   <- data[state %in% selected_state]
    agency <- unique(temp$agency)

    agency <- jsonlite::toJSON(agency, pretty = FALSE)
    write(agency, paste0(selected_state, "_agency_choices.json"))
  }
}


clean_cdc_colnames <- function(data) {
  names(data) <- data[1, ]
  data <- data[-1, ]
  names(data) <- gsub("\\ ", "_", names(data))
  names(data) <- tolower(names(data))
  return(data)
}

make_largest_agency_json <- function(data) {
  largest_agency <- data %>%
    dplyr::group_by(state) %>%
    dplyr::top_n(1, population) %>%
    dplyr::select(state, agency)
  largest_agency <- jsonlite::toJSON(largest_agency, pretty = TRUE)
  write(largest_agency, "largest_agency_choices.json")
}

reorder_police <- function(data) {
  employee_cols <- sort(grep("employee", names(data),
                             value = TRUE))
  killed_cols <- sort(grep("killed", names(data),
                           value = TRUE))
  injury_cols <- sort(grep("with_injury", names(data),
                           value = TRUE))
  injury_cols <- injury_cols[!grepl("indicator", injury_cols)]
  no_injury_cols <- sort(grep("no_injury", names(data),
                              value = TRUE))
  no_injury_cols <- no_injury_cols[!grepl("indicator", no_injury_cols)]
  total_assault_cols <- sort(grep("^total_assault", names(data),
                                  value = TRUE))
  total_assault_cols <- total_assault_cols[!grepl("clear|traffic", total_assault_cols)]
  assaults <- sort(grep("_assault_|.total_assaults", names(data),
                        value = TRUE))
  assaults <- assaults[!grepl("^total_assault|time|all_other", assaults)]
  all_other_assaults <- sort(grep("all_other_assault", names(data),
                                  value = TRUE))
  all_other_assaults <- all_other_assaults[!grepl("clear", all_other_assaults)]

  data <-
    data %>%
    dplyr::filter(!state %in% c("guam",
                                "canal zone",
                                "puerto rico",
                                "virgin islands")) %>%
    dplyr::left_join(crosswalk_agencies, by = "ori") %>%
    dplyr::filter(agency != "NANA") %>%
    dplyr::rename(ORI               = ori) %>%
    dplyr::select(all_of(starting_cols),
                  all_of(number_of_months_reported),
                  all_of(employee_cols),
                  all_of(killed_cols),
                  all_of(injury_cols),
                  all_of(no_injury_cols),
                  all_of(assaults),
                  all_of(all_other_assaults),
                  all_of(total_assault_cols))
  return(data)
}

police_make_agency_csvs <- function(data) {
  data <- data.table::data.table(data)
  pb <- txtProgressBar(min = 0, max = length(unique(data$ORI)), style = 3)
  for (selected_ori in sort(unique(data$ORI))) {
    temp   <- data[ORI %in% selected_ori]

    if (any(temp$number_of_months_reported %in% 12)) {

      temp   <- na_non_12_month_rows(temp)
      temp   <- dummy_rows_missing_years(temp)
      temp$number_of_months_reported <- NULL

      state  <- unique(temp$state)
      agency <- unique(temp$agency)
      state  <- gsub(" ", "_", state)
      agency <- gsub(" |:", "_", agency)
      agency <- gsub("/", "_", agency)
      agency <- gsub("_+", "_", agency)

      readr::write_csv(temp,
                       path = paste0(state, "_", agency, ".csv"))
      setTxtProgressBar(pb, i)    # update progress bar

    }
  }
  close(pb)

}

make_monthly_agency_csvs <- function(type) {

  for (state_group in 1:51) {
    setwd(here("data/temp"))
    if (type == "police") {
      type <- "leoka"
    }
    load(paste0("monthly_", type, "_state_group_", state_group, ".rda"))
    temp_state$agency <- gsub("\\(|\\)", "", temp_state$agency)
    temp_state <- remove_duplicate_capitalize_names(temp_state)
    temp_state <- temp_state[!is.na(temp_state$year), ]


    if (type == "leoka") {
      type <- "police"
    }
    setwd(here(paste0("data/", type, "_monthly")))
    agency <- unique(temp_state$agency)
    agency <- jsonlite::toJSON(agency, pretty = FALSE)
    write(agency, paste0(unique(temp_state$state), "_agency_choices.json"))

    temp_state <-
      temp_state %>%
      dplyr::group_split(ORI)
    parallel::mclapply(temp_state, make_csv_test, type = type)

    message(state_group); rm(temp_state); gc();
  }
}



make_agency_csvs <- function(data,
                             type = "year",
                             county = FALSE,
                             estimates = FALSE) {
  if (county) {
    names(data) <- gsub("^county$", "ORI", names(data))
  }
# library(progress)
#   unique_oris <- unique(data$ORI)
#   pb <- progress_bar$new(
#     format = "  processing [:bar] :percent eta: :eta",
#     total = length(unique_oris), clear = FALSE, width= 90)


  data <-
    data %>%
    dplyr::group_split(ORI)

# for (i in 1:length(unique_oris)) {
#   temp <- data %>%
#     filter(ORI %in% unique_oris[i])
#   make_csv_test(temp, type      = type,
#                 county    = county,
#                 estimates = estimates)
#   pb$tick()
#
# }

  parallel::mclapply(data,
                     make_csv_test,
                     type      = type,
                     county    = county,
                     estimates = estimates)

}

make_csv_test <- function(temp, type, county = FALSE, estimates = FALSE) {
  if (county) {
    names(temp) <- gsub("^ORI$", "agency", names(temp))
  } else {
    temp   <- dummy_rows_missing_years(temp, type = type)
  }

  state  <- unique(temp$state)
  agency <- unique(temp$agency)
  state  <- gsub(" ", "_", state)
  agency <- gsub(" |:", "_", agency)
  agency <- gsub("/", "_", agency)
  agency <- gsub("_+", "_", agency)
  agency <- gsub("\\(|\\)", "", agency)

  if (county) {
    names(temp) <- gsub("^agency$", "county", names(temp))
  }

  if (estimates) {
    temp$ORI <- NA
  }

  data.table::fwrite(temp, file = paste0(state, "_", agency, ".csv"))
}

save_monthly_state_temp <- function(data, start_year, type) {
  setwd(here("data/temp"))
  for (state_group in 1:length(states)) {
    selected_states <- states[state_group]
    if (year != start_year) {
      load(paste0("monthly_", type, "_state_group_", state_group, ".rda"))
      temp_state <- dplyr::bind_rows(temp_state,
                                     data[tolower(data$state) %in% selected_states, ])
    } else {
      temp_state <- data[tolower(data$state) %in% selected_states, ]
    }
    save(temp_state, file = paste0("monthly_", type, "_state_group_", state_group, ".rda"))
    rm(temp_state); gc();
  }
}




make_all_na <- function(col) {
  col <- NA
}

dummy_rows_missing_years <- function(data, type) {

  if (type == "year") {
    missing_years <- min(data$year):max(data$year)
  } else {
    missing_years <- seq.Date(lubridate::ymd(min(data$year)),
                              lubridate::ymd(max(data$year)),
                              by = "month")
    missing_years <- as.character(missing_years)
  }

  missing_years <- missing_years[!missing_years %in% data$year]

  if (length(missing_years) > 0) {

    temp <- data
    temp <- temp[1, ]
    temp <- splitstackshape::expandRows(temp,
                                        count = length(missing_years),
                                        count.is.col = FALSE)
    temp$year <- missing_years
    temp <-
      temp %>%
      dplyr::mutate_at(vars(-one_of("year", "agency", "state", "ORI")),
                       make_all_na)

    data <-
      data %>%
      dplyr::bind_rows(temp) %>%
      dplyr::arrange(desc(year)) %>%
      dplyr::mutate(year = as.character(year))
  }


  return(data)
}

na_non_12_month_rows <- function(data) {

  temp <-
    data %>%
    dplyr::filter(!number_of_months_reported %in% 12 & year >= 1972) %>%
    dplyr::mutate_at(vars(-one_of("year",
                                  "agency",
                                  "state",
                                  "ORI",
                                  "population",
                                  "female_employees_officers",
                                  "male_employees_officers",
                                  "total_employees_officers",
                                  "female_employees_civilians",
                                  "male_employees_civilians",
                                  "total_employees_civilians",
                                  "female_employees_total",
                                  "male_employees_total",
                                  "total_employees_total")),
                     make_all_na)

  data <-
    data %>%
    dplyr::filter(number_of_months_reported  == 12 | year < 1972) %>%
    dplyr::bind_rows(temp) %>%
    dplyr::arrange(desc(year))

  return(data)
}

simpleCap <- function(word) {
  word <- tolower(word)
  split_word <- strsplit(word, " ")[[1]]
  split_word <- paste(toupper(substring(split_word, 1,1)),
                      substring(split_word, 2),
                      sep = "",
                      collapse = " ")
  split_word <- gsub(" Of ", " of ", split_word)
}

save_state_data <- function(data, save_type) {
  for (selected_state in sort(unique(data$state))) {
    temp <-
      data %>%
      dplyr::filter(state %in% selected_state)

    save_state <- unique(temp$state)
    save_state <- gsub(" ", "_", save_state)

    readr::write_csv(temp,
                     path = paste0(save_state, "_", save_type, ".csv"))
  }
}

make_numeric <- function(x) {
  x <- suppressWarnings(readr::parse_number(x))
  return(x)
}

starting_cols <- c("agency",
                   "year",
                   "state",
                   "population",
                   "ORI")

ucr_to_drop <- c("state_abb",
                 "ORI9",
                 "FIPS_state_code",
                 "FIPS_county_code",
                 "months_reported",
                 "fips_state_county_code",
                 "fips_place_code",
                 "fips_state_place_code",
                 "division",
                 "core_city_indication",
                 "covered_by_code",
                 "population_1",
                 "county_1",
                 "msa_1",
                 "population_2",
                 "county_2",
                 "msa_2",
                 "population_3",
                 "county_3",
                 "msa_3",
                 "followup_indication",
                 "special_mailing_group",
                 "special_mailing_address",
                 "mailing_address_line_1",
                 "mailing_address_line_2",
                 "mailing_address_line_3",
                 "mailing_address_line_4",
                 "agency_type",
                 "agency_subtype_1",
                 "agency_subtype_2",
                 "group_number",
                 "agency_state_name",
                 "agency_name",
                 "field_office",
                 "total_population",
                 "city_sequence_number",
                 "last_update",
                 "field_office",
                 "zip_code")

arrests_to_drop <- c("ori9",
                     "agency_name",
                     "state_abb",
                     "fips_state_code",
                     "fips_county_code",
                     "fips_state_county_code",
                     "fips_place_code",
                     "fips_state_place_code",
                     "agency_type",
                     "agency_subtype_1",
                     "agency_subtype_2",
                     "group",
                     "geographic_division",
                     "suburban_agency",
                     "core_city",
                     "covered_by_another_agency")

