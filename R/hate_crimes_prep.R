

get_hate_crimes_data <- function(type) {
  hate_crimes <- readRDS("F:/ucr_data_storage/clean_data/hate_crimes/ucr_hate_crimes_1991_2023.rds") %>%
    fix_missing_states() %>%
    fix_ori() %>%
    dplyr::filter(
      hate_crime_incident_present %in%
        "one or more hate crime incidents present",
      !is.na(ucr_offense_code_1),
      !ucr_offense_code_1 %in% "undocumented code",
      !bias_motivation_offense_1 %in% "undocumented code"
    ) %>%
    dplyr::rename(date = incident_date) %>%
    dplyr::select(
      ori9,
      population,
      state,
      year,
      crosswalk_agency_name,
      date,
      starts_with("ucr_offense_code"),
      starts_with("bias_motivation"),
      -matches("_2|_3|_4|_5|_6|_7|_8|_9|_10")
    ) %>%
    dplyr::rename(
      agency = crosswalk_agency_name,
      ORI = ori9,
      offense = ucr_offense_code_1,
      bias_motivation = bias_motivation_offense_1
    ) %>%
    dplyr::mutate(offense = stringr::str_replace_all(
      offense,
      hate_crimes_offenses_fix
    )) %>%
    fastDummies::dummy_cols(select_columns = c(
      "offense",
      "bias_motivation"
    ))
  hate_crimes$`bias_motivation_anti-total` <- 1
  hate_crimes$offense_total <- 1

  for (bias_value in c(unique(hate_crimes$bias_motivation),
                       "anti-total")) {
    for (offense_value in c(unique(hate_crimes$offense),
                            "total")) {
      hate_crimes[, paste0(bias_value, "_", offense_value)] <-
        hate_crimes[, paste0("bias_motivation_", bias_value)] *
        hate_crimes[, paste0("offense_", offense_value)]
    }
  }

  hate_crimes <-
    hate_crimes %>%
    dplyr::select(-matches("^offense_|^bias_motivation_")) %>%
    dplyr::mutate(year_month = floor_date(ymd(date), "month"))

  if (type == "month") {
    setwd(here::here("data/hate_monthly"))
    hate_crimes <-
      hate_crimes %>%
      dplyr::select(-year) %>%
      dplyr::rename(year = year_month)
  } else {
    setwd(here::here("data/hate"))
    hate_crimes$year_month <- NULL
  }

  hate_crimes <-
    hate_crimes %>%
    dplyr::select(
      -offense,
      -bias_motivation,
      -date
    ) %>%
    dplyr::group_by(
      ORI,
      state,
      agency,
      population,
      year
    ) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::ungroup()

  names(hate_crimes)
  names(hate_crimes) <- gsub("\\.|\\(|\\)| |-|,|/", "_", names(hate_crimes))
  names(hate_crimes) <- gsub("_+", "_", names(hate_crimes))
  names(hate_crimes) <- gsub("_$", "", names(hate_crimes))

  # Reorder columns alphabetically
  crime_columns <- grep("anti-transgender", names(hate_crimes), value = TRUE)
  crime_columns <- gsub("anti-transgender", "", crime_columns)
  crime_columns <- sort(crime_columns)
  crime_columns <- crime_columns[crime_columns != "_total"]
  crime_columns <- c(crime_columns, "_total")
  anti_columns_small <- grep("_arson", names(hate_crimes), value = TRUE)
  anti_columns_small <- gsub("_arson", "", anti_columns_small)
  anti_columns_small <- sort(anti_columns_small)

  col_order <- c()
  for (i in 1:length(anti_columns_small)) {
    col_order <- c(col_order,
                   paste0(anti_columns_small[i], crime_columns))
  }


  hate_crimes <-
    hate_crimes %>%
    dplyr::select(
      all_of(starting_cols),
      col_order,
      everything()
    ) %>%
    arrange(
      ORI,
      desc(year)
    )
  names(hate_crimes)

  hate_crimes$agency <- gsub("\\(|\\)", "", hate_crimes$agency)
  hate_crimes <- remove_duplicate_capitalize_names(hate_crimes)
  hate_crimes$agency <- gsub(":", "", hate_crimes$agency)
  hate_crimes$state <- gsub(
    "Washington D.C.",
    "District of Columbia",
    hate_crimes$state
  )
  hate_crimes$year <- as.character(hate_crimes$year)
  hate_crimes <-
    hate_crimes %>%
    filter(!agency %in% "NANA")

  make_state_agency_choices(hate_crimes)
  make_largest_agency_json(hate_crimes)
  if (type == "year") {
    hate_crimes$year <- as.numeric(hate_crimes$year)
  }

  make_agency_csvs(hate_crimes, type = type)
}