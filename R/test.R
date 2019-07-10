temp <- hate_crimes[hate_crimes$ORI %in% "AK0010100",]

z <- add_missing_months_for_years_available(hate_crimes)


add_missing_months_for_years_available <- function(data) {
  for (ori in unique(data$ORI)) {
    message(ori)
    temp <- data[data$ORI %in% ori, ]
    original_years <- unique(lubridate::year(temp$year))

    temp$year <- as.character(temp$year)
    temp <- dummy_rows_missing_years(temp, type = "month") %>%
      dplyr::mutate(temp_year = ymd(year),
                    temp_year = lubridate::year(temp_year)) %>%
      dplyr::filter(temp_year %in% original_years) %>%
      dplyr::arrange(desc(year))

    temp <-
      temp %>%
      dplyr::mutate_at(vars(-one_of("year",
                                    "agency",
                                    "state",
                                    "ORI",
                                    "population",
                                    "temp_year")),
                       make_all_0)


    for (selected_year in original_years) {
      population_val <- unique(temp$population[temp$temp_year %in% selected_year])
      population_val <- population_val[!is.na(population_val)]
      temp$population[temp$temp_year %in% selected_year] <- population_val
    }
    temp$temp_year <- NULL
    temp$year <- ymd(temp$year)

    data <- data[!data$ORI %in% ori, ]
    data <- dplyr::bind_rows(data, temp)

  }
}



