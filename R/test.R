data <- hate_crimes[hate_crimes$ORI %in% "AK0010100", ]


add_missing_months_for_years_available <- function(data) {

    original_years <- unique(lubridate::year(data$year))

    data$year <- as.character(data$year)
    data <- dummy_rows_missing_years(data, type = "month") %>%
      dplyr::mutate(data_year = ymd(year),
                    data_year = lubridate::year(data_year)) %>%
      dplyr::filter(data_year %in% original_years) %>%
      dplyr::arrange(desc(year))

    data <-
      data %>%
      dplyr::mutate_at(vars(-one_of("year",
                                    "agency",
                                    "state",
                                    "ORI",
                                    "population",
                                    "data_year")),
                       make_all_0)


    for (selected_year in original_years) {
      population_val <- unique(data$population[data$data_year %in% selected_year])
      population_val <- population_val[!is.na(population_val)]
      data$population[data$data_year %in% selected_year] <- population_val
    }
    data$data_year <- NULL
    data$year <- ymd(data$year)

    return(data)

}


z <-
  hate_crimes %>%
  dplyr::select(year,
                matches("total")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarize_all(sum)
z <- as.data.frame(z)
head(z)
cbind(z$year, z$anti_arab_total)


cbind(z$year, z$anti_transgender_total)

cbind(z$year, z$anti_hispanic_total)
cbind(z$year, z$anti_not_hispanic_total)

cbind(z$year, z$anti_male_homosexual_gay_total)
cbind(z$year, z$anti_female_homosexual_lesbian_total)
cbind(z$year, z$anti_transgender_total)
cbind(z$year, z$anti_bisexual_total)
cbind(z$year, z$anti_gender_non_conforming_total)
cbind(z$year, z$anti_homosexual_gay_and_lesbian_total)

cbind(z$year, z$anti_lesbian_gay_bisexual_or_transgender_mixed_group_lgbt_total)


cbind(z$year,
      z$anti_male_homosexual_gay_total,
      z$anti_female_homosexual_lesbian_total,
      z$anti_homosexual_gay_and_lesbian_total,
      z$anti_lesbian_gay_bisexual_or_transgender_mixed_group_lgbt_total)
