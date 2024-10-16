get_arrest_data <- function(type, crosswalk_data) {

  if (type %in% "year") {
    arrests <- readRDS("F:/ucr_data_storage/clean_data/combined_years/srs/arrests_yearly_all_crimes_race_sex_1974_2023.rds") %>%
      filter(number_of_months_reported %in% 12)
  } else {
    arrests <- vector("list", length = 6)
    arrests_latest <- readRDS("F:/ucr_data_storage/clean_data/combined_years/srs/arrests_monthly_all_crimes_race_sex_2020_2023.rds") %>%
      filter(number_of_months_reported %in% 12) %>%
      select(-msa,
             -county,
             -date_of_1st_previous_update)
    arrests[[1]] <- arrests_latest
    rm(arrests_latest); gc(); Sys.sleep(1)

     arrests_2010_2019 <- readRDS("F:/ucr_data_storage/clean_data/combined_years/srs/arrests_monthly_all_crimes_race_sex_2010_2019.rds") %>%
                  filter(number_of_months_reported %in% 12) %>%
                  select(-msa,
                         -county,
                         -date_of_1st_previous_update)
     arrests[[2]] <- arrests_2010_2019
     rm(arrests_2010_2019); gc(); Sys.sleep(1)

     arrests_2000_2009 <- readRDS("F:/ucr_data_storage/clean_data/combined_years/srs/arrests_monthly_all_crimes_race_sex_2000_2009.rds") %>%
                  filter(number_of_months_reported %in% 12) %>%
                  select(-msa,
                         -county,
                         -date_of_1st_previous_update)
     arrests[[3]] <- arrests_2000_2009
     rm(arrests_2000_2009); gc(); Sys.sleep(1)

      arrests_1990_1999 <- readRDS("F:/ucr_data_storage/clean_data/combined_years/srs/arrests_monthly_all_crimes_race_sex_1990_1999.rds") %>%
                  filter(number_of_months_reported %in% 12) %>%
                  select(-msa,
                         -county,
                         -date_of_1st_previous_update)
      arrests[[4]] <- arrests_1990_1999
      rm(arrests_1990_1999); gc(); Sys.sleep(1)

      arrests_1980_1989 <- readRDS("F:/ucr_data_storage/clean_data/combined_years/srs/arrests_monthly_all_crimes_race_sex_1980_1989.rds") %>%
                  filter(number_of_months_reported %in% 12) %>%
                  select(-msa,
                         -county,
                         -date_of_1st_previous_update)
      arrests[[5]] <- arrests_1980_1989
      rm(arrests_1980_1989); gc(); Sys.sleep(1)

      arrests_1974_1979 <- readRDS("F:/ucr_data_storage/clean_data/combined_years/srs/arrests_monthly_all_crimes_race_sex_1974_1979.rds") %>%
                  filter(number_of_months_reported %in% 12) %>%
                  select(-msa,
                         -county,
                         -date_of_1st_previous_update)
      arrests[[6]] <- arrests_1974_1979
      rm(arrests_1974_1979); gc(); Sys.sleep(1)

      arrests <- data.table::rbindlist(arrests, fill = TRUE) %>%
        as.data.frame() %>%
      mutate(year = date)
      gc(); Sys.sleep(1)
  }

  arrests <-
    arrests  %>%
    fix_missing_states() %>%
    fix_ori() %>%
    dplyr::filter(!state %in% c("guam",
                                "canal zone",
                                "puerto rico",
                                "virgin islands")) %>%
    dplyr::left_join(crosswalk_agencies, by = "ori") %>%
    dplyr::filter(agency != "NANA",
                  state != "98")
  gc()


  table(is.na(arrests$agency))
  sort(unique(arrests$state), na.last = TRUE)

  unique_offenses <- grep("total_white", names(arrests), value = TRUE)
  unique_offenses <- gsub("_total_white", "", unique_offenses)
  unique_offenses <- sort(unique_offenses)

  all_cols <- c()
  for (col in unique_offenses) {
    col_values <- grep(col, names(arrests), value = TRUE)
    col_values_order <-
      c(grep("total_adult", col_values, value = TRUE),
        grep("total_juv", col_values, value = TRUE),
        grep("total_arrest", col_values, value = TRUE),
        grep("total_(fe)?male_adult", col_values, value = TRUE),
        grep("total_(fe)?male_juv$", col_values, value = TRUE),
        grep("total_(fe)?male", col_values, value = TRUE),
        grep("adult_(asian|amer|black|white)", col_values, value = TRUE),
        grep("juvenile_(asian|amer|black|white)", col_values, value = TRUE),
        grep("total_(asian|amer|black|white)", col_values, value = TRUE),
        grep("adult_hispanic", col_values, value = TRUE),
        grep("adult_non_hispanic", col_values, value = TRUE),
        grep("juvenile_hispanic", col_values, value = TRUE),
        grep("juvenile_non_hispanic", col_values, value = TRUE),
        grep("hispanic", col_values, value = TRUE),
        grep("total_non_hispanic", col_values, value = TRUE))
    col_values <- col_values[!col_values %in% col_values_order]
    col_values_order <- c(col_values_order,
                          col_values)

    all_cols <- c(all_cols,
                  col_values_order)
  }


  arrests <-
    arrests %>%
    dplyr::rename(ORI = ori) %>%
    dplyr::select(agency,
                  ORI,
                  year,
                  state,
                  population,
                  all_cols)

  arrest_categories <- grep("robbery", names(arrests), value = TRUE)
  arrest_categories <- gsub("robbery_", "", arrest_categories)

  arrests <- data.frame(arrests)
  for (arrest_category in arrest_categories) {
    arrests[, paste0("all_arrests_total_", arrest_category)] <-
      rowSums(arrests[, paste0(unique_offenses, "_", arrest_category)], na.rm = TRUE)
  }


  arrests$agency <- gsub("\\(|\\)", "", arrests$agency)
  arrests <- remove_duplicate_capitalize_names(arrests)
  gc(); Sys.sleep(1); gc()

  if (type %in% "year") {
    setwd(here("data/arrests"))
    make_agency_csvs(arrests)
    make_largest_agency_json(arrests)
    make_state_agency_choices(arrests)
  } else {
    setwd(here("data/arrests_monthly"))
    make_agency_csvs(arrests, type = "month")

    setwd(here("data/arrests"))
    files <- list.files(pattern = "agency_choices")
    files
    file.copy(files, paste0(here::here("data/arrests_monthly/")), overwrite = TRUE)
  }

}