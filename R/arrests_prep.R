
all_arrest_codes <- c("aggravated assault",
                      "all other offenses excluding traffic",
                      "arson",
                      "burglary",
                      "curfew loitering",
                      "disorderly conduct",
                      "drug possess - marijuana",
                      "drug possess - opium and cocaine and derivatives including heroin",
                      "drug possess - other drug",
                      "drug possess - synthetic - narcotics",
                      "drug sale - marijuana",
                      "drug sale - opium and cocaine and derivatives including heroin",
                      "drug sale - other drug",
                      "drug sale - synthetic narcotics",
                      "drunkenness",
                      "dui",
                      "embezzlement",
                      "family offenses",
                      "forgery and counterfeiting",
                      "fraud",
                      "gambling - total",
                      "human trafficking - commercial sex acts",
                      "human trafficking -involuntary servitude",
                      "liquor laws",
                      "motor vehicle theft",
                      "murder and nonnegligent manslaughter",
                      "negligent manslaughter",
                      "other assault",
                      "other sex offenses",
                      "prostitution and commercialized vice",
                      "rape",
                      "robbery",
                      "runaways",
                      "stolen property buying receiving possessing",
                      "suspicion",
                      "theft",
                      "vagrancy",
                      "vandalism",
                      "weapons carrying possessing etc")

get_arrest_data <- function(type, crosswalk_data) {
  California_Los_Angeles_Police_Department <- read_csv("California_Los_Angeles_Police_Department.csv")
  if (type %in% "year") {
    files <- list.files(path = "D:/ucr_data_storage/clean_data/arrests/", pattern = "year.*rds$", full.names = TRUE)
    arrests <- vector("list", length = length(files))

    for (i in 1:length(files)) {
      temp <- readRDS(files[i]) %>%
        filter(number_of_months_reported %in% 12) %>%
        select(ori,
               year,
               state,
               population,
               offense_code,
               adult_american_indian,
               adult_asian,
               adult_black,
               adult_white,
               adult_hispanic,
               adult_non_hispanic,
               juvenile_american_indian,
               juvenile_asian,
               juvenile_black,
               juvenile_white,
               juvenile_hispanic,
               juvenile_non_hispanic,
               total_male_juvenile,
               total_male_adult,
               total_female_juvenile,
               total_female_adult,
               total_male,
               total_female,
               total_arrests,
               total_american_indian,
               total_asian,
               total_black,
               total_white,
               total_hispanic,
               total_non_hispanic,
               total_juvenile,
               total_adult)

      all_arrests <- temp %>%
        filter(offense_code %in% all_arrest_codes) %>%
        select(-offense_code) %>%
        group_by(year,
                 state,
                 population,
                 ori) %>%
        summarize_all(sum) %>%
        ungroup() %>%
        mutate(offense_code = "all_arrests_total")

      temp <-
        temp %>%
        bind_rows(all_arrests) %>%
        pivot_longer(
          cols = -c(offense_code, ori, year, state, population),
          names_to = "old_name",
          values_to = "value"
        ) %>%
        mutate(new_name = paste0(offense_code, "_", old_name)) %>%
        select(-offense_code, -old_name) %>%
        pivot_wider(names_from = new_name, values_from = value) %>%
        rename_all(make_clean_names)

      arrests[[i]] <- temp
      message(files[i])
    }
    arrests <- data.table::rbindlist(arrests, fill = TRUE) %>%
      as.data.frame()
    gc()
  } else {
    files <- list.files(path = "D:/ucr_data_storage/clean_data/arrests/", pattern = "month.*rds$", full.names = TRUE)
    arrests <- vector("list", length = length(files))

    for (i in 1:length(files)) {
      temp <- readRDS(files[i]) %>%
        filter(number_of_months_reported %in% 12) %>%
        mutate(year = ymd(paste0(year, "-", month, "-01"))) %>%
        select(ori,
               year,
               state,
               population,
               offense_code,
               adult_american_indian,
               adult_asian,
               adult_black,
               adult_white,
               adult_hispanic,
               adult_non_hispanic,
               juvenile_american_indian,
               juvenile_asian,
               juvenile_black,
               juvenile_white,
               juvenile_hispanic,
               juvenile_non_hispanic,
               total_male_juvenile,
               total_male_adult,
               total_female_juvenile,
               total_female_adult,
               total_male,
               total_female,
               total_arrests,
               total_american_indian,
               total_asian,
               total_black,
               total_white,
               total_hispanic,
               total_non_hispanic,
               total_juvenile,
               total_adult)

      all_arrests <- temp %>%
        filter(offense_code %in% all_arrest_codes) %>%
        select(-offense_code) %>%
        group_by(year,
                 state,
                 population,
                 ori) %>%
        summarize_all(sum) %>%
        ungroup() %>%
        mutate(offense_code = "all_arrests_total")


      temp <-
        temp %>%
        bind_rows(all_arrests) %>%
        pivot_longer(
          cols = -c(offense_code, ori, year, state, population),
          names_to = "old_name",
          values_to = "value"
        ) %>%
        mutate(new_name = paste0(offense_code, "_", old_name)) %>%
        select(-offense_code, -old_name) %>%
        pivot_wider(names_from = new_name, values_from = value) %>%
        rename_all(make_clean_names)

      arrests[[i]] <- temp
      message(files[i])


    }
    arrests <- data.table::rbindlist(arrests, fill = TRUE) %>%
      as.data.frame()
    gc()
  }

  arrests <-
    arrests %>%
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
  gc()

  arrests$agency <- gsub("\\(|\\)", "", arrests$agency)
  arrests <- remove_duplicate_capitalize_names(arrests)

  # Reorder columns
  arrests <-
    arrests %>%
    select(names(California_Los_Angeles_Police_Department))
  gc(); Sys.sleep(1); gc()

  if (type %in% "year") {
    setwd(here("data/arrests"))
    make_agency_csvs(arrests)
    make_largest_agency_json(arrests)
    make_state_agency_choices(arrests)
  } else {
    arrests$year <- as.character(arrests$year)
    setwd(here("data/arrests_monthly"))
    make_agency_csvs(arrests, type = "month")

    setwd(here("data/arrests"))
    files <- list.files(pattern = "agency_choices")
    files
    file.copy(files, paste0(here::here("data/arrests_monthly/")), overwrite = TRUE)
  }

}
