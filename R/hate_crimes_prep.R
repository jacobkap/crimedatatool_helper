load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/hate_crimes/ucr_hate_crimes_1992_2017.rda")

hate_crimes <-
  ucr_hate_crimes_1992_2017 %>%
  dplyr::filter(hate_crime_incident_present_flag %in%
                  "one or more hate crime incidents present") %>%
  dplyr::select(ori9,
                population,
                state,
                year,
                agency_name,
                date,
                starts_with("ucr_offense_code"),
                starts_with("bias_motivation"),
                -matches("_2|_3|_4|_5|_6|_7|_8|_9|_10")) %>%
  dplyr::rename(agency = agency_name,
                ORI = ori9)

# 96.63% of cases have only 1 offenses/bias motivations
# 99.78% of cases have only 1 or two offenses/bias motivations

hate_crimes <-
  hate_crimes %>%
  dplyr::rename(offense = ucr_offense_code_1,
                bias_motivation = bias_motivation_offense_1) %>%
  dplyr::select(-matches("_[0-9]")) %>%
  dplyr::filter(!is.na(offense),
                offense != "23*",
                offense != "26g") %>%
  dplyr::mutate(offense = stringr::str_replace_all(offense,
                                                   hate_crimes_offenses_fix)) %>%
  fastDummies::dummy_cols(select_columns = c("offense",
                                             "bias_motivation"))

for (bias_value in unique(hate_crimes$bias_motivation)) {
  for (offense_value in unique(hate_crimes$offense)) {
    hate_crimes[, paste0(bias_value, "_", offense_value)] <-
      hate_crimes[, paste0("bias_motivation_", bias_value)] *
      hate_crimes[, paste0("offense_", offense_value)]

  }
}

hate_crimes <-
  hate_crimes %>%
  dplyr::select(-matches("^offense_|^bias_motivation_")) %>%
  dplyr::mutate(year_month = floor_date(ymd(date), "month")) %>%
  dplyr::select(-offense,
                -bias_motivation,
                -date) %>%
  dplyr::group_by(ORI,
                  state,
                  agency,
                  population,
                  year,
                  year_month) %>%
  dplyr::summarize_all(sum) %>%
  dplyr::ungroup()
hate_crimes <- data.frame(hate_crimes)
hate_crimes[1:5, 1:10]

names(hate_crimes) <- gsub("\\.", "_", names(hate_crimes))
names(hate_crimes) <- gsub("_+", "_", names(hate_crimes))
names(hate_crimes) <- gsub("_$", "", names(hate_crimes))

# Reorder columns alphabetically
anti_columns <- grep("anti", names(hate_crimes), value = TRUE)
anti_columns <- sort(anti_columns)

hate_crimes <-
  hate_crimes %>%
  dplyr::select(ORI,
                state,
                agency,
                year,
                year_month,
                anti_columns)
hate_crimes <- remove_duplicate_capitalize_names(hate_crimes)


setwd(here::here("data/hate_crimes"))
make_state_agency_choices(hate_crimes)
make_agency_csvs(hate_crimes)
