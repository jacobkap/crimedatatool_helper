load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/hate_crimes/ucr_hate_crimes_1992_2017.rda")

hate_crimes <-
  ucr_hate_crimes_1992_2017 %>%
  dplyr::filter(hate_crime_incident_present_flag %in%
                  "one or more hate crime incidents present") %>%
  dplyr::select(ori9,
                state,
                year,
                starts_with("ucr_offense_code"),
                starts_with("bias_motivation"),
                -matches("_6|_7|_8|_9|_10"))

offense_code_2 <- get_past_1_offenses(hate_crimes, 2)
offense_code_3 <- get_past_1_offenses(hate_crimes, 3)
offense_code_4 <- get_past_1_offenses(hate_crimes, 4)
offense_code_5 <- get_past_1_offenses(hate_crimes, 5)

hate_crimes <-
  hate_crimes %>%
  dplyr::rename(offense = ucr_offense_code_1,
                bias_motivation = bias_motivation_offense_1) %>%
  dplyr::select(-matches("_[0-9]")) %>%
  dplyr::bind_rows(offense_code_2) %>%
  dplyr::bind_rows(offense_code_3) %>%
  dplyr::bind_rows(offense_code_4) %>%
  dplyr::bind_rows(offense_code_5) %>%
  dplyr::filter(!is.na(offense),
                offense != "23*",
                offense != "26g") %>%
  fastDummies::dummy_cols(select_columns = c("offense",
                                             "bias_motivation"))

for (offense_value in unique(hate_crimes$offense)) {
  for (bias_value in unique(hate_crimes$bias_motivation)) {
    hate_crimes[, paste0(offense_value, "_", bias_value)] <-
      hate_crimes[, paste0("offense_", offense_value)] *
      hate_crimes[, paste0("bias_motivation_", bias_value)]

  }
}

hate_crimes <-
  hate_crimes %>%
  dplyr::select(-matches("^offense_|^bias_motivation_")) %>%
  dplyr::select(-offense,
                -bias_motivation) %>%
  dplyr::group_by(ori9,
                  state,
                  year) %>%
  dplyr::summarize_all(sum) %>%
  dplyr::ungroup()
#
# total_offenses <-
#   hate_crimes %>%
#   dplyr::select(-bias_motivation) %>%
#   dplyr::group_by(ori9,
#                   state,
#                   year,
#                   offense) %>%
#   dplyr::count() %>%
#   dplyr::ungroup() %>%
#   dplyr::rename(number_of_hate_crimes = n) %>%
#   dplyr::mutate(bias_motivation = "total")
#
#
# total_bias_motivations <-
#   hate_crimes %>%
#   dplyr::select(-offense) %>%
#   dplyr::group_by(ori9,
#                   state,
#                   year,
#                   bias_motivation) %>%
#   dplyr::count() %>%
#   dplyr::ungroup() %>%
#   dplyr::rename(number_of_hate_crimes = n) %>%
#   dplyr::mutate(offense = "total")
#
# total_total <-
#   hate_crimes %>%
#   dplyr::select(-offense,
#                 -bias_motivation) %>%
#   dplyr::group_by(ori9,
#                   state,
#                   year) %>%
#   dplyr::count() %>%
#   dplyr::ungroup() %>%
#   dplyr::rename(number_of_hate_crimes = n) %>%
#   dplyr::mutate(offense = "total",
#                 bias_motivation = "total")
#
#
# hate_crimes <-
#   hate_crimes %>%
#   dplyr::group_by(ori9,
#                   state,
#                   year,
#                   offense,
#                   bias_motivation) %>%
#   dplyr::count() %>%
#   dplyr::ungroup() %>%
#   dplyr::rename(number_of_hate_crimes = n) %>%
#   dplyr::bind_rows(total_offenses) %>%
#   dplyr::bind_rows(total_bias_motivations) %>%
#   dplyr::bind_rows(total_total) %>%
#   dplyr::filter(year == 2000)
#
#
# hate_crimes <- as.data.frame(hate_crimes)
# head(hate_crimes)
#
#
# z <- data.table::dcast(hate_crimes, ori9 + state + year ~ offense + bias_motivation,
#                        value.var = "number_of_hate_crimes")
# head(z)
#
# get_past_1_offenses <- function(data, number) {
#   data <- data[, c("ori9",
#                    "state",
#                    "year",
#                    paste0("ucr_offense_code_", number),
#                    paste0("bias_motivation_offense_", number))]
#   names(data)[4] <- "offense"
#   names(data)[5] <- "bias_motivation"
#   data <- data[!is.na(data$offense), ]
#   return(data)
# }
