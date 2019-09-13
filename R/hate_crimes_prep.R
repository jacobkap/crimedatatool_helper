load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/hate_crimes/ucr_hate_crimes_1991_2017.rda")
source(here::here('R/utils.R'))

#type = "year"
type = "month"

# Fewer than 0.005% of UCR offenses (10 from 1992-2017) are NA
hate_crimes <-
  ucr_hate_crimes_1991_2017 %>%
  dplyr::filter(hate_crime_incident_present_flag %in%
                  "one or more hate crime incidents present",
                !is.na(ucr_offense_code_1)) %>%
  dplyr::rename(date = incident_date) %>%
  dplyr::select(ori9,
                population,
                state,
                year,
                crosswalk_agency_name,
                date,
                starts_with("ucr_offense_code"),
                starts_with("bias_motivation"),
                -matches("_2|_3|_4|_5|_6|_7|_8|_9|_10")) %>%
  dplyr::rename(agency = crosswalk_agency_name,
                ORI = ori9)
rm(ucr_hate_crimes_1991_2017); gc()
# 96.63% of cases have only 1 offenses/bias motivations
# 99.78% of cases have only 1 or two offenses/bias motivations

hate_crimes <-
  hate_crimes %>%
  dplyr::rename(offense = ucr_offense_code_1,
                bias_motivation = bias_motivation_offense_1) %>%
  dplyr::select(-matches("_[0-9]")) %>%
  dplyr::filter(!is.na(offense),
                agency != "NANA") %>%
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
  dplyr::mutate(year_month = floor_date(ymd(date), "month"))

if (type == "month") {
  setwd(here::here("data/hate_crimes_monthly"))
  hate_crimes <-
    hate_crimes %>%
    dplyr::select(-year) %>%
    dplyr::rename(year = year_month)
} else {
  setwd(here::here("data/hate_crimes"))
  hate_crimes$year_month <- NULL
}

hate_crimes <-
  hate_crimes %>%
  dplyr::select(-offense,
                -bias_motivation,
                -date) %>%
  dplyr::group_by(ORI,
                  state,
                  agency,
                  population,
                  year) %>%
  dplyr::summarize_all(sum) %>%
  dplyr::ungroup()


names(hate_crimes) <- gsub("\\.|\\(|\\)| |-|,|/", "_", names(hate_crimes))
names(hate_crimes) <- gsub("_+", "_", names(hate_crimes))
names(hate_crimes) <- gsub("_$", "", names(hate_crimes))

# Make totals columns
bias_groups <- grep("^anti", names(hate_crimes), value = TRUE)
bias_groups <- gsub("_(non)?violent|_unknown$|_sexual$", "", bias_groups)
bias_groups <- unique(bias_groups)

for (group in bias_groups) {
  hate_crimes[, paste0(group, "_total")] <-
    rowSums(hate_crimes[, grep(paste0(group, "_(non|violent|sexual)"),
                               names(hate_crimes))])
}


for (crime_type in c("_nonviolent", "_violent", "_sexual")) {
  hate_crimes[, paste0("anti_total", crime_type)] <-
    rowSums(hate_crimes[, grep(crime_type, names(hate_crimes))])
}
hate_crimes$anti_total_total <-
  rowSums(hate_crimes[, grep("anti_total_", names(hate_crimes))])

# Reorder columns alphabetically
anti_columns <- grep("anti", names(hate_crimes), value = TRUE)
anti_columns <- sort(anti_columns)


hate_crimes <-
  hate_crimes %>%
  dplyr::select(starting_cols,
                ends_with("_violent"),
                ends_with("_nonviolent"),
                ends_with("_total"))
hate_crimes <- remove_duplicate_capitalize_names(hate_crimes)
hate_crimes$agency <- gsub(":", "", hate_crimes$agency)
hate_crimes$state  <- gsub("Washington D.C.",
                          "District of Columbia",
                          hate_crimes$state)
hate_crimes$year <- as.character(hate_crimes$year)


make_state_agency_choices(hate_crimes)
make_largest_agency_json(hate_crimes)

make_agency_csvs(hate_crimes, type = type)
