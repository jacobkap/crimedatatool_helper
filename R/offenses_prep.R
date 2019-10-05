load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/offenses_known/offenses_known_yearly_1960_2017.rda")
source(here::here('R/utils.R'))

ucr <-
  offenses_known_yearly_1960_2017 %>%
  dplyr::filter(number_of_months_reported %in% 12) %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA",
                ori    != "FL01394") %>%
  dplyr::mutate(agency = tolower(agency)) %>%
  dplyr::rename(ORI    = ori) %>%
  dplyr::select(starting_cols,
                dplyr::matches("act|clr|unfound|officer"))

rm(offenses_known_yearly_1960_2017); gc()

ucr <- remove_duplicate_capitalize_names(ucr)


setwd(here::here("data/offenses"))
make_agency_csvs(ucr)
make_state_agency_choices(ucr)
make_largest_agency_json(ucr)




setwd(here::here("raw_data"))
state_level_data_old <- read_csv("CrimeStatebyState.csv", skip = 9)
state_level_data_new <- read_csv("estimated_crimes_1995_2018.csv")
state_level_data_old_names <- c(
  "murder_and_nonnegligent_manslaughter" = "homicide",
  "legacy_rape_/1"                       = "legacy_rape",
  "revised_rape_/2"                      = "revised_rape",
  "larceny-theft"                        = "larceny",
  "violent_crime_total"                  = "violent_crime",
  "property_crime_total"                 = "property_crime"
)


state_level_data_old$state <- state_level_data_old$Year
state_level_data_old$state <- gsub("Estimated crime in ", "", state_level_data_old$state)
state_level_data_old$state[!grepl("Estimated crime in ", state_level_data_old$Year)] <- NA
state_level_data_old$state[1] <- "Alabama"
state_level_data_old$state <- zoo::na.locf(state_level_data_old$state)
state_level_data_old <- state_level_data_old[!is.na(state_level_data_old$Year), ]

state_level_data_old <- state_level_data_old[!is.na(state_level_data_old$Population), ]
state_level_data_old <- state_level_data_old[!is.na(state_level_data_old$Robbery), ]
state_level_data_old <- state_level_data_old[!state_level_data_old$Year == "Year", ]


names(state_level_data_old) <- tolower(names(state_level_data_old))
names(state_level_data_old) <- gsub("\\s", "_", names(state_level_data_old))
names(state_level_data_old) <- stringr::str_replace_all(names(state_level_data_old), state_level_data_old_names)
state_level_data_old$x13 <- NULL
state_level_data_old$state[state_level_data_old$state == "United States-Total"] <-
  "United States"
state_level_data_old$agency <- paste(state_level_data_old$state, "State-Level Estimate")
state_level_data_old$agency[state_level_data_old$state == "United States"] <-
  "United States - National Estimate"
names(state_level_data_old) <- gsub("legacy_rape", "rape", names(state_level_data_old))
state_level_data_old$rape[!is.na(state_level_data_old$revised_rape)] <-
  state_level_data_old$revised_rape[!is.na(state_level_data_old$revised_rape)]




state_level_data_new$state <- state.name[match(state_level_data_new$state_abbr,state.abb)]
state_level_data_new$agency <- paste(state_level_data_new$state, "State-Level Estimate")
state_level_data_new$agency[is.na(state_level_data_new$state)] <- "United States - National Estimate"
state_level_data_new$state[is.na(state_level_data_new$state)] <- "United States"
names(state_level_data_new) <- gsub("rape_legacy", "rape", names(state_level_data_new))
state_level_data_new$rape[!is.na(state_level_data_new$rape_revised)] <-
  state_level_data_new$rape_revised[!is.na(state_level_data_new$rape_revised)]
state_level_data_new <- state_level_data_new[state_level_data_new$year >= 2015, ]
state_level_data_new[] <- sapply(state_level_data_new, as.character)

state_level_data <-
  state_level_data_old %>%
  dplyr::bind_rows(state_level_data_new) %>%
  dplyr::select(-rape_revised,
                -state_abbr,
                -caveats,
                -revised_rape) %>%
  dplyr::mutate(ORI = agency) %>%
  dplyr::rename(actual_rape_total          = rape,
                actual_robbery_total       = robbery,
                actual_assault_aggravated  = aggravated_assault,
                actual_burg_total          = burglary,
                actual_theft_total         = larceny,
                actual_mtr_veh_theft_total = motor_vehicle_theft,
                actual_index_violent       = violent_crime,
                actual_murder              = homicide,
                actual_index_property      = property_crime,
                actual_theft_total         = larceny) %>%
  dplyr::mutate(actual_index_total         = actual_index_property + actual_index_violent)
state_level_data[, 1:11] <- sapply(state_level_data[, 1:11], parse_number)

setwd(here::here("data/offenses"))
make_agency_csvs(state_level_data, estimates = TRUE)
ucr <- dplyr::bind_rows(ucr, state_level_data)
make_state_agency_choices(ucr)
