load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/offenses_known/ucr_offenses_known_yearly_1960_2016.rda")
source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/utils.R')

ucr <-
  ucr_offenses_known_yearly_1960_2016 %>%
  dplyr::filter(months_reported == "december is the last month reported",
                !state %in% c("guam",
                              "canal zone",
                              "puerto rico",
                              "virgin islands")) %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA",
                ori    != "FL01394") %>%
  dplyr::mutate(population = total_population,
                agency = tolower(agency)) %>%
  dplyr::rename(ORI                = ori,
                ORI9               = ori9,
                FIPS_state_code    = fips_state_code,
                FIPS_county_code   = fips_county_code,
                act_all_crimes     = act_all_fields,
                clr_all_crimes     = clr_all_fields,
                clr_18_all_crimes  = clr_18_all_fields,
                unfound_all_crimes = unfound_all_fields) %>%
  dplyr::select(-one_of(ucr_to_drop)) %>%
  dplyr::select(starting_cols,
                dplyr::matches("all_crimes"),

                dplyr::matches("aggravated_assault"),
                dplyr::matches("assault_total"),
                dplyr::matches("gun_assault"),
                dplyr::matches("knife_assault"),
                dplyr::matches("other_weapon_assault"),
                dplyr::matches("simple_assault"),
                dplyr::matches("hand_feet_assault"),

                dplyr::matches("burglary_total"),
                dplyr::matches("attempted_burglary"),
                dplyr::matches("burg_force_entry"),
                dplyr::matches("burg_no_force_entry"),

                dplyr::matches("mtr_vhc_theft_total"),
                dplyr::matches("manslaughter"),
                dplyr::matches("murder"),

                dplyr::matches("officers_assaulted"),
                dplyr::matches("officers_killed_by_accident"),
                dplyr::matches("officers_killed_by_felony"),

                dplyr::matches("rape_total"),
                dplyr::matches("attempted_rape"),
                dplyr::matches("force_rape"),

                dplyr::matches("robbery_total"),
                dplyr::matches("gun_robbery"),
                dplyr::matches("knife_robbery"),
                dplyr::matches("other_weapon_robbery"),
                dplyr::matches("strong_arm_robbery"),

                dplyr::matches("theft_total"),
                dplyr::matches("auto_theft"),
                dplyr::matches("other_vhc_theft"),
                dplyr::matches("truck_bus_theft"))
rm(ucr_offenses_known_yearly_1960_2016); gc()

z = ucr[!duplicated(ucr$ORI),]
z$temp <- paste(z$agency, z$state)
z = z[duplicated(z$temp),]
ucr <- ucr[!ucr$ORI %in% z$ORI, ]
ucr$agency <- sapply(ucr$agency, simpleCap)
ucr$state <- sapply(ucr$state, simpleCap)
ucr$state <- gsub(" Of ", " of ", ucr$state)


ucr <- data.table::data.table(ucr)
setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/offenses")
for (selected_ori in unique(ucr$ORI)) {
  temp   <- ucr[ORI %in% selected_ori]
  state  <- unique(temp$state)
  agency <- unique(temp$agency)
  state  <- gsub(" ", "_", state)
  agency <- gsub(" |:", "_", agency)
  agency <- gsub("/", "_", agency)
  agency <- gsub("_+", "_", agency)
  readr::write_csv(temp,
                   path = paste0(state, "_", agency, ".csv"))
}

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/offenses")
for (selected_state in unique(ucr$state)) {
  temp   <- ucr[state %in% selected_state]
  agency <- unique(temp$agency)
  agency <- jsonlite::toJSON(agency, pretty = FALSE)
  write(agency, paste0(selected_state, "_agency_choices.json"))
}

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/offenses")
largest_agency <- ucr %>%
  dplyr::group_by(state) %>%
  dplyr::top_n(1, population) %>%
  dplyr::select(state, agency)
largest_agency <- jsonlite::toJSON(largest_agency, pretty = TRUE)
write(largest_agency, "largest_agency_choices.json")
rm(ucr); gc()

