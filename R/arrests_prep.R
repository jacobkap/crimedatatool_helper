source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/utils.R')
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/arrests/ucr_arrests_yearly_all_crimes_totals_race_1974_2016.rda")
load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/arrests/ucr_arrests_yearly_all_crimes_totals_sex_1974_2016.rda")

ucr_arrests_yearly_all_crimes_totals_sex_1974_2016 <-
  ucr_arrests_yearly_all_crimes_totals_sex_1974_2016 %>%
  dplyr::filter(number_of_months_reported %in% 12)
gc()
common_names <- names(ucr_arrests_yearly_all_crimes_totals_sex_1974_2016)[names(ucr_arrests_yearly_all_crimes_totals_sex_1974_2016) %in% names(ucr_arrests_yearly_all_crimes_totals_race_1974_2016)]
common_names <- common_names[!common_names %in% c("ori", "year")]
ucr_arrests_yearly_all_crimes_totals_race_1974_2016 <-
  ucr_arrests_yearly_all_crimes_totals_race_1974_2016 %>%
  dplyr::filter(number_of_months_reported %in% 12) %>%
  dplyr::select(-one_of(common_names))
gc()
arrests <-
  ucr_arrests_yearly_all_crimes_totals_sex_1974_2016 %>%
  dplyr::left_join(ucr_arrests_yearly_all_crimes_totals_race_1974_2016) %>%
  dplyr::filter(!state %in% c("guam",
                              "canal zone",
                              "puerto rico",
                              "virgin islands")) %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA") %>%
  dplyr::select(-one_of(arrests_to_drop)) %>%
  dplyr::rename(ORI               = ori) %>%
  dplyr::select(starting_cols,
                dplyr::matches("tot_adult"),
                dplyr::matches("tot_juv"),
                dplyr::matches("tot_arrests"),
                dplyr::matches("tot_(fe)?male_adult$"),
                dplyr::matches("tot_(fe)?male_juv$"),
                dplyr::matches("tot_(fe)?male$"),
                dplyr::matches("adult_(asian|amer|black|white)"),
                dplyr::matches("juv_(asian|amer|black|white)"),
                dplyr::matches("tot_(asian|amer|black|white)"),
                everything()) %>%
  dplyr::select(starting_cols,
                dplyr::matches("agg_assault"),
                dplyr::matches("all_other"),
                dplyr::matches("oth_assault"),
                dplyr::matches("arson"),
                dplyr::matches("burglary"),
                dplyr::matches("curfew_loiter"),
                dplyr::matches("disorder_cond"),

                dplyr::matches("total_drug"),
                dplyr::matches("poss_drug_total"),
                dplyr::matches("poss_synth_narc"),
                dplyr::matches("poss_cannabis"),
                dplyr::matches("poss_heroin_coke"),
                dplyr::matches("poss_other_drug"),
                dplyr::matches("sale_drug_total"),
                dplyr::matches("sale_synth_narc"),
                dplyr::matches("sale_cannabis"),
                dplyr::matches("sale_heroin_coke"),
                dplyr::matches("sale_other_drug"),

                dplyr::matches("drunkenness"),
                dplyr::matches("dui"),
                dplyr::matches("embezzlement"),
                dplyr::matches("family_off"),
                dplyr::matches("forgery"),
                dplyr::matches("fraud"),

                dplyr::matches("gamble_total"),
                dplyr::matches("gamble_bookmaker"),
                dplyr::matches("gamble_lottery"),
                dplyr::matches("gamble_other"),

                dplyr::matches("liquor"),
                dplyr::matches("manslaught_neg"),
                dplyr::matches("mtr_veh_theft"),
                dplyr::matches("murder"),
                dplyr::matches("prostitution"),
                dplyr::matches("rape"),
                dplyr::matches("robbery"),
                dplyr::matches("runaways"),
                dplyr::matches("oth_sex_off"),
                dplyr::matches("stolen_prop"),
                dplyr::matches("suspicion"),
                dplyr::matches("theft"),
                dplyr::matches("vagrancy"),
                dplyr::matches("vandalism"),
                dplyr::matches("weapons"))
rm(ucr_arrests_yearly_all_crimes_totals_sex_1974_2016); gc()
rm(ucr_arrests_yearly_all_crimes_totals_race_1974_2016); gc()

arrests <-
  arrests %>%
  dplyr::select(agency,
                ORI,
                year,
                state,
                population,
                dplyr::everything())


z = arrests[!duplicated(arrests$ORI),]
z$temp <- paste(z$agency, z$state)
z = z[duplicated(z$temp),]
arrests <- arrests[!arrests$ORI %in% z$ORI, ]
arrests$agency <- sapply(arrests$agency, simpleCap)
arrests$state  <- sapply(arrests$state, simpleCap)
arrests$state  <- gsub(" Of ", " of ", arrests$state)

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_data/data/arrests")
arrests <- data.table::data.table(arrests)
for (selected_ori in sort(unique(arrests$ORI))) {
  temp   <- arrests[ORI %in% selected_ori]
  state  <- unique(temp$state)
  agency <- unique(temp$agency)
  state  <- gsub(" ", "_", state)
  agency <- gsub(" |:", "_", agency)
  agency <- gsub("/", "_", agency)
  agency <- gsub("_+", "_", agency)

  readr::write_csv(temp,
                   path = paste0(state, "_", agency, ".csv"))
}

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_data/data/arrests")
for (selected_state in unique(arrests$state)) {
  temp   <- arrests[state %in% selected_state]
  agency <- unique(temp$agency)
  agency <- jsonlite::toJSON(agency, pretty = FALSE)
  write(agency, paste0(selected_state, "_agency_choices.json"))
}

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_data/data/arrests")
largest_agency <- arrests %>%
  dplyr::group_by(state) %>%
  dplyr::top_n(1, population) %>%
  dplyr::select(state, agency)
largest_agency <- jsonlite::toJSON(largest_agency, pretty = TRUE)
write(largest_agency, "largest_agency_choices.json")
rm(arrests); gc()

