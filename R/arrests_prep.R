source('~/crimedatatool_helper/R/utils.R')
setwd("E:/ucr_data_storage/clean_data/arrests")
arrests <- readRDS("E:/ucr_data_storage/clean_data/arrests/ucr_arrests_yearly_all_crimes_race_sex_1974_2021.rds") %>%
  filter(number_of_months_reported %in% 12) %>%
  select(-matches("num_months"))

arrests <-
  arrests %>%
  dplyr::filter(!state %in% c("guam",
                              "canal zone",
                              "puerto rico",
                              "virgin islands")) %>%
  dplyr::left_join(crosswalk_agencies, by = "ori") %>%
  dplyr::filter(agency != "NANA",
                state != "98") %>%
  dplyr::select(-one_of(arrests_to_drop)) %>%
  dplyr::rename(ORI = ori) %>%
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
                dplyr::matches("gamble_bookmake"),
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

arrests <- data.frame(arrests)
for (arrest_category in arrest_categories) {
  arrests[, paste0("all_arrests_total_", arrest_category)] <-
    rowSums(arrests[, paste0(all_unique_arrest_cols, "_", arrest_category )], na.rm = TRUE)
}


arrests <-
  arrests %>%
  dplyr::select(agency,
                ORI,
                year,
                state,
                population,
                dplyr::everything())
arrests$agency <- gsub("\\(|\\)", "", arrests$agency)
arrests <- remove_duplicate_capitalize_names(arrests)

setwd("~/crimedatatool_helper/data/arrests")
make_agency_csvs(arrests)
make_state_agency_choices(arrests)
make_largest_agency_json(arrests)
rm(arrests)
