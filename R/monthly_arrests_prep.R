source('~/crimedatatool_helper/R/utils.R')

for (year in 1974:2020) {
  setwd("D:/ucr_data_storage/clean_data/arrests")
  temp <- readRDS(paste0("ucr_arrests_monthly_all_crimes_race_sex_", year, ".rds")) %>%
    dplyr::mutate(date = paste0(year, "-", month, "-1"),
                  date = lubridate::ymd(date)) %>%
    dplyr::select(-year) %>%
    dplyr::rename(year = date) %>%
    dplyr::filter(number_of_months_reported %in% 12) %>%
    dplyr::filter(!state %in% c("guam",
                                "canal zone",
                                "puerto rico",
                                "virgin islands"),
                  !is.na(ori)) %>%
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

  for (arrest_category in arrest_categories) {
    temp[, paste0("all_arrests_total_", arrest_category)] <-
      rowSums(temp[, paste0(all_unique_arrest_cols, "_", "tot_adult" )], na.rm = TRUE)
  }
  temp <-
    temp %>%
    dplyr::select(agency,
                  ORI,
                  year,
                  state,
                  population,
                  dplyr::everything())

  temp$year <- as.character(temp$year)
  rm(temp_race)
  rm(temp_sex)


  save_monthly_state_temp(temp, start_year = 1974, type = "arrests")
  rm(temp); gc(); Sys.sleep(1)
  message(year)
}



make_monthly_agency_csvs(type = "arrests")
setwd("~/crimedatatool_helper/data/arrests")
files <- list.files(pattern = "largest_agency_choices")
file.copy(files, paste0(here::here("data/arrests_monthly/")), overwrite = TRUE)
