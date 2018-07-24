load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/ASR/asr_simple_1980_2016.rda")
source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/utils.R')

arrests <-
  asr_simple_1980_2016 %>%
  dplyr::filter(!state %in% c("guam",
                              "canal zone",
                              "puerto rico",
                              "virgin islands")) %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA") %>%
  dplyr::select(-one_of(arrests_to_drop)) %>%
  dplyr::rename(ORI               = ori) %>%
  dplyr::select(starting_cols,
                dplyr::matches("agg_assault"),
                dplyr::matches("oth_assault"),
                dplyr::matches("arson"),
                dplyr::matches("burglary"),
                dplyr::matches("curfew"),
                dplyr::matches("disorder_cond"),

                dplyr::matches("total_drug"),
                dplyr::matches("poss_drug"),
                dplyr::matches("poss_narcotic"),
                dplyr::matches("poss_cannabis"),
                dplyr::matches("poss_coke"),
                dplyr::matches("poss_oth_drug"),
                dplyr::matches("sale_drug"),
                dplyr::matches("sale_narcotic"),
                dplyr::matches("sale_cannabis"),
                dplyr::matches("sale_coke"),
                dplyr::matches("sale_oth_drug"),

                dplyr::matches("drunkenness"),
                dplyr::matches("dui"),
                dplyr::matches("embezzlement"),
                dplyr::matches("family_offenses"),
                dplyr::matches("forgery"),
                dplyr::matches("fraud"),

                dplyr::matches("total_gambling"),
                dplyr::matches("bookmaking"),
                dplyr::matches("number_lottery"),
                dplyr::matches("oth_gambling"),

                dplyr::matches("liquor"),
                dplyr::matches("manslaught_neg"),
                dplyr::matches("mtr_veh_theft"),
                dplyr::matches("murder"),
                dplyr::matches("oth_non_traffic"),
                dplyr::matches("prostitution"),
                dplyr::matches("rape"),
                dplyr::matches("robbery"),
                dplyr::matches("runaway"),
                dplyr::matches("oth_sex_off"),
                dplyr::matches("stolen_property"),
                dplyr::matches("suspicion"),
                dplyr::matches("theft"),
                dplyr::matches("vagrancy"),
                dplyr::matches("vandalism"),
                dplyr::matches("weapons"))
rm(asr_simple_1980_2016); gc()

z = arrests[!duplicated(arrests$ORI),]
z$temp <- paste(z$agency, z$state)
z = z[duplicated(z$temp),]
arrests <- arrests[!arrests$ORI %in% z$ORI, ]
arrests$agency <- sapply(arrests$agency, simpleCap)
arrests$state <- sapply(arrests$state, simpleCap)
arrests$state <- gsub(" Of ", " of ", arrests$state)

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/arrests")
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

for (selected_state in unique(arrests$state)) {
  temp   <- arrests[state %in% selected_state]
  agency <- unique(temp$agency)
  agency <- jsonlite::toJSON(agency, pretty = FALSE)
  write(agency, paste0(selected_state, "_agency_choices.json"))
}

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/arrests")
largest_agency <- arrests %>%
  dplyr::group_by(state) %>%
  dplyr::top_n(1, population) %>%
  dplyr::select(state, agency)
largest_agency <- jsonlite::toJSON(largest_agency, pretty = TRUE)
write(largest_agency, "largest_agency_choices.json")
rm(arrests); gc()


