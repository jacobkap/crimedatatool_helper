load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/LEOKA/leoka_yearly_1960_2017.rda")
source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/utils.R')

employee_cols <- sort(grep("employee", names(leoka_yearly_1960_2017),
                           value = TRUE))
killed_cols <- sort(grep("killed", names(leoka_yearly_1960_2017),
                           value = TRUE))
injury_cols <- sort(grep("with_injury", names(leoka_yearly_1960_2017),
                           value = TRUE))
injury_cols <- injury_cols[!grepl("indicator", injury_cols)]
no_injury_cols <- sort(grep("no_injury", names(leoka_yearly_1960_2017),
                         value = TRUE))
no_injury_cols <- no_injury_cols[!grepl("indicator", no_injury_cols)]
total_assault_cols <- sort(grep("^total_assault", names(leoka_yearly_1960_2017),
                            value = TRUE))
total_assault_cols <- total_assault_cols[!grepl("clear|traffic", total_assault_cols)]
assaults <- sort(grep("_assault_|.total_assaults", names(leoka_yearly_1960_2017),
                      value = TRUE))
assaults <- assaults[!grepl("^total_assault|time|all_other", assaults)]
all_other_assaults <- sort(grep("all_other_assault", names(leoka_yearly_1960_2017),
                      value = TRUE))
all_other_assaults <- all_other_assaults[!grepl("clear", all_other_assaults)]

leoka <-
  leoka_yearly_1960_2017 %>%
  dplyr::filter(!state %in% c("guam",
                              "canal zone",
                              "puerto rico",
                              "virgin islands")) %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::filter(agency != "NANA") %>%
  dplyr::rename(ORI               = ori) %>%
  dplyr::select(starting_cols,
                number_of_months_reported,
                employee_cols,
                killed_cols,
                injury_cols,
                no_injury_cols,
                assaults,
                all_other_assaults,
                total_assault_cols)
rm(leoka_yearly_1960_2017); gc();

z = leoka[!duplicated(leoka$ORI),]
z$temp <- paste(z$agency, z$state)
z = z[duplicated(z$temp),]
leoka <- leoka[!leoka$ORI %in% z$ORI, ]
leoka$agency      <- sapply(leoka$agency, simpleCap)
leoka$state       <- sapply(leoka$state, simpleCap)


setwd(here::here("data/leoka"))
leoka <- data.table::data.table(leoka)
for (selected_ori in sort(unique(leoka$ORI))) {
  temp   <- leoka[ORI %in% selected_ori]

  if (any(temp$number_of_months_reported %in% 12)) {

  temp   <- na_non_12_month_rows(temp)
  temp   <- dummy_rows_missing_years(temp)
  temp$number_of_months_reported <- NULL

  state  <- unique(temp$state)
  agency <- unique(temp$agency)
  state  <- gsub(" ", "_", state)
  agency <- gsub(" |:", "_", agency)
  agency <- gsub("/", "_", agency)
  agency <- gsub("_+", "_", agency)

  readr::write_csv(temp,
                   path = paste0(state, "_", agency, ".csv"))
  }
}

make_state_agency_choices(leoka)
make_largest_agency_json(leoka)
