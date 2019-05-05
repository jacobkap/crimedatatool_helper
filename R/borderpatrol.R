source(here::here('R/utils.R'))
setwd(here::here("data/borderpatrol"))

load("C:/Users/user/Dropbox/R_project/borderpatrol/data/clean/sector_profile_2011_2017.rda")
load("C:/Users/user/Dropbox/R_project/borderpatrol/data/clean/apprehensions_seizures_stats_2011_2017.rda")
load("C:/Users/user/Dropbox/R_project/borderpatrol/data/clean/southwest_border_apprehensions_1960_2018.rda")
load("C:/Users/user/Dropbox/R_project/borderpatrol/data/clean/southwest_border_deaths_1998_2018.rda")
load("C:/Users/user/Dropbox/R_project/borderpatrol/data/clean/family_child_total_monthly_2000_2018.rda")
load("C:/Users/user/Dropbox/R_project/borderpatrol/data/clean/nationwide_total_apprehensions_1925_2018.rda")
load("C:/Users/user/Dropbox/R_project/borderpatrol/data/clean/border_patrol_staffing_1992_2018.rda")
nationwide <-
  nationwide_total_apprehensions_1925_2018 %>%
  dplyr::mutate(sector = "nationwide total")
family <-
  family_child_total_monthly_2000_2018 %>%
  dplyr::select(-month) %>%
  dplyr::filter(fiscal_year > 2009) %>%
  dplyr::group_by(sector,
                  fiscal_year) %>%
  dplyr::summarize_all(sum) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(fiscal_year))

seizures <- apprehensions_seizures_stats_2011_2017 %>%
  dplyr::select(-aliens_special_interest_county)
border <- sector_profile_2011_2017
save_as_csv(border, "sector_profile")
save_as_csv(seizures, "seizures")
save_as_csv(southwest_border_apprehensions_1960_2018, "southwest_apprehensions")
save_as_csv(southwest_border_deaths_1998_2018, "southwest_deaths")
save_as_csv(family, "family")
save_as_csv(nationwide, "nationwide")
save_as_csv(border_patrol_staffing_1992_2018, "staffing")

save_as_csv <- function(data, file_name) {
  data$sector <- gsub(" border", " border total", data$sector)
  data$sector <- gsub("_", " ", data$sector)
  data$sector <- sapply(data$sector, simpleCap)

  for (sector in unique(data$sector)) {
    temp <- data[data$sector %in% sector, ]
    temp <-
      temp %>%
      dplyr::arrange(fiscal_year)
    readr::write_csv(temp,
                     path = paste0(file_name, "_", sector, ".csv"))

  }
}

