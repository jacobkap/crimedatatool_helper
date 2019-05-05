source(here::here('R/utils.R'))
setwd(here::here("data/borderpatrol"))

load("C:/Users/user/Dropbox/R_project/borderpatrol/data/clean/sector_profile_2011_2017.rda")
load("C:/Users/user/Dropbox/R_project/borderpatrol/data/clean/apprehensions_seizures_stats_2011_2017.rda")
load("C:/Users/user/Dropbox/R_project/borderpatrol/data/clean/southwest_border_apprehensions_1960_2018.rda")
load("C:/Users/user/Dropbox/R_project/borderpatrol/data/clean/southwest_border_deaths_1998_2018.rda")


seizures <- apprehensions_seizures_stats_2011_2017
border <- sector_profile_2011_2017
save_as_csv(border, "sector_profile")

save_as_csv <- function(data, file_name) {
border$sector <- gsub(" border", " border total", border$sector)
border$sector <- sapply(border$sector, simpleCap)
border$sector <- gsub(" ", "_", border$sector)

for (sector in unique(border$sector)) {
  temp <- border[border$sector %in% sector, ]
  temp <-
    temp %>%
    dplyr::arrange(fiscal_year)
  readr::write_csv(temp,
                   path = paste0(file_name, "_", sector, ".csv"))

}
}

