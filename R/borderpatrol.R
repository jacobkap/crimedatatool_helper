source(here::here('R/utils.R'))
setwd(here::here("data/borderpatrol"))

load("C:/Users/user/Dropbox/R_project/borderpatrol/data/clean/sector_profile_2011_2017.rda")
border <- sector_profile_2011_2017
border$sector <- gsub(" border", " border total", border$sector)
border$sector <- sapply(border$sector, simpleCap)
border$sector <- gsub(" ", "_", border$sector)

for (sector in unique(border$sector)) {
  temp <- border[border$sector %in% sector, ]
  temp <-
    temp %>%
    dplyr::arrange(fiscal_year)
  readr::write_csv(temp,
                   path = paste0("border_patrol_sector", "_", sector, ".csv"))

}

