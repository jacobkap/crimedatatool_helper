source(here::here('R/utils.R'))
setwd("C:/Users/user/Dropbox/R_project/texas_jails/data/clean_data")
texas_immigrant <- read_csv("texas_jails_immigrant_detainer_2011_2017.csv")
names(texas_immigrant) <- c("county",
                            "year",
                            "month",
                            "num_of_immigrant_detainer_inmates",
                            "num_of_immigrant_detainer_inmate_days",
                            "immigrant_detainer_cost_in_dollars",
                            "link")
texas_pop       <- read_csv("texas_jails_jail_pop_1992_2017.csv")
texas_pregnant  <- read_csv("texas_jails_pregnant_inmates_2012_2017.csv")

texas_pregnant  = prep_texas(texas_pregnant)
texas_immigrant = prep_texas(texas_immigrant)
texas_pop       = prep_texas(texas_pop)

texas_jails <-
  texas_pop %>%
  dplyr::full_join(texas_pregnant) %>%
  dplyr::full_join(texas_immigrant) %>%
  dplyr::select(county,
                state,
                year,
                everything()) %>%
  dplyr::arrange(county,
                 desc(year))

setwd(here::here("data/jail"))
make_agency_csvs(texas_jails, county = TRUE)

prep_texas <- function(data) {
  data <-
    data %>%
    dplyr::select(-link) %>%
    dplyr::group_by(county,
                    year,
                    month) %>%
    dplyr::summarize_all(sum)

  data      <- fastDummies::dummy_rows(data)
  data$date <- ymd(paste(data$year, data$month, "1", sep = "-"))
  data$county <- sapply(data$county, simpleCap)
  data$county <- gsub("private", "Private", data$county)

  data <-
    data %>%
    dplyr::select(-year,
                  -month) %>%
    dplyr::rename(year = date) %>%
    dplyr::mutate(state = "Texas")

  return(data)
}
