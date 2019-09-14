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

texas_pregnant  = prep_jails(texas_pregnant, state = "Texas")
texas_immigrant = prep_jails(texas_immigrant, state = "Texas")
texas_pop       = prep_jails(texas_pop, state = "Texas")

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
texas_jails <-
  texas_jails %>%
  dplyr::rename(agency = county,
                population = total_population)
make_state_agency_choices(texas_jails)
make_largest_agency_json(texas_jails)


setwd("C:/Users/user/Dropbox/R_project/california_jails/clean_data")
load('california_jail_county_monthly_1995_2018.rda')
ca_jails <-
  california_jail_county_monthly_1995_2018 %>%
  dplyr::rename(county = jurisdiction) %>%
  dplyr::select(-date,
                -census_county_name,
                -fips_state_code,
                -fips_county_code,
                -fips_state_county_code,
                -day_of_highest_count)
ca_jails$county <- gsub(" Sheriff's Department", "", ca_jails$county)
ca_jails <- prep_jails(ca_jails, state = "California")
ca_jails <-
  ca_jails %>%
  dplyr::select(county,
                state,
                year,
                everything()) %>%
  dplyr::arrange(county,
                 desc(year))

setwd(here::here("data/jail"))
make_agency_csvs(ca_jails, county = TRUE)
ca_jails <-
  ca_jails %>%
  dplyr::rename(agency = county,
                population = avg_daily_pop_total_jurisdiction)
make_state_agency_choices(ca_jails)



prep_jails <- function(data, state) {
  data$link <- NULL
  data <-
    data %>%
    dplyr::group_by(county,
                    year,
                    month) %>%
    dplyr::summarize_all(sum)

  data      <- fastDummies::dummy_rows(data)
  data$date <- ymd(paste(data$year, data$month, "1", sep = "-"))
  data$county <- sapply(data$county, simpleCap)
  data$county <- gsub("\\(|\\)", "", data$county)
  data$county <- gsub("private", "Private", data$county)

  data <-
    data %>%
    dplyr::select(-year,
                  -month) %>%
    dplyr::rename(year = date) %>%
    dplyr::mutate(state = state)

  return(data)
}


# http://www.icjia.state.il.us/research/overview
library(readxl)
library(data.table)
setwd(here::here("raw_data"))
download.file("http://www.icjia.state.il.us/assets/datasets/130/xls/JailBookings.xls",
              destfile = "illinois_jail_booking.xls",
              mode = "wb")
download.file("http://www.icjia.state.il.us/assets/datasets/120/xls/JailADP.xls",
              destfile = "illinois_jail_adp.xls",
              mode = "wb")
il_bookings <- read_xls("illinois_jail_booking.xls",
                        sheet = 2)
il_adp      <- read_xls("illinois_jail_adp.xls",
                        sheet = 2)

il_bookings <- prep_illinois(il_bookings, "number_of_inmates_booked")
il_adp      <- prep_illinois(il_adp, "average_daily_population")

il_jails <-
  il_bookings %>%
  dplyr::left_join(il_adp) %>%
  dplyr::select(county,
                state,
                year,
                everything()) %>%
  dplyr::mutate(year = as.numeric(as.character(year))) %>%
  dplyr::arrange(county,
                 desc(year))
setwd(here::here("data/jail"))
make_agency_csvs(il_jails, county = TRUE)
il_jails <-
  il_jails %>%
  dplyr::rename(agency = county,
                population = average_daily_population)
make_state_agency_choices(il_jails)

texas_jails$year <- as.character(texas_jails$year)
il_jails$year    <- as.character(il_jails$year)
ca_jails$year    <- as.character(ca_jails$year)
il_jails$population <- as.numeric(il_jails$population)
temp <- bind_rows(ca_jails, texas_jails, il_jails)
make_largest_agency_json(temp)

prep_illinois <- function(data, type) {
  data                <- data[6:113, ]
  names(data)         <- data[1, ]
  data                <- data[-1, ]
  data$ICJIAnumber    <- NULL
  data$FIPS           <- NULL
  data$ICJIAnumber_FK <- NULL

  data <- melt(data, id.vars = c("County"))

  data <- data[!data$County %in% c("Central",
                                   "Northern minus Cook",
                                   "South",
                                   "Tri-County Jail",
                                   "Illinois"), ]

  names(data) <- c("county", "year", type)
  data$state = "Illinois"

  return(data)
}


