source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/prisoners_utils.R')
setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/census_data")
census_1970_1980 <- clean_census("1970_1980")
census_1990_2000 <- clean_census("1990_2000")
census_2001_2008 <- clean_census("2001_2008")
census_2009_2016 <- clean_census("2009_2016")


z1970 <- census_1970_1980[census_1970_1980$year == 1970,]
z1980 <- census_1970_1980[census_1970_1980$year == 1980,]
z1990 <- census_1990_2000[census_1990_2000$year == 1990,]
z2000 <- census_1990_2000[census_1990_2000$year == 2000,]
z1970_1980 <- census_interpolator(z1970, z1980)
z1980_1990 <- census_interpolator(z1980, z1990)
z1990_2000 <- census_interpolator(z1990, z2000)

all_census <-
  z1970_1980 %>%
  dplyr::bind_rows(z1980_1990) %>%
  dplyr::bind_rows(z1990_2000) %>%
  dplyr::distinct(.keep_all = TRUE) %>%
  dplyr::filter(year %in% 1978:2000) %>%
  dplyr::bind_rows(census_2001_2008) %>%
  dplyr::bind_rows(census_2009_2016)

national_data <-
  all_census %>%
  dplyr::select(-state) %>%
  dplyr::group_by(year) %>%
  dplyr::summarize_all(sum) %>%
  dplyr::mutate(state = "US Prison Total")
national_data2 <-
  national_data %>%
  dplyr::mutate(state = "State Prison Total")
national_data3 <-
  national_data %>%
  dplyr::mutate(state = "Federal Prison Total")

prisoners_census <-
  all_census %>%
  dplyr::bind_rows(national_data) %>%
  dplyr::bind_rows(national_data2) %>%
  dplyr::bind_rows(national_data3) %>%
  dplyr::arrange(desc(year),
                 state)

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/raw_data")
save(prisoners_census, file = "prisoners_census.rda")
