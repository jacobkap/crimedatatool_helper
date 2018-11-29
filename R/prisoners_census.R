setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/census_data")
library(tidyverse)
library(asciiSetupReader)
library(zoo)

census_yearly <-
  spss_ascii_reader("census_2001_2016.dat",
                    "census_acs.sps")  %>%
  dplyr::rename(year = Census_year,
                state = State_FIPS_code) %>%
  dplyr::group_by(year,
                  state) %>%
  dplyr::mutate(Person_weight = Person_weight / 100) %>%
  dplyr::summarise(population = sum(Person_weight))

census_decennial <-
  spss_ascii_reader("census_1970_2000.dat",
                    "census_acs.sps")  %>%
  dplyr::rename(year = Census_year,
                state = State_FIPS_code) %>%
  dplyr::group_by(year,
                  state) %>%
  dplyr::mutate(Person_weight = Person_weight / 100) %>%
  dplyr::summarise(population = sum(Person_weight))

z1970 <- census_decennial[census_decennial$year == 1970,]
z1980 <- census_decennial[census_decennial$year == 1980,]
z1990 <- census_decennial[census_decennial$year == 1990,]
z2000 <- census_decennial[census_decennial$year == 2000,]
z1970_1980 <- census_interpolator(z1970, z1980)
z1980_1990 <- census_interpolator(z1980, z1990)
z1990_2000 <- census_interpolator(z1990, z2000)

all_census <-
  z1970_1980 %>%
  dplyr::bind_rows(z1980_1990) %>%
  dplyr::bind_rows(z1990_2000) %>%
  dplyr::distinct(.keep_all = TRUE) %>%
  dplyr::filter(year %in% 1978:2000) %>%
  dplyr::bind_rows(census_yearly)

national_data <-
  all_census %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(population = sum(population)) %>%
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
  dplyr::bind_rows(national_data3)

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/raw_data")
save(prisoners_census, file = "prisoners_census.rda")

census_interpolator <- function(data1, data2) {
  final <- data.frame()
  for (geo_unit in unique(data1$state)) {
    temp_data1 <- data1[data1$state == geo_unit, ]
    temp_data2 <- data2[data2$state == geo_unit, ]
    temp <- data.frame(matrix(ncol = ncol(data1), nrow = 11))
    names(temp) <- names(temp_data1)
    temp$year <- data1$year[1]:data2$year[1]
    temp$state <- temp_data1$state
    temp[1, ] <- temp_data1[1, ]
    temp[11, ] <- temp_data2[1, ]

    for (i in 2:(nrow(temp)-1)) {
        yearly_adder <- temp_data2$population[1] - temp_data1$population[1]
        yearly_adder <- yearly_adder / 10

        temp$population[i] <- temp$population[i - 1] + yearly_adder
      }
    final <- dplyr::bind_rows(final, temp)
  }
  final$population <- round(final$population)
  return(final)
}
