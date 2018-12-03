library(tidyverse)
library(asciiSetupReader)
library(zoo)
library(stringr)
library(readr)
setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/census_data")

cols <- c("YEAR",
          "STATEFIP",
          "PERWT",
          "AGE",
          "RACE",
          "HISPAN",
          "SEX")

age_fix <- c("100 (100+ in 1960-1970)" = "100",
             "90 (90+ in 1980 and 1990)" = "90",
             "less than 1 year old" = "0")

race_fix <- c("white"                            = "white",
              "other race, nec"                  = "other or unknown",
              "chinese"                          = "asian",
              "black/african american/negro"     = "black",
              "japanese"                         = "asian",
              "other asian or pacific islander"  = "asian",
              "american indian or alaska native" = "american indian",
              "two major races"                  = "other or unknown",
              "three or more major races"        = "other or unknown")

ethnicity_fix <- c("puerto rican" = "hispanic",
                   "mexican"      = "hispanic",
                   "other"        = "hispanic",
                   "cuban"        = "hispanic")

make_race_pop_cols <- function(data, race, total_pop = FALSE) {
  clean_race <- gsub(" ", "_", race)

  data[, paste0("population_",  clean_race)] <- 0
  data[, paste0("population_female_", clean_race)] <- 0
  data[, paste0("population_male_",   clean_race)] <- 0

  if (total_pop) {
    data[, paste0("population_", clean_race)] <- 1
  } else {
    data[data$race %in% race, paste0("population_", clean_race)] <- 1
  }
  data[, paste0("population_female_", clean_race)] <-
    data[, paste0("population_", clean_race)] * data$female
  data[, paste0("population_male_", clean_race)] <-
    data[, paste0("population_", clean_race)] * data$male

  # Adult
  data[, paste0("population_adult_", clean_race)] <-
    data[, paste0("population_", clean_race)] * data$population_adult
  data[, paste0("population_female_adult_", clean_race)] <-
    data[, paste0("population_", clean_race)] * data$population_adult * data$female
  data[, paste0("population_male_adult_", clean_race)] <-
    data[, paste0("population_", clean_race)] * data$population_adult * data$male

  # Age 18-65
  data[, paste0("population_aged_18_65_", clean_race)] <-
    data[, paste0("population_", clean_race)] * data$population_aged_18_65
  data[, paste0("population_female_aged_18_65_", clean_race)] <-
    data[, paste0("population_", clean_race)] * data$population_aged_18_65 * data$female
  data[, paste0("population_male_aged_18_65_", clean_race)] <-
    data[, paste0("population_", clean_race)] * data$population_aged_18_65 * data$male

  names(data) <- gsub("_$", "", names(data))
  data <- data[, unique(names(data))]
  gc(); Sys.sleep(1.5)
  return(data)
}


clean_census <- function(years) {
  data <-
    spss_ascii_reader(paste0("census_", years, ".dat"),
                      "census_acs.sps",
                      keep_columns = cols) %>%
    dplyr::rename(year      = Census_year,
                  state     = State_FIPS_code,
                  race      = Race_general_version,
                  ethnicity = Hispanic_origin_general_version,
                  weight    = Person_weight,
                  age       = Age,
                  sex       = Sex) %>%
    dplyr::mutate(sex       = tolower(sex),
                  race      = tolower(race),
                  race      = str_replace_all(race, race_fix),
                  age       = tolower(age),
                  age       = str_replace_all(age, age_fix),
                  age       = parse_number(age),
                  ethnicity = tolower(ethnicity),
                  ethnicity = str_replace_all(ethnicity, ethnicity_fix),
                  weight    = weight / 100)

  data$population_adult <- 0
  data$population_aged_18_65 <- 0
  data$population_adult[data$age >= 18] <- 1
  data$population_aged_18_65[data$age %in% 18:65] <- 1
  data$age <- NULL; gc()

  data$race[data$ethnicity %in% "hispanic"] <- "hispanic"
  data$ethnicity <- NULL; gc()

  data$female <- 0
  data$male   <- 0
  data$female[data$sex %in% "female"] <- 1
  data$male[data$sex   %in% "male"]   <- 1
  data$sex <- NULL; gc()


  data <- make_race_pop_cols(data, "", total_pop = TRUE)
  data <- make_race_pop_cols(data, "american indian")
  data <- make_race_pop_cols(data, "asian")
  data <- make_race_pop_cols(data, "black")
  data <- make_race_pop_cols(data, "hispanic")
  data <- make_race_pop_cols(data, "other or unknown")
  data <- make_race_pop_cols(data, "white")


pop_cols <- grep("population", names(data), value = TRUE)
for (col in pop_cols) {
  data[, col] <- data[, col] * data$weight
}

  data <-
    data %>%
    dplyr::select(-race,
                  -male,
                  -female,
                  -weight) %>%
    dplyr::group_by(year,
                    state) %>%
    dplyr::summarise_all(sum)

  gc(); Sys.sleep(2)
  return(data)
}

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
      for (n in 3:(ncol(temp))) {
        yearly_adder <- temp_data2[1, n] - temp_data1[1, n]
        yearly_adder <- yearly_adder / 10

        temp[i, n] <- temp[i - 1, n] + yearly_adder
        temp[i, n] <- round(temp[i, n], digits = 1)
      }
    }

    final <- dplyr::bind_rows(final, temp)
  }
  return(final)
}
