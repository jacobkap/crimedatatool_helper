source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/utils.R')
load("C:/Users/user/Dropbox/R_project/alcohol/data/apparent_per_capita_alcohol_consumption.rda")
apparent_per_capita_alcohol_consumption$state <- sapply(apparent_per_capita_alcohol_consumption$state,
                                                        simpleCap)
apparent_per_capita_alcohol_consumption <-
  apparent_per_capita_alcohol_consumption %>%
  dplyr::mutate(number_of_beers = round(number_of_beers),
                number_of_glasses_wine = round(number_of_glasses_wine),
                number_of_shots_liquor = round(number_of_shots_liquor),
                number_of_drinks_total = round(number_of_drinks_total),
                state = gsub("Us ", "US ", state)) %>%
  dplyr::select(state,
                year,
                number_of_beers,
                number_of_glasses_wine,
                number_of_shots_liquor,
                number_of_drinks_total)
setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/alcohol")
save_state_data(apparent_per_capita_alcohol_consumption, "alcohol")

