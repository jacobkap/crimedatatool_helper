load("C:/Users/user/Dropbox/R_project/alcohol/data/apparent_per_capita_alcohol_consumption.rda")

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/alcohol")
save_state_data(apparent_per_capita_alcohol_consumption)
save_state_data <- function(data) {
  for (selected_state in sort(unique(data$state))) {
    temp <-
      data %>%
      dplyr::filter(state %in% selected_state)

    save_state     <- unique(temp$state)
    save_state     <- gsub(" ", "_", save_state)

    readr::write_csv(temp,
                     path = paste0(save_state, "_","alcohol.csv"))
  }
}
