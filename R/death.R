setwd(here::here("data/raw_data"))
source(here::here('R/utils.R'))


death_totals <- clean_cdc_data("Underlying Cause of Death_intent, 1999-2017.txt",
                               split_fixed_num = 12)
drug_totals <- clean_cdc_data("drug_totals_Underlying Cause of Death, 1999-2017.txt",
                              split_fixed_num = 11)
drug_subtotals <- clean_cdc_data("drug_subtotals_Underlying Cause of Death, 1999-2017.txt",
                                 split_fixed_num = 11)

data <-
  death_totals %>%
  dplyr::bind_rows(drug_totals) %>%
  dplyr::bind_rows(drug_subtotals) %>%
  dplyr::mutate(cause_of_death = trimws(cause_of_death),
                cause_of_death = stringr::str_replace_all(cause_of_death,
                                                          cause_of_death_fix),
                cause_of_death = tolower(cause_of_death))

data <- data.table::dcast(setDT(data), year+state+population~cause_of_death,
                          value.var = c('deaths',
                                        'crude_rate',
                                        "age_adjusted_rate"))

setwd(here::here("data/death"))
save_state_data(data, "death")

clean_cdc_data <- function(file_name, split_fixed_num) {
  data <- readr::read_lines(file_name)
  data <- gsub('\\"', "", data)
  data <- stringr::str_split_fixed(data, pattern = "\\t", n = split_fixed_num)
  data <- data.frame(data, stringsAsFactors = FALSE)
  data <- clean_cdc_colnames(data)
  names(data) <- gsub("induced_cause", "induced", names(data))
  names(data) <- gsub("drug/alcohol_induced", "cause_of_death", names(data))
  names(data) <- gsub("injury_intent", "cause_of_death", names(data))

  if (file_name == "drug_totals_Underlying Cause of Death, 1999-2017.txt") {
    data$cause_of_death[data$notes == "Total"] <- "Drug and Alcohol - Total"
  }

  data <-
    data %>%
    dplyr::select(state,
                  year,
                  cause_of_death,
                  deaths,
                  population,
                  crude_rate,
                  age_adjusted_rate) %>%
    dplyr::filter(year != "") %>%
    # Some values are "suppressed" or "unreliable" due to small samples
    # Those turn to NA.
    dplyr::mutate(deaths            = as.numeric(deaths),
                  population        = as.numeric(population),
                  crude_rate        = as.numeric(crude_rate),
                  age_adjusted_rate = as.numeric(age_adjusted_rate))
  return(data)
}






