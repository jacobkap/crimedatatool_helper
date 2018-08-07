load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/hate_crimes/ucr_hate_crimes_1992_2016.rda")
source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/utils.R')
shr <-
  ucr_hate_crimes_1992_2016 %>%
  dplyr::left_join(crosswalk_agencies) %>%
  dplyr::mutate(population = current_population_1 +
                   current_population_2 +
                   current_population_3 +
                   current_population_4 +
                   current_population_5) %>%
  dplyr::select(ori,
                state,
                agency,
                population,
                year,
                incident_date,
                total_num_of_individual_victims,
                total_offenders,
                offenders_race_as_a_group,
                bias_motivation_offense_1,
                location_code_offense_1,
                ucr_offense_code_1
                )
