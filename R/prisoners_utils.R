library(tidyverse)
library(asciiSetupReader)
library(zoo)
library(stringr)
library(readr)

custody_cols <- c("custody_unsentenced_male",
                  "custody_unsentenced_female",
                  "total_under_custody_male",
                  "total_under_custody_female",
                  "custody_public_prisons_male",
                  "custody_public_prisons_female",
                  "custody_including_private_facilities_male",
                  "custody_including_private_facilities_female",
                  "custody_private_prison_male",
                  "custody_private_prison_female",
                  "custody_unsentenced_total",
                  "total_under_custody_total",
                  "custody_public_prisons_total",
                  "custody_private_prison_total")
custody_cols <- sort(custody_cols)

jurisdiction_cols <- c("jurisdiction_unsentenced_male",
                       "jurisdiction_unsentenced_female",
                       "total_under_jurisdiction_male",
                       "total_under_jurisdiction_female",
                       "jurisdiction_private_prison_in_state_male",
                       "jurisdiction_private_prison_in_state_female",
                       "jurisdiction_private_prison_out_of_state_male",
                       "jurisdiction_private_prison_out_of_state_female",
                       "jurisdiction_housed_in_local_facility_male",
                       "jurisdiction_housed_in_local_facility_female",
                       "jurisdiction_local_facilities_solely_to_ease_prison_crowding_male",
                       "jurisdiction_local_facilities_solely_to_ease_prison_crowding_female",
                       "jurisdiction_private_prison_male",
                       "jurisdiction_private_prison_female",
                       "jurisdiction_public_prison_male",
                       "jurisdiction_public_prison_female",
                       "jurisdiction_unsentenced_total",
                       "total_under_jurisdiction_total",
                       "jurisdiction_private_prison_total",
                       "jurisdiction_private_prison_out_of_state_total",
                       "jurisdiction_private_prison_in_state_total",
                       "jurisdiction_local_facilities_solely_to_ease_prison_crowding_total",
                       "jurisdiction_housed_in_local_facility_total",
                       "jurisdiction_public_prison_total")
jurisdiction_cols <- sort(jurisdiction_cols)

race_ethnicity_cols <- c("white_male",
                         "white_female",
                         "black_male",
                         "black_female",
                         "hispanic_or_latino_male",
                         "hispanic_or_latino_female",
                         "american_indian_male",
                         "american_indian_female",
                         "asian_male",
                         "asian_female",
                         "other_or_unknown_race_female",
                         "other_or_unknown_race_male",
                         "other_or_unknown_race_total",
                         "white_total",
                         "black_total",
                         "hispanic_or_latino_total",
                         "american_indian_total",
                         "asian_total")
race_ethnicity_cols <- sort(race_ethnicity_cols)

noncitizen_juvenile_cols <- c("in_custody_under_18_years_of_age_male",
                              "in_custody_under_18_years_of_age_female",
                              "in_custody_not_us_citizens_male",
                              "in_custody_not_us_citizens_female",
                              "in_custody_under_18_years_of_age_total",
                              "in_custody_not_us_citizens_total")
noncitizen_juvenile_cols <- sort(noncitizen_juvenile_cols)

capacity_cols <- c("rated_capacity_male",
                   "rated_capacity_female",
                   "rated_capacity_total",
                   "operational_capacity_male",
                   "operational_capacity_female",
                   "operational_capacity_total",
                   "design_capacity_male",
                   "design_capacity_female",
                   "design_capacity_total")
capacity_cols <- sort(capacity_cols)

death_cols <- c("deaths_from_execution_male",
                "deaths_from_execution_female",
                "deaths_from_illness_or_natural_cause_male",
                "deaths_from_illness_or_natural_cause_female",
                "deaths_from_aids_male",
                "deaths_from_aids_female",
                "deaths_from_suicide_male",
                "deaths_from_suicide_female",
                "deaths_from_accidental_injury_to_self_male",
                "deaths_from_accidental_injury_to_self_female",
                "deaths_from_homicide_by_other_inmates_male",
                "deaths_from_homicide_by_other_inmates_female",
                "deaths_from_other_homicide_male",
                "deaths_from_other_homicide_female",
                "deaths_caused_by_another_person_male",
                "deaths_caused_by_another_person_female",
                "total_deaths_male",
                "total_deaths_female",
                "deaths_from_execution_total",
                "deaths_from_illness_or_natural_cause_total",
                "deaths_from_aids_total",
                "deaths_from_suicide_total",
                "deaths_from_accidental_injury_to_self_total",
                "deaths_from_homicide_by_other_inmates_total",
                "deaths_from_other_homicide_total",
                "deaths_caused_by_another_person_total",
                "total_deaths_total" )
death_cols <- sort(death_cols)

release_cols <- c("unconditional_release_expirations_of_sentence_male",
                  "unconditional_release_expirations_of_sentence_female",
                  "unconditional_release_commutations_male",
                  "unconditional_release_commutations_female",
                  "other_unconditional_release_male",
                  "other_unconditional_release_female",
                  "conditional_release_probations_male",
                  "conditional_release_probations_female",
                  "supervised_mandatory_release_male",
                  "supervised_mandatory_release_female",
                  "discretionary_parole_male",
                  "discretionary_parole_female",
                  "other_conditional_release_male",
                  "other_conditional_release_female",
                  "awol_release_male",
                  "awol_release_female",
                  "transfers_to_other_jurisdictions_male",
                  "transfers_to_other_jurisdictions_female",
                  "release_to_appeal_or_bond_male",
                  "release_to_appeal_or_bond_female",
                  "escape_from_confinement_male",
                  "escape_from_confinement_female",
                  "total_releases_male",
                  "total_releases_female",
                  "unconditional_release_expirations_of_sentence_total",
                  "unconditional_release_commutations_total",
                  "other_unconditional_release_total",
                  "conditional_release_probations_total",
                  "supervised_mandatory_release_total",
                  "discretionary_parole_total",
                  "other_conditional_release_total",
                  "awol_release_total",
                  "transfers_to_other_jurisdictions_total",
                  "release_to_appeal_or_bond_total",
                  "escape_from_confinement_total",
                  "total_releases_total")
release_cols <- sort(release_cols)

admission_cols <- c("new_court_commitments_male",
                    "new_court_commitments_female",
                    "parole_violators_with_new_sentence_male",
                    "parole_violators_with_new_sentence_female",
                    "parole_violators_without_new_sentence_male",
                    "parole_violators_without_new_sentence_female",
                    "other_conditional_release_violators_admitted_with_new_sentence_male",
                    "other_conditional_release_violators_admitted_with_new_sentence_female",
                    "other_conditional_release_violators_admitted_without_new_sentence_male",
                    "other_conditional_release_violators_admitted_without_new_sentence_female",
                    "transfers_admitted_from_other_jurisdictions_male",
                    "transfers_admitted_from_other_jurisdictions_female",
                    "awol_returns_with_or_without_new_sentences_male",
                    "awol_returns_with_or_without_new_sentences_female",
                    "escapee_returns_with_or_without_new_sentences_male",
                    "escapee_returns_with_or_without_new_sentences_female",
                    "returns_from_appeal_or_bond_male",
                    "returns_from_appeal_or_bond_female",
                    "other_admissions_male",
                    "other_admissions_female",
                    "total_admissions_male",
                    "total_admissions_female",
                    "new_court_commitments_total",
                    "parole_violators_with_new_sentence_total",
                    "parole_violators_without_new_sentence_total",
                    "other_conditional_release_violators_admitted_with_new_sentence_total",
                    "other_conditional_release_violators_admitted_without_new_sentence_total",
                    "transfers_admitted_from_other_jurisdictions_total",
                    "awol_returns_with_or_without_new_sentences_total",
                    "escapee_returns_with_or_without_new_sentences_total",
                    "returns_from_appeal_or_bond_total",
                    "other_admissions_total",
                    "total_admissions_total")
admission_cols <- sort(admission_cols)

aids_cols <- c("asymptomatic_hiv_positive_male",
               "asymptomatic_hiv_positive_female",
               "asymptomatic_hiv_positive_total",
               "infected_with_lesser_forms_of_symptomatic_hiv_disease_male",
               "infected_with_lesser_forms_of_symptomatic_hiv_disease_female",
               "infected_with_lesser_forms_of_symptomatic_hiv_disease_total",
               "confirmed_to_have_aids_male",
               "confirmed_to_have_aids_female",
               "confirmed_to_have_aids_total",
               "total_in_custody_hiv_positive_or_with_aids_male",
               "total_in_custody_hiv_positive_or_with_aids_female",
               "total_in_custody_hiv_positive_or_with_aids_total")
aids_cols <- sort(aids_cols)

prisoners_categories <- list(custody_cols,
                             jurisdiction_cols,
                             race_ethnicity_cols,
                             release_cols,
                             admission_cols,
                             capacity_cols,
                             noncitizen_juvenile_cols,
                             death_cols,
                             aids_cols)


names(prisoners_categories) <- c("custody",
                                 "jurisdiction",
                                 "race_ethnicity",
                                 "release",
                                 "admission",
                                 "capacity",
                                 "noncitizen_juvenile",
                                 "death",
                                 "aids")


prisoners_name_fix <- c(
  "inmates_under_jurisdiction_housed_in_privately_operated_correctional_facility_in_state_male" = "jurisdiction_private_prison_in_state_male",
  "inmates_under_jurisdiction_housed_in_privately_operated_correctional_facility_in_state_female" = "jurisdiction_private_prison_in_state_female",
  "inmates_under_jurisdiction_housed_in_privately_operated_correctional_facility_in_other_state_male"                                                   = "jurisdiction_private_prison_out_of_state_male",
  "inmates_under_jurisdiction_housed_in_privately_operated_correctional_facility_in_other_state_female"                                                 = "jurisdiction_private_prison_out_of_state_female",
  "inmates_under_jurisdiction_housed_in_local_facilities_operated_by_county_or_other_local_authority_male"                                              = "jurisdiction_housed_in_local_facility_male",
  "inmates_under_jurisdiction_housed_in_local_facilities_operated_by_county_or_other_local_authority_female"                                            = "jurisdiction_housed_in_local_facility_female",
  "local_facilities_solely_to_ease_prison_crowding_male" = "jurisdiction_local_facilities_solely_to_ease_prison_crowding_male",
  "local_facilities_solely_to_ease_prison_crowding_female" = "jurisdiction_local_facilities_solely_to_ease_prison_crowding_female",
  "american_indian_or_alaska_native_male"            = "american_indian_male",
  "american_indian_or_alaska_native_female"          = "american_indian_female",
  "native_hawaiian_or_other_pacific_islander_male"   = "native_hawaiian_male",
  "native_hawaiian_or_other_pacific_islander_female" = "native_hawaiian_female",
  "additional_other_categories_for_race_male"        = "other_race_male",
  "additional_other_categories_for_race_female"      = "other_race_female",
  "custody_excluding_private_facilities_male"        = "custody_public_prisons_male",
  "custody_excluding_private_facilities_female"      = "custody_public_prisons_female",
  "in_custody_not_u_s_citizens_male"                 = "in_custody_not_us_citizens_male",
  "in_custody_not_u_s_citizens_female"               = "in_custody_not_us_citizens_female"
)


cols <- c("YEAR",
          "STATEFIP",
          "PERWT",
          "AGE",
          "RACE",
          "HISPAN")

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
                  age       = Age) %>%
    dplyr::mutate(race      = tolower(race),
                  race      = str_replace_all(race, race_fix),
                  age       = tolower(age),
                  age       = str_replace_all(age, age_fix),
                  age       = parse_number(age),
                  ethnicity = tolower(ethnicity),
                  ethnicity = str_replace_all(ethnicity, ethnicity_fix),
                  weight    = weight / 100)

  data$population_adult <- 0
  data$population_adult_aged_18_65 <- 0
  data$population_adult[data$age >= 18] <- 1
  data$population_adult_aged_18_65[data$age %in% 18:65] <- 1
  data$age <- NULL; gc()

  data$race[data$ethnicity %in% "hispanic"] <- "hispanic"
  data$ethnicity <- NULL; gc()
  data$population_american_indian  <- 0
  data$population_asian            <- 0
  data$population_black            <- 0
  data$population_hispanic         <- 0
  data$population_other_or_unknown <- 0
  data$population_white            <- 0

  data$population_american_indian[data$race %in% "american indian"] <- 1
  data$population_asian[data$race %in% "asian"] <- 1
  data$population_black[data$race %in% "black"] <- 1
  data$population_hispanic[data$race %in% "hispanic"] <- 1
  data$population_other_or_unknown[data$race %in% "other or unknown"] <- 1
  data$population_white[data$race %in% "white"] <- 1

  data$population_adult_american_indian  <- data$population_american_indian * data$population_adult
  data$population_adult_asian            <- data$population_asian * data$population_adult
  data$population_adult_black            <- data$population_black * data$population_adult
  data$population_adult_hispanic       <- data$population_hispanic * data$population_adult
  data$population_adult_other_or_unknown <- data$population_other_or_unknown * data$population_adult
  data$population_adult_white            <- data$population_white * data$population_adult

  data$population_aged_18_65_american_indian  <- data$population_american_indian * data$population_adult_aged_18_65
  data$population_aged_18_65_asian            <- data$population_asian * data$population_adult_aged_18_65
  data$population_aged_18_65_black            <- data$population_black * data$population_adult_aged_18_65
  data$population_aged_18_65_hispanic         <- data$population_hispanic * data$population_adult_aged_18_65
  data$population_aged_18_65_other_or_unknown <- data$population_other_or_unknown * data$population_adult_aged_18_65
  data$population_aged_18_65_white            <- data$population_white * data$population_adult_aged_18_65

  data <-
    data %>%
    dplyr::group_by(year,
                    state) %>%
    dplyr::summarise(population = sum(weight),
                     # Adult population
                     population_adult = sum(weight[population_adult == 1]),
                     population_adult_aged_18_65 = sum(weight[population_adult_aged_18_65 == 1]),

                     # Race/ethnicity population
                     population_american_indian = sum(weight[population_american_indian == 1]),
                     population_asian = sum(weight[population_asian == 1]),
                     population_black = sum(weight[population_black == 1]),
                     population_hispanic = sum(weight[population_hispanic == 1]),
                     population_other_or_unknown = sum(weight[population_other_or_unknown == 1]),
                     population_white = sum(weight[population_white == 1]),

                     # Race/ethnicity adult population
                     population_adult_american_indian = sum(weight[population_adult_american_indian == 1]),
                     population_adult_asian = sum(weight[population_adult_asian == 1]),
                     population_adult_black = sum(weight[population_adult_black == 1]),
                     population_adult_hispanic = sum(weight[population_adult_hispanic == 1]),
                     population_adult_other_or_unknown = sum(weight[population_adult_other_or_unknown == 1]),
                     population_adult_white = sum(weight[population_adult_white == 1]),

                     # Race/ethnicity adult aged 18-65 population
                     population_aged_18_65_american_indian = sum(weight[population_aged_18_65_american_indian == 1]),
                     population_aged_18_65_asian = sum(weight[population_aged_18_65_asian == 1]),
                     population_aged_18_65_black = sum(weight[population_aged_18_65_black == 1]),
                     population_aged_18_65_hispanic = sum(weight[population_aged_18_65_hispanic == 1]),
                     population_aged_18_65_other_or_unknown = sum(weight[population_aged_18_65_other_or_unknown == 1]),
                     population_aged_18_65_white = sum(weight[population_aged_18_65_white == 1]))

  gc()
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
      }
    }

    final <- dplyr::bind_rows(final, temp)
  }
  final$population <- round(final$population)
  return(final)
}
