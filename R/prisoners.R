setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/raw_data")
source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/utils.R')
source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/prisoners_utils.R')
library(dplyr)
library(asciiSetupReader)

load("prisoners_census.rda")
prisoners <-
  spss_ascii_reader("national_prisoner_statistics_1978_2016.txt",
                    "national_prisoner_statistics_1978_2016.sps") %>%
  dplyr::rename_all(tolower)


prisoners <-
  spss_ascii_reader("national_prisoner_statistics_1978_2016.txt",
                    "national_prisoner_statistics_1978_2016.sps") %>%
  dplyr::rename_all(tolower) %>%
  dplyr::select(survey_year,
                state_fips_identification_code,
                # Number under custody
                custody_unsentenced_males,
                custody_unsentenced_females,
                total_under_custody_males,
                total_under_custody_females,
                custody_unsentenced_total_1978_1982_only,
                total_under_custody_total_1978_1982_only,
                custody_excluding_private_facilities_male,
                custody_excluding_private_facilities_female,
                custody_including_private_facilities_male,
                custody_including_private_facilities_female,
                # Number under jurisdiction
                jurisdiction_unsentenced_males,
                jurisdiction_unsentenced_females,
                total_under_jurisdiction_males,
                total_under_jurisdiction_females,
                inmates_under_jurisdiction_housed_in_privately_operated_correctional_facility_in_state_male,
                inmates_under_jurisdiction_housed_in_privately_operated_correctional_facility_in_state_female,
                inmates_under_jurisdiction_housed_in_privately_operated_correctional_facility_in_other_state_male,
                inmates_under_jurisdiction_housed_in_privately_operated_correctional_facility_in_other_state_female,
                inmates_under_jurisdiction_housed_in_local_facilities_operated_by_county_or_other_local_authority_male,
                inmates_under_jurisdiction_housed_in_local_facilities_operated_by_county_or_other_local_authority_female,
                local_facilities_solely_to_ease_prison_crowding_male,
                local_facilities_solely_to_ease_prison_crowding_female,

                # Race/ethnicity
                white_male,
                white_female,
                black_male,
                black_female,
                hispanic_or_latino_male,
                hispanic_or_latino_female,
                american_indian_or_alaska_native_male,
                american_indian_or_alaska_native_female,
                native_hawaiian_or_other_pacific_islander_male,
                native_hawaiian_or_other_pacific_islander_female,
                asian_male,
                asian_female,
                asian_or_pacific_islander_male,
                asian_or_pacific_islander_female,
                additional_other_categories_for_race_male,
                additional_other_categories_for_race_female,
                unknown_race_male,
                unknown_race_female,

                # Admissions
                new_court_commitments_male,
                new_court_commitments_female,
                parole_violators_with_new_sentence_male,
                parole_violators_with_new_sentence_female,
                parole_violators_without_new_sentence_male,
                parole_violators_without_new_sentence_female,
                other_conditional_release_violators_admitted_with_new_sentence_male,
                other_conditional_release_violators_admitted_with_new_sentence_female,
                other_conditional_release_violators_admitted_without_new_sentence_male,
                other_conditional_release_violators_admitted_without_new_sentence_female,
                transfers_admitted_from_other_jurisdictions_male,
                transfers_admitted_from_other_jurisdictions_female,
                awol_returns_with_or_without_new_sentences_male,
                awol_returns_with_or_without_new_sentences_female,
                escapee_returns_with_or_without_new_sentences_male,
                escapee_returns_with_or_without_new_sentences_female,
                returns_from_appeal_or_bond_male,
                returns_from_appeal_or_bond_female,
                other_admissions_male,
                other_admissions_female,
                total_admissions_male,
                total_admissions_female,

                # Releases
                unconditional_release_expirations_of_sentence_male,
                unconditional_release_expirations_of_sentence_female,
                unconditional_release_commutations_male,
                unconditional_release_commutations_female,
                other_unconditional_release_male,
                other_unconditional_release_female,
                conditional_release_probations_male,
                conditional_release_probations_female,
                supervised_mandatory_release_male,
                supervised_mandatory_release_female,
                discretionary_parole_male,
                discretionary_parole_female,
                other_conditional_release_male,
                other_conditional_release_female,
                awol_release_male,
                awol_release_female,
                transfers_to_other_jurisdictions_male,
                transfers_to_other_jurisdictions_female,
                release_to_appeal_or_bond_male,
                release_to_appeal_or_bond_female,
                escape_from_confinement_male,
                escape_from_confinement_female,
                total_releases_male,
                total_releases_female,

                # Capacity
                rated_capacity_male,
                rated_capacity_female,
                rated_capacity_total,
                operational_capacity_male,
                operational_capacity_female,
                operational_capacity_total,
                design_capacity_male,
                design_capacity_female,
                design_capacity_total,

                # Minors and non-citizens
                in_custody_under_18_years_of_age_male,
                in_custody_under_18_years_of_age_female,
                in_custody_not_u_s_citizens_male,
                in_custody_not_u_s_citizens_female,

                # Death
                deaths_from_execution_male,
                deaths_from_execution_female,
                deaths_from_illness_or_natural_cause_male,
                deaths_from_illness_or_natural_cause_female,
                deaths_from_aids_male,
                deaths_from_aids_female,
                deaths_from_suicide_male,
                deaths_from_suicide_female,
                deaths_from_accidental_injury_to_self_male,
                deaths_from_accidental_injury_to_self_female,
                deaths_from_homicide_by_other_inmates_male,
                deaths_from_homicide_by_other_inmates_female,
                deaths_from_other_homicide_male,
                deaths_from_other_homicide_female,
                deaths_caused_by_another_person_male,
                deaths_caused_by_another_person_female,
                total_deaths_male,
                total_deaths_female,

                # HIV/AIDS
                asymptomatic_hiv_positive_male,
                asymptomatic_hiv_positive_female,
                asymptomatic_hiv_positive_total,
                infected_with_lesser_forms_of_symptomatic_hiv_disease_male,
                infected_with_lesser_forms_of_symptomatic_hiv_disease_female,
                infected_with_lesser_forms_of_symptomatic_hiv_disease_total,
                confirmed_to_have_aids_male,
                confirmed_to_have_aids_female,
                confirmed_to_have_aids_total,
                total_in_custody_hiv_positive_or_with_aids_male,
                total_in_custody_hiv_positive_or_with_aids_female,
                total_in_custody_hiv_positive_or_with_aids_total) %>%
  dplyr::rename(year = survey_year,
                state = state_fips_identification_code) %>%
  dplyr::mutate_at(vars(3:ncol(.)), make_numeric) %>%
  dplyr::rename_all(funs(stringr::str_replace_all(., prisoners_name_fix))) %>%
  dplyr::mutate(state = gsub("[0-9]+. ", "", state),
                state = gsub("Federal BOP", "Federal Prison Total", state),
                state = gsub("^US prison total.*", "US Prison Total", state),
                state = gsub("^State prison total", "State Prison Total", state),
                # Private Prison custody
                custody_private_prison_male = custody_including_private_facilities_male -
                  custody_public_prisons_male,
                custody_private_prison_female = custody_including_private_facilities_female -
                  custody_public_prisons_female,

                # Private/public Prison jurisdiction
                jurisdiction_private_prison_male = jurisdiction_private_prison_in_state_male +
                  jurisdiction_private_prison_out_of_state_male,
                jurisdiction_private_prison_female = jurisdiction_private_prison_in_state_female +
                  jurisdiction_private_prison_out_of_state_female,

                jurisdiction_public_prison_male = total_under_jurisdiction_males -
                  jurisdiction_private_prison_male,
                jurisdiction_public_prison_female = total_under_jurisdiction_females -
                  jurisdiction_private_prison_female) %>%
  dplyr::mutate(
    # Custody total columns
    custody_unsentenced_total =
      rowSums(.[, grepl("^custody_unsentenced_[a-z]+$",
                        names(.))]),
    total_under_custody_total =
      rowSums(.[, grepl("^total_under_custody_[a-z]+$",
                        names(.))]),
    custody_public_prisons_total =
      rowSums(.[, grepl("^custody_public_prisons_",
                        names(.))]),
    custody_private_prison_total =
      rowSums(.[, grepl("^custody_private_prison_",
                        names(.))]),


    # Jurisdiction total columns
    jurisdiction_unsentenced_total =
      rowSums(.[, grepl("^jurisdiction_unsentenced_",
                        names(.))]),
    total_under_jurisdiction_total =
      rowSums(.[, grepl("^total_under_jurisdiction_",
                        names(.))]),

    jurisdiction_private_prison_out_of_state_total =
      rowSums(.[, grepl("^jurisdiction_private_prison_out_of_state_",
                        names(.))]),
    jurisdiction_private_prison_in_state_total =
      rowSums(.[, grepl("^jurisdiction_private_prison_in_state_",
                        names(.))]),
    jurisdiction_private_prison_total =
      rowSums(.[, c("jurisdiction_private_prison_male",
                    "jurisdiction_private_prison_female")]),

    jurisdiction_local_facilities_solely_to_ease_prison_crowding_total =
      rowSums(.[, grepl("^jurisdiction_local_facilities_solely_to_ease_prison_crowding_",
                        names(.))]),
    jurisdiction_housed_in_local_facility_total =
      rowSums(.[, grepl("^jurisdiction_housed_in_local_facility_",
                        names(.))]),
    jurisdiction_public_prison_total =
      rowSums(.[,  c("jurisdiction_public_prison_male",
                     "jurisdiction_public_prison_female")]),


    # Admissions total columns
    new_court_commitments_total =
      rowSums(.[, grepl("^new_court_commitments",
                        names(.))]),
    parole_violators_with_new_sentence_total =
      rowSums(.[, grepl("^parole_violators_with_new_sentence_",
                        names(.))]),
    parole_violators_without_new_sentence_total =
      rowSums(.[, grepl("^parole_violators_without_new_sentence_",
                        names(.))]),
    other_conditional_release_violators_admitted_with_new_sentence_total =
      rowSums(.[, grepl("^other_conditional_release_violators_admitted_with_new_sentence_",
                        names(.))]),
    other_conditional_release_violators_admitted_without_new_sentence_total =
      rowSums(.[, grepl("^other_conditional_release_violators_admitted_without_new_sentence_",
                        names(.))]),
    transfers_admitted_from_other_jurisdictions_total =
      rowSums(.[, grepl("^transfers_admitted_from_other_jurisdictions_",
                        names(.))]),

    awol_returns_with_or_without_new_sentences_total =
      rowSums(.[, grepl("^awol_returns_with_or_without_new_sentences_",
                        names(.))]),
    escapee_returns_with_or_without_new_sentences_total =
      rowSums(.[, grepl("^escapee_returns_with_or_without_new_sentences_",
                        names(.))]),
    returns_from_appeal_or_bond_total =
      rowSums(.[, grepl("^returns_from_appeal_or_bond_",
                        names(.))]),
    other_admissions_total =
      rowSums(.[, grepl("^other_admissions_",
                        names(.))]),
    total_admissions_total =
      rowSums(.[, grepl("^total_admissions_",
                        names(.))]),

    # Releases total columns
    unconditional_release_expirations_of_sentence_total =
      rowSums(.[, grepl("^unconditional_release_expirations_of_sentence_",
                        names(.))]),
    unconditional_release_commutations_total =
      rowSums(.[, grepl("^unconditional_release_commutations_",
                        names(.))]),
    other_unconditional_release_total =
      rowSums(.[, grepl("^other_unconditional_release_",
                        names(.))]),
    conditional_release_probations_total =
      rowSums(.[, grepl("^conditional_release_probations_",
                        names(.))]),
    supervised_mandatory_release_total =
      rowSums(.[, grepl("^supervised_mandatory_release_",
                        names(.))]),
    discretionary_parole_total =
      rowSums(.[, grepl("^discretionary_parole_",
                        names(.))]),
    other_conditional_release_total =
      rowSums(.[, grepl("^other_conditional_release_[a-z]+$",
                        names(.))]),
    awol_release_total =
      rowSums(.[, grepl("^awol_release_",
                        names(.))]),

    transfers_to_other_jurisdictions_total =
      rowSums(.[, grepl("^transfers_to_other_jurisdictions_",
                        names(.))]),
    release_to_appeal_or_bond_total =
      rowSums(.[, grepl("^release_to_appeal_or_bond_",
                        names(.))]),
    escape_from_confinement_total =
      rowSums(.[, grepl("^escape_from_confinement_",
                        names(.))]),
    total_releases_total =
      rowSums(.[, grepl("^total_releases_",
                        names(.))]),



    # Make total race columns
    white_total =
      rowSums(.[, grepl("^white",
                        names(.))]),
    black_total =
      rowSums(.[, grepl("^black",
                        names(.))]),
    hispanic_or_latino_total =
      rowSums(.[, grepl("^hispanic_or_latino",
                        names(.))]),
    american_indian_total =
      rowSums(.[, grepl("^american_indian",
                        names(.))]),
    asian_total =
      rowSums(.[, grepl("asian_male|asian_female|native_hawaiian",
                        names(.))]),
    asian_or_pacific_islander_total =
      rowSums(.[, grepl("^asian_or_pacific",
                        names(.))]),
    other_race_total =
      rowSums(.[, grepl("^other_race",
                        names(.))]),
    unknown_race_total =
      rowSums(.[, grepl("^unknown_race",
                        names(.))]),


    # Minors and non-citizens total
    in_custody_under_18_years_of_age_total =
      rowSums(.[, grepl("^in_custody_under_18_years_of_age_",
                        names(.))]),
    in_custody_not_us_citizens_total =
      rowSums(.[, grepl("^in_custody_not_us_citizens_",
                        names(.))]),

    # Death total
    deaths_from_execution_total =
      rowSums(.[, grepl("^deaths_from_execution_",
                        names(.))]),
    deaths_from_illness_or_natural_cause_total =
      rowSums(.[, grepl("^deaths_from_illness_or_natural_cause",
                        names(.))]),
    deaths_from_aids_total =
      rowSums(.[, grepl("^deaths_from_aids_",
                        names(.))]),
    deaths_from_suicide_total =
      rowSums(.[, grepl("^deaths_from_suicide_",
                        names(.))]),
    deaths_from_accidental_injury_to_self_total =
      rowSums(.[, grepl("^deaths_from_accidental_injury_to_self_",
                        names(.))]),
    deaths_from_homicide_by_other_inmates_total =
      rowSums(.[, grepl("^deaths_from_homicide_by_other_inmates_",
                        names(.))]),
    deaths_from_other_homicide_total =
      rowSums(.[, grepl("^deaths_from_other_homicide_",
                        names(.))]),
    deaths_caused_by_another_person_total =
      rowSums(.[, grepl("^deaths_caused_by_another_person_",
                        names(.))]),
    total_deaths_total =
      rowSums(.[, grepl("^total_deaths_",
                        names(.))])
  ) %>%
  dplyr::left_join(prisoners_census)
prisoners$total_under_custody_total[is.na(prisoners$total_under_custody_total)] <-
  prisoners$total_under_custody_total_1978_1982_only[is.na(prisoners$total_under_custody_total)]
prisoners$custody_unsentenced_total[is.na(prisoners$custody_unsentenced_total)] <-
  prisoners$custody_unsentenced_total_1978_1982_only[is.na(prisoners$custody_unsentenced_total)]
names(prisoners) <- gsub("males", "male", names(prisoners))

# No pre-1999 capacity info for male/female and no total post-1998
prisoners$rated_capacity_total[prisoners$year >= 1999] <-
  prisoners$rated_capacity_female[prisoners$year >= 1999] +
  prisoners$rated_capacity_male[prisoners$year >= 1999]

prisoners$design_capacity_total[prisoners$year >= 1999] <-
  prisoners$design_capacity_female[prisoners$year >= 1999] +
  prisoners$design_capacity_male[prisoners$year >= 1999]

prisoners$operational_capacity_total[prisoners$year >= 1999] <-
  prisoners$operational_capacity_female[prisoners$year >= 1999] +
  prisoners$operational_capacity_male[prisoners$year >= 1999]


# Fix asian column - pre-1997 columns were asian_or_pacific_islander
prisoners$asian_female[prisoners$year < 1997] <- prisoners$asian_or_pacific_islander_female[prisoners$year < 1997]
prisoners$asian_male[prisoners$year < 1997]   <- prisoners$asian_or_pacific_islander_male[prisoners$year < 1997]
prisoners$asian_total[prisoners$year < 1997]  <- prisoners$asian_or_pacific_islander_total[prisoners$year < 1997]

prisoners$asian_female[prisoners$year >= 1997] <- rowSums(prisoners[prisoners$year >= 1997, c("asian_female",
                                                                                              "native_hawaiian_female")])
prisoners$asian_male[prisoners$year >= 1997]  <- rowSums(prisoners[prisoners$year >= 1997, c("asian_male",
                                                                                             "native_hawaiian_male")])

# Fix other and unknown columns - name changed in 1999
prisoners$other_race_female[prisoners$year < 1999] <- prisoners$unknown_race_female[prisoners$year < 1999]
prisoners$other_race_male[prisoners$year < 1999]   <- prisoners$unknown_race_male[prisoners$year < 1999]
prisoners$other_race_total[prisoners$year < 1999]  <- prisoners$unknown_race_total[prisoners$year < 1999]

# A few years (1995-1999) shoots up to 40-50k (10 times the previous or
# following years) which is far higher than any year-over-year growth in
# prisoners (about 10x as many as number of prisoner growth in total US).
# So I consider these data issues and making them NA.

# prisoners$other_race_female[prisoners$year %in% 1995:1999] <- NA
# prisoners$other_race_male[prisoners$year   %in% 1995:1999] <- NA
# prisoners$other_race_total[prisoners$year  %in% 1995:1999] <- NA


prisoners <-
  prisoners %>%
  dplyr::rename(other_or_unknown_race_female = other_race_female,
                other_or_unknown_race_male   = other_race_male,
                other_or_unknown_race_total  = other_race_total) %>%
  dplyr::select(-asian_or_pacific_islander_female,
                -asian_or_pacific_islander_male,
                -asian_or_pacific_islander_total,
                -native_hawaiian_female,
                -native_hawaiian_male,
                -unknown_race_female,
                -unknown_race_male,
                -unknown_race_total,
                -total_under_custody_total_1978_1982_only,
                -custody_unsentenced_total_1978_1982_only)

# dim(prisoners)
# summary(prisoners)
# sapply(prisoners, max, na.rm = TRUE)
# names(prisoners)
# sort(unique(prisoners$state))


setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/prisoners")
for (selected_state in sort(unique(prisoners$state))) {
  for (i in 1:length(prisoners_categories)) {
    cols_to_keep <- prisoners_categories[[i]]

    if (names(prisoners_categories)[i] == "race_ethnicity") {
      pop_cols <- grep("population", names(prisoners), value = TRUE)
    } else {
      pop_cols <- grep("male$|65$|adult$|population$", names(prisoners), value = TRUE)
    }

    temp <-
      prisoners %>%
      dplyr::filter(state %in% selected_state) %>%
      dplyr::select(state,
                    year,
                    pop_cols,
                    cols_to_keep) %>%
      dplyr::arrange(desc(year))

    save_state     <- unique(temp$state)
    save_state     <- gsub(" ", "_", save_state)
    category       <- names(prisoners_categories)[i]

    if (category == "aids") {
      temp <- temp[temp$year > 1990, ]
    } else if (category == "capacity") {
      temp <- temp[temp$year > 1982, ]
    } else if (category == "noncitizen_juvenile") {
      temp <- temp[temp$year > 1999, ]
    }
    temp$year <- as.character(temp$year)

    readr::write_csv(temp,
                     path = paste0(save_state, "_", category, "_prisoners.csv"))
  }
}
