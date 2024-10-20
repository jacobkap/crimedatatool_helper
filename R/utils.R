library(groundhog)
packages <- c(
  "tidyverse",
  "data.table",
  "fastDummies",
  "splitstackshape",
  "readr",
  "here",
  "dplyr",
  "lubridate",
  "progress",
  "priceR"
)
groundhog.library(packages, "2024-10-01")

load(here("data/crosswalk_agencies.rda"))

fix_ori <- function(data) {
  data$ori[data$ori %in% "AKAST01"] <- "AKASP00"
  data$ori[data$ori %in% "MDMSP02"] <- "MD306SP"
  data$ori[data$ori %in% "CA03349"] <- "CA03378"
  data$ori[data$ori %in% "NJ00805"] <- "NJ01808"
  return(data)
}


make_state_from_abb <- function(state_abb) {
  state_crosswalk <- data.frame(
    state_name = c(
      state.name,
      "canal zone",
      "district of columbia",
      "guam",
      "guam",
      "puerto rico",
      "virgin islands",
      "nebraska",
      "bahamas",
      "american samoa"
    ),
    state_abb = c(
      state.abb,
      "CZ",
      "DC",
      "GU",
      "GM",
      "PR",
      "VI",
      "NB",
      "BD",
      "AM"
    )
  )

  state <- state_crosswalk$state_name[match(
    tolower(state_abb),
    tolower(state_crosswalk$state_abb)
  )]
  return(state)
}

# Primarily for federal agencies
fix_missing_states <- function(data) {
  data$state2 <- substr(data$ori, 1, 2)
  data$state2 <- make_state_from_abb(data$state2)
  data$state[is.na(data$state)] <- data$state2[is.na(data$state)]
  data$state <- tolower(data$state)
  data$state2 <- NULL
  return(data)
}


remove_duplicate_capitalize_names <- function(data) {
  same_name_agencies <-
    data %>%
    distinct(ORI, .keep_all = TRUE) %>%
    mutate(temp = paste(agency, state))
  same_name_agencies_name <-
    same_name_agencies %>%
    count(temp) %>%
    filter(n > 1)
  same_name_agencies_ori <-
    same_name_agencies %>%
    filter(temp %in% same_name_agencies_name$temp)
  print(summary(data$population[data$ORI %in% same_name_agencies_ori$ORI]))
  data <-
    data %>%
    filter(!ORI %in% same_name_agencies_ori$ORI) %>%
    mutate(
      agency = tolower(agency),
      agency = gsub("dept\\.|dept|dep |dep$", "department", agency),
      agency = gsub("^st ", "state", agency),
      agency = gsub(" hdq ", " headquarters ", agency),
      agency = gsub("sp:", "state police:", agency),
      agency = gsub("pd", "police", agency),
      agency = gsub(" univ ", " university ", agency),
      agency = gsub(" co$| co ", " county", agency),
      agency = gsub(" offi$", " office", agency),
      agency = gsub("twp", "township", agency),
      agency = gsub("div ", "division ", agency),
      agency = gsub("ptrl:", "patrol:", agency),
      agency = gsub("bf:", "bureau of forestry:", agency),
      agency = gsub("hp:", "highway patrol:", agency),
      agency = gsub("chp ", "california highway patrol:", agency),
      agency = gsub("bn:", "office of attorney general region:", agency),
      agency = gsub("dle:", "division of law enforcement:", agency),
      agency = gsub("enf:|enf ", "enforcement", agency),
      agency = gsub("law enf div dept natrl resources", "department of natural resources", agency),
      agency = gsub("fl ", "florida", agency),
      agency = gsub("dnr:", "department of natural resources:", agency),
      agency = gsub("co crim law enf", "county criminal law enforcement", agency),
      agency = gsub("uppr:", "union pacific railroad:", agency),
      agency = str_to_title(agency),
      state = str_to_title(state),
      agency = gsub("U S ", "US ", agency),
      agency = gsub("^Us ", "US ", agency, ignore.case = TRUE),
      agency = gsub(" Atf ", " ATF ", agency),
      agency = gsub("Fbi,", "FBI,", agency),
      agency = gsub("Dea,", "DEA,", agency),
      agency = gsub("Doi:", " DOI:", agency),
      state = gsub(" Of ", " of ", state),
      agency = trimws(agency),
      state = trimws(state)
    )


  return(data)
}

remove_duplicate_capitalize_names <- function(data) {
  same_name_agencies <-
    data %>%
    distinct(ORI, .keep_all = TRUE) %>%
    mutate(temp = paste(agency, state))
  same_name_agencies_name <-
    same_name_agencies %>%
    count(temp) %>%
    filter(n > 1)
  same_name_agencies_ori <-
    same_name_agencies %>%
    filter(temp %in% same_name_agencies_name$temp)
  print(summary(data$population[data$ORI %in% same_name_agencies_ori$ORI]))
  data <-
    data %>%
    filter(!ORI %in% same_name_agencies_ori$ORI) %>%
    mutate(
      agency = tolower(agency),
      agency = gsub("dept\\.|dept|dep |dep$", "department", agency),
      agency = gsub("sp:", "state police:", agency),
      agency = gsub("pd", "police", agency),
      agency = gsub(" univ ", " university ", agency),
      agency = gsub(" co$| co ", " county", agency),
      agency = gsub(" offi$", " office", agency),
      agency = gsub("twp", "township", agency),
      agency = gsub("div ", "division ", agency),
      agency = gsub("ptrl:", "patrol:", agency),
      agency = gsub("bf:", "bureau of forestry:", agency),
      agency = gsub("hp:", "highway patrol:", agency),
      agency = gsub("chp ", "california highway patrol:", agency),
      agency = gsub("bn:", "office of attorney general region:", agency),
      agency = gsub("dle:", "division of law enforcement:", agency),
      agency = gsub("enf:|enf ", "enforcement", agency),
      agency = gsub("law enf div dept natrl resources", "department of natural resources", agency),
      agency = gsub("fl ", "florida", agency),
      agency = gsub("dnr:", "department of natural resources:", agency),
      agency = gsub("co crim law enf", "county criminal law enforcement", agency),
      agency = gsub("uppr:", "union pacific railroad:", agency),
      agency = str_to_title(agency),
      state = str_to_title(state),
      agency = gsub("U S ", "US ", agency),
      agency = gsub("^Us ", "US ", agency, ignore.case = TRUE),
      agency = gsub(" Atf ", " ATF ", agency),
      agency = gsub("Fbi,", "FBI,", agency),
      agency = gsub("Dea,", "DEA,", agency),
      agency = gsub("Doi:", " DOI:", agency),
      state = gsub(" Of ", " of ", state),
      agency = trimws(agency),
      state = trimws(state)
    )


  return(data)
}

keep_most_common_agency_name <- function(data) {
  temp <-
    data %>%
    distinct(
      ori,
      agency_name
    ) %>%
    count(ori) %>%
    filter(n > 1)
  temp <-
    data %>%
    filter(ori %in% temp$ori)
  new_names <- data.frame(
    ori = unique(temp$ori),
    new_name = NA
  )
  pb <- progress_bar$new(
    format = " [:bar] :percent eta: :eta",
    total = nrow(new_names), clear = FALSE, width = 60
  )
  for (i in 1:nrow(new_names)) {
    temp_ori <-
      temp %>%
      filter(ori %in% new_names$ori[i]) %>%
      count(agency_name) %>%
      arrange(desc(n))
    new_names$new_name[i] <- temp_ori$agency_name[1]
    pb$tick()
  }
  data <-
    data %>%
    left_join(new_names)
  data$agency_name[!is.na(data$new_name)] <- data$new_name[!is.na(data$new_name)]
  data$new_name <- NULL
  return(data)
}

make_state_agency_choices <- function(data) {
  data <- data.table::as.data.table(data)
  for (selected_state in unique(data$state)) {
    temp <- data[state %in% selected_state]
    agency <- unique(temp$agency)

    agency <- jsonlite::toJSON(agency, pretty = FALSE)
    write(agency, paste0(selected_state, "_agency_choices.json"))
  }
}


make_largest_agency_json <- function(data) {
  largest_agency <- data %>%
    dplyr::group_by(state) %>%
    dplyr::top_n(1, population) %>%
    dplyr::select(state, agency)
  largest_agency <- jsonlite::toJSON(largest_agency, pretty = TRUE)
  write(largest_agency, "largest_agency_choices.json")
}

reorder_police <- function(data, crosswalk_data) {
  employee_cols <- sort(grep("employee", names(data),
    value = TRUE
  ))
  killed_cols <- sort(grep("killed", names(data),
    value = TRUE
  ))
  injury_cols <- sort(grep("with_injury", names(data),
    value = TRUE
  ))
  injury_cols <- injury_cols[!grepl("indicator", injury_cols)]
  no_injury_cols <- sort(grep("no_injury", names(data),
    value = TRUE
  ))
  no_injury_cols <- no_injury_cols[!grepl("indicator", no_injury_cols)]
  total_assault_cols <- sort(grep("^total_assault", names(data),
    value = TRUE
  ))
  total_assault_cols <- total_assault_cols[!grepl("clear|traffic", total_assault_cols)]
  assaults <- sort(grep("_assault_|.total_assaults", names(data),
    value = TRUE
  ))
  assaults <- assaults[!grepl("^total_assault|time|all_other", assaults)]
  all_other_assaults <- sort(grep("all_other_assault", names(data),
    value = TRUE
  ))
  all_other_assaults <- all_other_assaults[!grepl("clear", all_other_assaults)]

  data <-
    data %>%
    dplyr::filter(!state %in% c(
      "guam",
      "canal zone",
      "puerto rico",
      "virgin islands"
    )) %>%
    dplyr::left_join(crosswalk_data, by = "ori") %>%
    dplyr::filter(agency != "NANA") %>%
    dplyr::rename(ORI = ori) %>%
    dplyr::select(
      all_of(starting_cols),
      number_of_months_reported,
      all_of(employee_cols),
      all_of(killed_cols),
      all_of(injury_cols),
      all_of(no_injury_cols),
      all_of(assaults),
      all_of(all_other_assaults),
      all_of(total_assault_cols)
    )
  return(data)
}





make_agency_csvs <- function(data,
                             type = "year") {
  data <-
    data %>%
    dplyr::group_split(ORI)


  parallel::mclapply(data,
    make_csv_test,
    type = type
  )
}

make_csv_test <- function(temp, type) {
  temp <- dummy_rows_missing_years(temp, type = type)


  state <- unique(temp$state)
  agency <- unique(temp$agency)
  state <- gsub(" ", "_", state)
  agency <- gsub(" |:", "_", agency)
  agency <- gsub("/", "_", agency)
  agency <- gsub("_+", "_", agency)
  agency <- gsub("\\(|\\)", "", agency)


  data.table::fwrite(temp, file = paste0(state, "_", agency, ".csv"))
}

save_monthly_state_temp <- function(data, start_year, type) {
  setwd(here("data/temp"))
  states <- c(tolower(state.name), "district of columbia")

  state_name <- c(state.name, "District of Columbia")
  state_abb <- c(state.abb, "DC")
  for (state_group in 1:length(states)) {
    selected_states <- states[state_group]
    if (year != start_year) {
      load(paste0("monthly_", type, "_state_group_", state_group, ".rda"))
      temp_state <- dplyr::bind_rows(
        temp_state,
        data[tolower(data$state) %in% selected_states, ]
      )
    } else {
      temp_state <- data[tolower(data$state) %in% selected_states, ]
    }
    save(temp_state, file = paste0("monthly_", type, "_state_group_", state_group, ".rda"))
    rm(temp_state)
    gc()
  }
}


make_all_na <- function(col) {
  col <- NA
}

dummy_rows_missing_years <- function(data, type) {
  if (type == "year") {
    missing_years <- min(data$year):max(data$year)
  } else {
    missing_years <- seq.Date(lubridate::ymd(min(data$year)),
      lubridate::ymd(max(data$year)),
      by = "month"
    )
    missing_years <- as.character(missing_years)
  }

  missing_years <- missing_years[!missing_years %in% data$year]

  if (length(missing_years) > 0) {
    temp <- data
    temp <- temp[1, ]
    temp <- splitstackshape::expandRows(temp,
      count = length(missing_years),
      count.is.col = FALSE
    )
    temp$year <- missing_years
    temp <-
      temp %>%
      dplyr::mutate_at(
        vars(-one_of("year", "agency", "state", "ORI")),
        make_all_na
      )

    data <-
      data %>%
      dplyr::bind_rows(temp) %>%
      dplyr::arrange(desc(year)) %>%
      dplyr::mutate(year = as.character(year))
  }


  return(data)
}


hate_crimes_offenses_fix <- c(
  "^arson$" = "arson",
  "^assault offenses - aggravated assault$" = "assault",
  "^assault offenses - intimidation$" = "assault",
  "^assault offenses - simple assault$" = "assault",
  "^bribery$" = "bribery",
  "^burglary/breaking and entering$" = "burglary/breaking and entering",
  "^counterfeiting/forgery$" = "fraud",
  "^destruction/damage/vandalism of property$" = "fraud",
  "^drug/narcotic offenses - drug equipment violations$" = "drug offense",
  "^drug/narcotic offenses - drug/narcotic violations$" = "drug offense",
  "^embezzlement$" = "fraud",
  "^extortion/blackmail$" = "extortion/blackmail",
  "^fraud offenses - credit card/atm fraud$" = "fraud",
  "^fraud offenses - false pretenses/swindle/confidence game$" = "fraud",
  "^fraud offenses - impersonation$" = "fraud",
  "^fraud offenses - other$" = "fraud",
  "^fraud offenses - welfare fraud$" = "fraud",
  "^fraud offenses - wire fraud$" = "fraud",
  "^gambling offenses - betting/wagering$" = "gambling offenses",
  "^human trafficking - commercial sex acts$" = "human trafficking",
  "^human trafficking - involuntary servitude$" = "human trafficking",
  "^kidnapping/abduction$" = "kidnapping/abduction",
  "^larceny/theft offenses - all other larceny$" = "theft",
  "^larceny/theft offenses - other$" = "theft",
  "^larceny/theft offenses - pocket-picking$" = "theft",
  "^larceny/theft offenses - purse-snatching$" = "theft",
  "^larceny/theft offenses - shoplifting$" = "theft",
  "^larceny/theft offenses - theft from building$" = "theft",
  "^larceny/theft offenses - theft from motor vehicle$" = "theft",
  "^larceny/theft offenses - theft of motor vehicle parts/accessories$" = "theft",
  "^larceny/theft offenses - theft rom coin-operated machine or device$" = "theft",
  "^motor vehicle theft$" = "motor vehicle theft",
  "^murder/nonnegligent manslaughter$" = "murder/nonnegligent manslaughter",
  "^negligent manslaughter$" = "negligent manslaughter",
  "^pornography/obscene material$" = "pornography/obscene material",
  "^prostitution offenses - assisting or promoting prostitution$" = "prostitution",
  "^prostitution offenses - prostitution$" = "prostitution",
  "^prostitution offenses - purchasing prostitution$" = "prostitution",
  "^robbery$" = "robbery",
  "^sex offenses - fondling - indecent liberties/child molest$" = "sex offenses",
  "^sex offenses - incest$" = "sex offenses",
  "^sex offenses - rape$" = "sex offenses",
  "^sex offenses - sexual assault with an object$" = "sex offenses",
  "^sex offenses - sodomy$" = "sex offenses",
  "^sex offenses - statutory rape$" = "sex offenses",
  "^stolen property offenses \\(receiving, selling, etc.\\)$" = "stolen property offenses",
  "^weapon law violations$" = "weapon law violations"
)


starting_cols <- c(
  "agency",
  "year",
  "state",
  "population",
  "ORI"
)
