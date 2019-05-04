source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/utils.R')
library(dplyr)
library(asciiSetupReader)
setwd("C:/Users/user/Dropbox/R_project/crime_data/clean_data/crosswalk")
crosswalk <- asciiSetupReader::read_ascii_setup("crosswalk2012.txt",
                                                "crosswalk2012.sps",
                                                use_clean_names = FALSE) %>%
  dplyr::select(NAME,
                ORI9) %>%
  dplyr::rename(agency = NAME,
                ori = ORI9)

batch_header <- get_batch_headers()
nibrs <- get_nibrs_offenses()
nibrs <-
  nibrs %>%
  dplyr::left_join(batch_header) %>%
  dplyr::left_join(crosswalk) %>%
  dplyr::rename(ORI = ori) %>%
  dplyr::select(agency,
                year,
                state,
                population,
                ORI,
                everything()) %>%
  dplyr::arrange(ORI,
                 desc(year)) %>%
  dplyr::filter(number_of_months_reported == 12) %>%
  dplyr::select(-number_of_months_reported,
                -numeric_state_code)

nibrs$agency <- sapply(nibrs$agency, simpleCap)
nibrs$state <- sapply(nibrs$state, simpleCap)

setwd(here::here("data/nibrs"))
make_agency_csvs(nibrs)
make_state_agency_choices(nibrs)
make_largest_agency_json(nibrs)

get_batch_headers <- function() {
  batch_header_2_columns <- c("B2003",
                              "B2005",
                              "B2009",
                              "B2013")

  batch_header_3_columns <- c("BH003",
                              "BH019",
                              "BH023",
                              "BH027",
                              "BH040",
                              "BH002")

  batch_header_final <- data.frame()
  for (year in 1991:2016) {
    if (year < 2013) {
      batch_columns <- batch_header_2_columns
      file_name_start <- "nibrs_batch_header_segment_2_"
      setwd("C:/Users/user/Dropbox/R_project/crime_data/raw_data/nibrs/batch_header_segment_2")
    } else {
      batch_columns <- batch_header_3_columns
      file_name_start <- "nibrs_batch_header_segment_3_"
      setwd("C:/Users/user/Dropbox/R_project/crime_data/raw_data/nibrs/batch_header_segment_3")
    }

    # There are no population_4 or population_5 populations (all are 0)
    batch_header <- asciiSetupReader::read_ascii_setup(
      data           = paste0(file_name_start, year, ".txt"),
      setup_file     = paste0(file_name_start, year, ".sps"),
      select_columns = batch_columns) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::rename(ori = originating_agency_identifier) %>%
      dplyr::mutate(population = rowSums(select(.,
                                                current_population_1,
                                                current_population_2,
                                                current_population_3),
                                         na.rm = TRUE),
                    year = year) %>%
      dplyr::select(-current_population_1,
                    -current_population_2,
                    -current_population_3)

    if (year < 2013) {
      setwd("C:/Users/user/Dropbox/R_project/crime_data/raw_data/nibrs/batch_header_segment_3")
      batch_header_temp <- asciiSetupReader::read_ascii_setup(
        data           = paste0("nibrs_batch_header_segment_3_", year, ".txt"),
        setup_file     = paste0("nibrs_batch_header_segment_3_", year, ".sps"),
        select_columns = c("B3003",
                           "B3010",
                           "B3002")) %>%
        dplyr::rename_all(tolower) %>%
        dplyr::rename(ori = originating_agency_identifier)


      batch_header <-
        batch_header %>%
        dplyr::left_join(batch_header_temp, by = "ori")
    }

    batch_header_final <- dplyr::bind_rows(batch_header_final,
                                           batch_header)

  }

  states <- data.frame(state = state.name,
                       numeric_state_code = state.abb,
                       stringsAsFactors = FALSE)
  states$numeric_state_code[states$numeric_state_code == "NE"] <- "NB"

  batch_header_final <-
    batch_header_final %>%
    dplyr::left_join(states)
  return(batch_header_final)
}


get_nibrs_offenses <- function() {
  setwd("C:/Users/user/Dropbox/R_project/crime_data/raw_data/nibrs/offense_segment")
  library(dplyr)
  offenses <- data.frame()
  for (year in 1991:2016) {
    temp <- asciiSetupReader::read_ascii_setup(
      data           = paste0("nibrs_offense_segment_", year, ".txt"),
      setup_file     = paste0("nibrs_offense_segment_", year, ".sps"),
      select_columns = c("V2003", # ORI
                         "V2006" # UCR offense code
      )) %>%
      dplyr::rename(ori = ORIGINATING_AGENCY_IDENTIFIER,
                    offense = UCR_OFFENSE_CODE) %>%
      dplyr::mutate(offense = tolower(offense))

    temp$offense <- gsub("/| |-", "_", temp$offense)
    temp$offense <- gsub("_+", "_", temp$offense)
    temp$offense <- gsub("_\\(.*", "", temp$offense)
    temp$offense <- gsub("fondling", "fondling_child_molest", temp$offense)
    temp$offense <- gsub("forcible_", "", temp$offense)

    temp <-
      temp %>%
      fastDummies::dummy_cols(select_columns = "offense") %>%
      dplyr::select(-offense) %>%
      dplyr::group_by(ori) %>%
      dplyr::summarize_all(sum) %>%
      dplyr::mutate(year = year) %>%
      dplyr::select(ori,
                    year,
                    everything())
    names(temp) <- gsub("offense_", "", names(temp))
    offenses <- dplyr::bind_rows(offenses, temp)
    message(year); rm(temp); gc(); Sys.sleep(3)
  }

  offenses[] <- sapply(offenses, tidyr::replace_na, 0)
  offenses[2:ncol(offenses)] <- sapply(offenses[2:ncol(offenses)], as.numeric)
  return(offenses)
}



