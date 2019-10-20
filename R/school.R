# https://ope.ed.gov/campussafety/#/
setwd(here::here("raw_data"))
source(here::here('R/utils.R'))

arrests    <- get_school_data(list.files(pattern = "Arrests"), "arrests")
crimes     <- get_school_data(list.files(pattern = "Criminal"), "crimes")
discipline <- get_school_data(list.files(pattern = "Disciplinary"), "discipline")
hate       <- get_school_data(list.files(pattern = "Hate"), "hate")
vawa       <- get_school_data(list.files(pattern = "VAWA"), "vawa")

school <-
  crimes %>%
  dplyr::full_join(arrests) %>%
  dplyr::full_join(discipline) %>%
  dplyr::full_join(hate) %>%
  dplyr::full_join(vawa) %>%
  dplyr::select(school_name,
                year,
                school_unique_id,
                number_of_students,
                tidyselect::matches("on_campus_[^student]"),
                tidyselect::starts_with("on_campus_student_housing_facilities"),
                tidyselect::starts_with("noncampus"),
                tidyselect::starts_with("public_property"),
                everything()) %>%
  dplyr::arrange(school_name,
                 desc(year))

temp <- school[, c("school_unique_id", "school_name")]
temp <- temp[!duplicated(temp$school_name), ]
school <- school[school$school_unique_id %in% temp$school_unique_id, ]
school <- school[, -grep("ethnicity_national_origin", names(school))]

setwd(here::here("data/school"))
school_choices <- jsonlite::toJSON(temp$school_name, pretty = FALSE)
write(school_choices, "agency_choices.json")
make_all_school_csvs(school)



make_csv_school <- function(temp) {
  school_name <- unique(temp$school_name)
  school_name <- gsub(" |:|/", "_", school_name)
  school_name <- gsub("_+", "_", school_name)
  school_name <- gsub("\\(|\\)", "", school_name)

  data.table::fwrite(temp,
                     file = paste0(school_name, ".csv"))
}

make_all_school_csvs <- function(data) {

  data <-
    data %>%
    dplyr::group_by(school_unique_id) %>%
    dplyr::group_split()
  parallel::mclapply(data, make_csv_school)
}



get_school_data <- function(files, type) {
  final <- data.frame()
  files <- files[-grep("Local_State_Police", files)]
  for (file in files) {
    data <- read.csv(file)
    names(data) <- fix_column_names(names(data))
    data <-
      data %>%
      dplyr::rename(year = survey_year,
                    school_name = institution_name,
                    number_of_students = institution_size,
                    school_unique_id = unitid) %>%
      dplyr::select(-campus_name)
    student_population <- get_population(data)
    data <-
      data %>%
      dplyr::select(-campus_id,
                    -number_of_students) %>%
      dplyr::group_by(year,
                      school_unique_id,
                      school_name) %>%
      dplyr::summarise_all(sum) %>%
      dplyr::left_join(student_population)

    data <- add_location_to_names(data, file, type)
    if (nrow(final) == 0) {
      final <- data
    } else {
      final <- full_join(final, data)
    }

  }

  return(final)
}

add_location_to_names <- function(data, file, type) {
  cols <- grep("school|year|number_of_students", names(data), invert = TRUE)
  file <- gsub("Arrests_|.*Offenses_|Disciplinary_Actions_|Hate_Crimes_|.csv",
               "", file)
  file <- tolower(file)
  names(data)[cols] <- paste(type, file, names(data)[cols], sep = "_")
  return(data)
}



get_population <- function(data) {
  data <-
    data %>%
    dplyr::select(year,
                  school_unique_id,
                  campus_id,
                  number_of_students) %>%
    dplyr::distinct(year,
                    school_unique_id,
                    .keep_all = TRUE) %>%
    dplyr::select(-campus_id)
  return(data)
}

