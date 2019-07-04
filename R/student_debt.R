source(here::here('R/utils.R'))
setwd(here::here("raw_data"))

debt <- readr::read_csv("ProgramDebt1415_1516PP.csv")
debt2 <- readr::read_csv("ProgramDebt1516_1617PP.csv")

debt <-
  debt %>%
  dplyr::bind_rows(debt2) %>%
  dplyr::select(-CREDLEV,
                -CIPCODE,
                -IPEDSCOUNT) %>%
  dplyr::rename(median_debt = DEBTMEDIAN,
                mean_debt = DEBTMEAN,
                school_name = NAME,
                school_type = TYPE,
                school_ID_num = OPEID,
                degree_type = CREDDESC,
                program = CIPDESC,
                number_people_in_cohort = COUNT,
                cohort = COHORT) %>%
  dplyr::mutate(median_debt = readr::parse_number(median_debt),
                mean_debt = readr::parse_number(mean_debt),
                number_people_in_cohort = readr::parse_number(number_people_in_cohort)) %>%
  dplyr::arrange(school_name,
                 program)

debt$school_name <- sapply(debt$school_name, simpleCap)
debt$school_type <- sapply(debt$school_type, simpleCap)
debt$degree_type <- gsub("Bachelor.*", "Bachelor's Degree", debt$degree_type)
debt$school_name <- gsub("\\| ", "", debt$school_name)
debt <- as.data.frame(debt)
head(debt)
summary(debt)

setwd(here::here("data/student_debt"))
data <- data.table::data.table(debt)
pb <- txtProgressBar(min = 0, max = length(unique(data$school_name)), style = 3)
for (i in 1:length(unique(data$school_name))) {
  selected_school = unique(data$school_name)[i]
  temp <- data[school_name %in% selected_school]
  temp <-
    temp %>%
    dplyr::arrange(program)

  school <- gsub(" |:", "_", selected_school)
  school <- gsub("/", "_", school)
  school <- gsub("_+", "_", school)
  if (any(!is.na(temp$mean_debt)) |
      any(!is.na(temp$median_debt)))  {
  readr::write_csv(temp, path = paste0(school, ".csv"))
  }
  setTxtProgressBar(pb, i)    # update progress bar
}
close(pb)