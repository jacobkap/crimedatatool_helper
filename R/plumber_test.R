load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/offenses_known/ucr_offenses_known_yearly_1960_2016.rda")
ucr_offenses_known_yearly_1960_2016 <- data.table::as.data.table(ucr_offenses_known_yearly_1960_2016)
#' Get UCR data
#' @param ORI subset UCR data
#' @get /
function(ORI){

  # Filter if the species was specified
    myData <- ucr_offenses_known_yearly_1960_2016[ori %in% ORI]

}