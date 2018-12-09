source('C:/Users/user/Dropbox/R_project/crimedatatool_helper/R/national_corrections_utils.R')

admissions   <- clean_and_agg_data("corrections_prison_admissions_1991_2016",
                                   type = "admissions")
releases     <- clean_and_agg_data("corrections_prison_releases_1991_2016",
                                   type = "releases")
prisoners    <- clean_and_agg_data("corrections_year_end_pop_1991_2016",
                                   type = "prisoners")

setwd("C:/Users/user/Dropbox/R_project/crimedatatool_helper/data/prisoners")
save_as_CSVs(admissions, "admissions")
save_as_CSVs(releases,   "releases")
save_as_CSVs(prisoners,  "custody")