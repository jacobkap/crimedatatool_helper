
# Load scripts ------------------------------------------------------------
source(here::here("R/utils.R"))
source(here::here("R/crosswalk.R"))
source(here::here("R/offenses_prep.R"))
source(here::here("R/leoka_prep.R"))
source(here::here("R/hate_crimes_prep.R"))
source(here::here("R/property_stolen_prep.R"))
source(here::here("R/arson_prep.R"))
source(here::here("R/arrests_prep.R"))

# Offenses ---------------
get_offenses_data("year", crosswalk_agencies)
get_offenses_data("month", crosswalk_agencies)

# Leoka ---------------------------
get_leoka_data("year", crosswalk_agencies)
get_leoka_data("month", crosswalk_agencies)

# Hate crimes ------------------
get_hate_crimes_data("year")
get_hate_crimes_data("month")

# Arson -------------------------------------------------------------------
get_arson_data("year", crosswalk_agencies)
get_arson_data("month", crosswalk_agencies)

# Property Stolen and Recovered -------------------------------------------
get_property_stolen_data("year", crosswalk_agencies)
get_property_stolen_data("month", crosswalk_agencies)

# Arrest -------------------------------------------
get_arrest_data("year", crosswalk_agencies)
get_arrest_data("month", crosswalk_agencies)

