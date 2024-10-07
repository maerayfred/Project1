source("src/main.R")

test_1_m <- function() {
  # test get a year
  year <- "2022"
  variable_list <- c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL")
  geography_level <- "state:10"
  return_data <- get_data(
    year=year,
    variables=variable_list,
    geography_level = geography_level
  )
  return (return_data)
}

test_2_m <- function() {
  # test get all years in range
  years <- 2010:2022
  variable_list <- c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL")
  geography_level <- "state:10"
  return_data <- get_data_years(
    years=years,
    variables=variable_list,
    geography_level = geography_level
  )
  return (return_data)
}

# data_1_m <- test_1_m()
# data_1_m$URL
# str(data_1_m$parsed)
# data_2_m <- test_2_m()
# str(data_2_m, max.level = 2)
