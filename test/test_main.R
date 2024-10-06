source("src/main.R")

test_1 <- function() {
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

test_2 <- function() {
  # test get all years in range
  years <- c(2010:2022)
  variable_list <- c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL")
  geography_level <- "state:10"
  return_data <- get_data_years(
    years=years,
    variables=variable_list,
    geography_level = geography_level
  )
  return (return_data)
}

# data_1 <- test_1()
# data_1$URL
# str(data_1$parsed)
# data_2 <- test_2()
# str(data_2, max.level = 2)
