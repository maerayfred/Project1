library(tidyverse)
library(tidycensus)
library(httr)
library(jsonlite)
library(glue)


build_url <- function(year="2022", get_vals=c("AGEP", "PWGTP", "SEX"), for_val="state:10", schl_val="16") {
  BASE_URL <- glue("https://api.census.gov/data/{year}/acs/acs1/pums")
  get_vals_csv <- paste(get_vals, collapse=",")
  get_str <- glue("get={get_vals_csv}")
  for_str <- glue("for={for_val}")
  schl_str <- glue("SCHL={schl_val}")
  suffix <- paste(get_str, for_str, schl_str, sep="&")

  return(glue("{BASE_URL}?{suffix}"))
}

get_data <- function(year="2022", variables=c("AGEP", "PWGTP", "SEX")) {
  if (!("PWGTP" %in% variables)) {
    variables <- c(variables, "PWGTP")
  }

  options <- c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL")
  if (!all(variables %in% options)) {
    stop("Invalid variable name")
  }

  URL_VARIABLES <- "https://api.census.gov/data/2022/acs/acs1/pums/variables.json"
  var_info <- httr::GET(URL_VARIABLES)
  var_info_parsed <- fromJSON(rawToChar(var_info$content))
  var_info_tibble <- as_tibble(var_info_parsed)

  var_data <- c()
  for (var in options) {
    var_data <- c(var_data, var_info_tibble$variables[var])
  }

  # PWGTP always included
  # AGEP as default, at least 1 numeric variable needs to be returned aside from PWGTP

  # SEX is default categorical variable
  # at least one categorical variable needs to be returned

  URL <- build_url(year=year, get_vals=variables)
  id_info <- httr::GET(URL)

  ## Creating a tibble to view API information
  parsed <- fromJSON(rawToChar(id_info$content))

  parsed_tibble <- as_tibble(parsed[-1,])
  colnames(parsed_tibble) <- parsed[1,]

  return(list(parsed=parsed_tibble, var_info=var_data))
}

return_data <- get_data()
