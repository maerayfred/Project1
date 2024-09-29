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
  var_info_tibble <- as_tibble(var_info_parsed)$variables

  var_data <- list()
  for (var in options) {
    var_data[var] <- var_info_tibble[var]
  }

  # PWGTP always included
  # AGEP as default, at least 1 numeric variable needs to be returned aside from PWGTP
  # do for loop that checks if range is in the var_data values (AGEP,PWGTP,etc.) ex) var_data$AGEP$values$range

  # SEX is default categorical variable
  # at least one categorical variable needs to be returned
  numeric_items <- c()
  categorical_items <- c()

  for (var in variables) {
    if (!is.null(var_data[[var]]$values$range)) {
      numeric_items <- c(numeric_items, var)
      # print(paste(var, "is numeric", sep=" "))
    } else {
      categorical_items <- c(categorical_items, var)
      # print(paste(var, "is categorical", sep=" "))
    }
  }

  if (length(numeric_items) <= 1) {
    numeric_items <- c("AGEP", "PWGTP")
  }

  if (length(categorical_items) == 0) {
    categorical_items <- c("SEX")
  }

  variables <- c(numeric_items, categorical_items)


  URL <- build_url(year=year, get_vals=variables)
  id_info <- httr::GET(URL)

  ## Creating a tibble to view API information
  parsed <- fromJSON(rawToChar(id_info$content))

  parsed_tibble <- as_tibble(parsed[-1,])
  colnames(parsed_tibble) <- parsed[1,]

  return(list(parsed=parsed_tibble, var_info=var_data, var_info_tibble=var_info_tibble))
}

return_data <- get_data(variables = c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL"))

JWAP_values <- return_data$var_info$JWAP$values$item[sort(names(return_data$var_info$JWAP$values$item))]
JWAP_df <- pivot_longer(as.data.frame(JWAP_values), cols=everything(), names_to="JWAP", values_to="time_intervals")
JWAP_wider <- separate_wider_delim(JWAP_df[-1,], cols="time_intervals", delim=". to ", names=c("left", "right"))
