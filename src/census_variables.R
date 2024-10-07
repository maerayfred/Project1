library("glue")
library("jsonlite")
library("tidyverse")
library("httr")

get_variable_list <- function(year="2022", subset=NULL) {
  # Get the full variable list for the given year, with option to filter the list down to a subset
  URL_VARIABLES <- glue("https://api.census.gov/data/{year}/acs/acs1/pums/variables.json")
  var_info <- httr::GET(URL_VARIABLES)
  var_info_parsed <- fromJSON(rawToChar(var_info$content))
  var_info_tibble <- as_tibble(var_info_parsed)$variables

  if (is.null(subset)) {
    return(var_info_tibble)
  }

  var_data <- list()
  for (var in subset) {
    var_data[var] <- var_info_tibble[var]
  }

  return(var_data)
}

is_numeric_variable <- function(var, var_info_tibble) {
  # check if a value is numeric - meaning range is provided and or it is a specific named date variable
  if (!is.null(var_info_tibble[[var]]$values$range) | var %in% c("JWAP", "JWDP")) {
    return (TRUE)
  }
  return (FALSE)
}

get_valid_variable_values <- function(var, var_info_tibble) {
  # get an output to see the valid variables that can be had (the items and the range)
  # items are just placed into the list, the range pulls the named items of min and max
  var_range <- var_info_tibble[[var]]$values$range
  var_item <- var_info_tibble[[var]]$values$item

  ret <- NULL

  if (!is.null(var_item)) {
    ret <- c(ret, names(var_item))
  }

  if (!is.null(var_range)) {
    min <- as.numeric(var_range$min)
    max <- as.numeric(var_range$max)
    ret <- c(ret, c(min=min,max=max))
  }

  return (ret)
}

is_valid_variable_value <- function(val, var, var_info_tibble) {
  # give a value for a variable, check if it is valid
  valid_values <- get_valid_variable_values(var, var_info_tibble)

  if (val %in% valid_values) {
    return (TRUE)
  }

  # if names contains min and max both, there was a range provided
  if ("min" %in% names(valid_values) & "max" %in% names(valid_values)) {
    min <- as.numeric(valid_values["min"])
    max <- as.numeric(valid_values["max"])
    num_val <- as.numeric(val)
    return (num_val >= min & num_val <= max)
  }

  return (FALSE)
}