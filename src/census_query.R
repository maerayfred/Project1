library(glue)
library(httr)
library(jsonlite)
library(tidyverse)


census_url <- function(year="2022", get_vals=c("AGEP", "PWGTP", "SEX"), get_vals_subset=NULL, for_val="state:10") {
  # Builds the URL, assumes all values are valid and/or validation is done outside of this function
  # Some defaults are provided, just to ensure a large API call URL isn't constructed on accident

  BASE_URL <- glue("https://api.census.gov/data/{year}/acs/acs1/pums")
  get_vals_str <- ifelse((vals <- paste(get_vals, collapse=",")) != "", glue("get={vals}"), "")
  get_vals_subset_str <- paste(get_vals_subset, collapse="&")
  for_str <- ifelse(for_val != "", glue("for={for_val}"), "")
  suffix_list <- c(get_vals_str, get_vals_subset_str, for_str)
  suffix <- paste(suffix_list[suffix_list != ""], collapse="&")
  return(glue("{BASE_URL}?{suffix}"))
}

census_tibble <- function(URL) {
  # Given a census_url, this function will return a tibble of the data
  id_info <- httr::GET(URL)

  ## Creating a tibble to view API information
  parsed <- fromJSON(rawToChar(id_info$content))

  parsed_tibble <- as_tibble(parsed[-1,])
  colnames(parsed_tibble) <- parsed[1,]

  return (parsed_tibble)
}