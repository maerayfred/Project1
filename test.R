library(tidyverse)
library(tidycensus)
library(httr)
library(jsonlite)
library(glue)

# remove empty string from list
build_url <- function(year="2022", get_vals=c("AGEP", "PWGTP", "SEX"), get_vals_subset=c(), for_val="state:10") {
  BASE_URL <- glue("https://api.census.gov/data/{year}/acs/acs1/pums")
  get_vals_csv <- paste(get_vals, collapse=",")
  get_str <- glue("get={get_vals_csv}")
  for_str <- glue("for={for_val}")
  get_vals_subset_str <- paste(get_vals_subset, collapse="&")
  suffix_list <- c(get_str, get_vals_subset_str, for_str)
  suffix_list <- suffix_list[suffix_list != "for=all" & suffix_list != ""]
  suffix <- paste(suffix_list, collapse="&")
  return(glue("{BASE_URL}?{suffix}"))
}

# TODO PWGTP cannot be given as a predicate, is a weight
# TODO ranges by colon (not all ranges contain all values for these)
# TODO ranges by colon or setting predicates equals, can be a non-supported value and will return nothing
example_build_url <- build_url(
  year="2022",
  get_vals=c("AGEP", "SEX", "PWGTP"),
  get_vals_subset = c("HHL=0,1", "HHL=3,5", "JWAP=1:20", "HISPEED=0:10"),
  for_val = "state:10"
)
example_build_url

get_variable_list <- function(year="2022") {
  URL_VARIABLES <- glue("https://api.census.gov/data/{year}/acs/acs1/pums/variables.json")
  var_info <- httr::GET(URL_VARIABLES)
  var_info_parsed <- fromJSON(rawToChar(var_info$content))
  var_info_tibble <- as_tibble(var_info_parsed)$variables

  return(var_info_tibble)
}

example_get_variable_list <- get_variable_list()

filter_variable_list <- function(subset, var_info_tibble) {
  var_data <- list()
  for (var in subset) {
    var_data[var] <- var_info_tibble[var]
  }

  return(var_data)
}

example_all_var_list <- c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "REGION", "DIVISION", "ST")
example_filtered_var_list <- filter_variable_list(
  example_all_var_list,
  example_get_variable_list
)

is_numeric_variable <- function(var, var_info_tibble) {
    if (!is.null(var_info_tibble[[var]]$values$range) | var %in% c("JWAP", "JWDP")) {
      return (TRUE)
    }
    return (FALSE)
}

for (var in example_all_var_list) {
  print(paste(var, ifelse(is_numeric_variable(var, example_filtered_var_list), "numeric", "categorical")), sep=" ")
}
is_numeric_variable("test", example_get_variable_list)

get_valid_variable_values <- function(var, var_info_tibble) {
    var_range <- var_info_tibble[[var]]$values$range
    var_item <- var_info_tibble[[var]]$values$item

    ret <- c()

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

print(get_valid_variable_values("AGEP", example_get_variable_list))
print(get_valid_variable_values("REGION", example_get_variable_list))
print(get_valid_variable_values("test", example_get_variable_list))


is_valid_variable_value <- function(val, var, var_info_tibble) {
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

print(is_valid_variable_value("43", "ST", example_get_variable_list))
print(is_valid_variable_value("3", "REGION", example_get_variable_list))
print(is_valid_variable_value("0", "AGEP", example_get_variable_list))
print(is_valid_variable_value("00", "AGEP", example_get_variable_list))

fix_time_interval_categories <- function(item, value_list) {
  sorted_item <- item[sort(names(item))]
  item_values <- pivot_longer(as.data.frame(sorted_item), cols=everything(), names_to=NULL, values_to="time_intervals")
  item_df <- pivot_longer(as.data.frame(item_values), cols=everything(), names_to=NULL, values_to="time_intervals")
  item_df_transformed <- item_df[-1,] |>
      separate_wider_delim(cols="time_intervals", delim=" to ", names=c("left", "right")) |>
      mutate(left = gsub("a.m.", "AM", gsub("p.m.", "PM", left)),
           right = gsub("a.m.", "AM", gsub("p.m.", "PM", right))) |>
      mutate(left = parse_date_time(left, orders = "I:M p", tz = "UTC"),
           right = parse_date_time(right, orders = "I:M p", tz = "UTC")) |>
      mutate(midpoint = format(left + (right - left)/2, "%H:%M:%S")) |>
    select("midpoint")

  value_map <- c()
  for (value in value_list) {
    if (value == "0") {
      value_map <- c(value_map, "NA")
      next
    }
    value_map <- c(value_map, item_df_transformed$midpoint[as.numeric(value)])
  }
  return(value_list=value_map)
}
example_data <- httr::GET(build_url(year="2022", get_vals=c("JWAP", "JWDP"), for_val="state:10"))
example_parsed_data <- fromJSON(rawToChar(example_data$content))
example_parsed_tibble <- as_tibble(example_parsed_data[-1,])
colnames(example_parsed_tibble) <- example_parsed_data[1,]

example_parsed_tibble$JWAP_fixed <- fix_time_interval_categories(example_filtered_var_list$JWAP$values$item, example_parsed_tibble$JWAP)
example_parsed_tibble$JWDP_fixed <- fix_time_interval_categories(example_filtered_var_list$JWDP$values$item, example_parsed_tibble$JWDP)

is_valid_variable_input <- function(given_list, valid_list) {
  strip_value <- function(element) {
      return(strsplit(element, "=")[[1]][1])
  }

  # does it match any of the items in the valid list
  # all the items must match an item in the valid list
  # ^ ensures it's the start of the string
  if (!all(sapply(given_list, function(x) any(grepl(paste0("^", strip_value(x)), valid_list))))) {
      return (FALSE)
  }
  return (TRUE)
}

is_valid_variable_input(
  c("AGEP", "PWGTP", "SEX=1"),
  c("AGEP", "PWGTP", "SEX")
)
is_valid_variable_input(
  c("^AGEP", "PWGTP", "SAX=1"),
  c("AGEP", "PWGTP", "SEX")
)

get_data <- function(year="2022", variables=c("AGEP", "PWGTP", "SEX"), geography_level="all") {
  # check if PWGTP is provided in the variable list. May be PWGTP=30 or PWGTP=30,50
  if (!any(grepl("^PWGTP(=|$)", variables))) {
    variables <- c(variables, "PWGTP")
  }

  # ensure year is between 2010 and 2022 (inclusive)
  year_options <- c(2010:2022)
  if (!year %in% year_options) {
    stop(glue("Invalid year {year}, should be one of {glue_collapse(year_options, sep=', ')}"))
  }

  # ensure variables is in the desired set given
  variable_options <- c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL")
  if (!is_valid_variable_input(variables, variable_options)) {
    stop(glue("Invalid variable in {glue_collapse(variables, sep=', ')}, valid options {glue_collapse(variable_options, sep=', ')}"))
  }

  geography_options <- c("state:", "region:", "division:")
  print(geography_level)
  print(geography_options)
  print(geography_level != "all")
  print(paste0("^", geography_level))
  print(any(grepl(paste0("^", geography_level), geography_options))) # TODO need to split

  if (geography_level != "all" & !any(grepl(paste0("^", geography_level), geography_options))) {
    stop(glue("Invalid geography level {geography_level}, should be 'all' or one of {glue_collapse(geography_options, sep=', ')}"))
  }

  var_info_tibble <- get_variable_list(year)
  filtered_var_info <- filter_variable_list(variable_options, var_info_tibble)

  # TODO check variables values for valid
  # TODO check geography level values valid
  # TODO send to build_url as 2 lists - one for get values and one for values that will have direct filtering

  # PWGTP always included
  # AGEP as default, at least 1 numeric variable needs to be returned aside from PWGTP
  # do for loop that checks if range is in the var_data values (AGEP,PWGTP,etc.) ex) var_data$AGEP$values$range

  # SEX is default categorical variable
  # at least one categorical variable needs to be returned

  numeric_items_count <- 0
  categorical_items_count <- 0

  # check and filter the passed in variables
  for (var in variables) {
    var_split <- strsplit(var, "=")[[1]]
    var <- var_split[1]
    values <- var_split[2]
    print(paste(var, values, sep=" "))

    if (is_numeric_variable(var, filtered_var_info)) {
      numeric_items_count <- numeric_items_count + 1
    } else {
      categorical_items_count <- categorical_items_count + 1
    }
    print(get_valid_variable_values(var, filtered_var_info))
  }
  return()

  if (numeric_items_count <= 1) {
    numeric_items <- c("AGEP", "PWGTP")
  }

  if (categorical_items_count == 0) {
    categorical_items <- c("SEX")
  }

  variables <- c(numeric_items, categorical_items)


  URL <- build_url(year=year, get_vals=variables)
  id_info <- httr::GET(URL)

  ## Creating a tibble to view API information
  parsed <- fromJSON(rawToChar(id_info$content))

  parsed_tibble <- as_tibble(parsed[-1,])
  colnames(parsed_tibble) <- parsed[1,]

  parsed_tibble$JWAP_fixed <- fix_time_interval_categories(filtered_var_info$JWAP$values$item, parsed_tibble$JWAP)
  parsed_tibble$JWDP_fixed <- fix_time_interval_categories(filtered_var_info$JWDP$values$item, parsed_tibble$JWDP)


  return(list(parsed=parsed_tibble, var_info=filtered_var_info, var_info_tibble=var_info_tibble))
}

return_data <- get_data(
  year=2022,
  variables = c("AGEP=10,11", "PWGTP", "GASP", "SEX", "AGEP=13"),
  geography_level = "state:10"
)

