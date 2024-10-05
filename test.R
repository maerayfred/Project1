library(tidyverse)
library(tidycensus)
library(httr)
library(jsonlite)
library(glue)

build_url <- function(year="2022", get_vals=c("AGEP", "PWGTP", "SEX"), get_vals_subset=NULL, for_val="state:10") {
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

# TODO PWGTP cannot be given as a predicate, is a weight
# TODO ranges by colon (not all ranges contain all values for these)
# TODO ranges by colon or setting predicates equals, can be a non-supported value and will return nothing
example_build_url <- build_url(
  year="2022",
  get_vals=c("SEX", "PWGTP"),
  get_vals_subset = c("AGEP=00"),
  # get_vals_subset = c("HHL=0,1", "HHL=3,5", "JWAP=1:20", "HISPEED=0:10"),
  for_val = "state:10"
)
example_build_url

get_variable_list <- function(year="2022", subset=NULL) {
  # Get the full variable list for the given year
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

example_get_variable_list <- get_variable_list()

example_all_var_list <- c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "REGION", "DIVISION", "ST")
example_filtered_var_list <- get_variable_list(subset=example_all_var_list)

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

is_valid_variable_input <- function(given_list, valid_list, sep="=", with_delim=FALSE) {
  strip_value <- function(element) {
    if (with_delim) {
      return(substr(element, 1, regexpr(sep, element)))
    }
    return(strsplit(element, sep)[[1]][1])
  }

  # does it match any of the items in the valid list
  # all the items must match an item in the valid list
  # ^ ensures it's the start of the string
  if (!all(sapply(given_list, function(x) any(grepl(paste0("^", strip_value(x), "$"), valid_list))))) {
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
is_valid_variable_input(
  "state:",
  c("state:", "region:", "division:"),
  sep=":",
  with_delim=TRUE
)

remove_categorical_row_items_in_numeric <- function(tibble, variable_info) {
  numeric_columns <- tibble |> select(where(is.numeric))

  for (col in names(numeric_columns)) {
    # Get the min and max range for the current column from variable_info
    min_val <- as.integer(variable_info[[col]]$values$range$min)
    max_val <- as.integer(variable_info[[col]]$values$range$max)

    # Filter the tibble rows based on the min and max values for this column
    tibble <- tibble |>
      filter(!!sym(col) >= min_val & !!sym(col) <= max_val)

    return (tibble)
  }
}
# remove_categorical_row_items_in_numeric(return_data$parsed, return_data$var_info)

convert_categorical_to_factor <- function(tibble, variable_info) {
  character_columns <- tibble |> select(where(is.character))
  for (col in names(character_columns)) {
    if (col %in% c("JWAP", "JWDP")) {
      next
    }
    col_var <- col
    col_var <- ifelse(col == "state", "ST", col_var)
    col_var <- ifelse(col == "region", "REGION", col_var)
    col_var <- ifelse(col == "division", "DIVISION", col_var)

    # Get the item values for the current column from variable_info
    item_values <- variable_info[[col_var]]$values$item
    # sort the items based on names in reverse order to use labels later
    item_values <- item_values[order(names(item_values))]
    # some have 01, 02, etc. but are placed in the lists as 1, 2, etc.
    names(item_values) <- as.character(as.integer(names(item_values)))
    # Convert the column to a factor based on the item values
    tibble <- tibble |>
      mutate(!!sym(col) := factor(!!sym(col), levels=names(item_values), labels=item_values))
  }
  return (tibble)
}
# factor_tibble <- convert_categorical_to_factor(return_data$parsed, return_data$var_info)

factor_tibble <- return_data$parsed |>
  remove_categorical_row_items_in_numeric(return_data$var_info) |>
  convert_categorical_to_factor(return_data$var_info)

get_data <- function(year="2022", variables=c("AGEP", "PWGTP", "SEX"), geography_level="state:10") {
  # check if PWGTP is provided in the variable list. May be PWGTP=30 or PWGTP=30,50
  if (!any(grepl("^PWGTP$", variables))) {
    variables <- c(variables, "PWGTP")
  }

  # ensure year is between 2010 and 2022 (inclusive)
  year_options <- c(2010:2022)
  if (!year %in% year_options) {
    stop(glue("Invalid year {year}, should be one of [{glue_collapse(year_options, sep=', ')}]"))
  }

  # ensure variables is in the desired set given
  variable_options <- c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL")
  if (!is_valid_variable_input(variables, variable_options)) {
    stop(glue("Invalid variable in [{glue_collapse(variables, sep=', ')}], valid options [{glue_collapse(variable_options, sep=', ')}]"))
  }

  geography_options <- c("state:", "region:", "division:")

  if (geography_level != "" & !is_valid_variable_input(geography_level, geography_options, sep=":", with_delim=TRUE)) {
    stop(glue("Invalid geography level {geography_level}, should be 'all' or one of [{glue_collapse(geography_options, sep=', ')}]"))
  }

  filtered_var_info <- get_variable_list(year, c(variable_options, "ST", "REGION", "DIVISION"))

  # PWGTP always included
  # AGEP as default, at least 1 numeric variable needs to be returned aside from PWGTP
  # do for loop that checks if range is in the var_data values (AGEP,PWGTP,etc.) ex) var_data$AGEP$values$range

  # SEX is default categorical variable
  # at least one categorical variable needs to be returned

  numeric_items_count <- 0
  categorical_items_count <- 0
  numeric_item_list <- c()

  # check and filter the passed in variables
  for (var in variables) {
    var_split <- strsplit(var, "=")[[1]]
    var <- var_split[1]
    values <- var_split[2]
    print(paste(var, values, sep=" "))

    if (is_numeric_variable(var, filtered_var_info)) {
      numeric_item_list <- c(numeric_item_list, var)
      numeric_items_count <- numeric_items_count + 1
    } else {
      categorical_items_count <- categorical_items_count + 1
    }
    print(get_valid_variable_values(var, filtered_var_info))
  }

  if (numeric_items_count <= 1) {
    variables <- c(variables, "AGEP")
    numeric_item_list <- c(numeric_item_list, "AGEP")
    print("AGEP NA")
    print(get_valid_variable_values("AGEP", filtered_var_info))
  }

  if (categorical_items_count == 0) {
    variables <- c(variables, "SEX")
    print("SEX NA")
    print(get_valid_variable_values("SEX", filtered_var_info))
  }

  var_with_filter <- c()
  for (var in variables) {
    if (regexpr("=", var) != -1) {
      var_with_filter <- c(var_with_filter, var)
      variables <- variables[variables != var]
    }
  }

  URL <- build_url(year=year, get_vals=variables, get_vals_subset=var_with_filter, for_val=geography_level)
  id_info <- httr::GET(URL)

  ## Creating a tibble to view API information
  parsed <- fromJSON(rawToChar(id_info$content))

  parsed_tibble <- as_tibble(parsed[-1,])
  colnames(parsed_tibble) <- parsed[1,]

  if ("JWAP" %in% names(parsed_tibble)) {
    parsed_tibble$JWAP <- fix_time_interval_categories(filtered_var_info$JWAP$values$item, parsed_tibble$JWAP)
  }
  if ("JWDP" %in% names(parsed_tibble)) {
    parsed_tibble$JWDP <- fix_time_interval_categories(filtered_var_info$JWDP$values$item, parsed_tibble$JWDP)
  }
  print(glue("Numeric item list, gets converted to int - [{glue_collapse(numeric_item_list, sep=', ')}]"))
  print(glue("List without JWAP and JWDP - [{glue_collapse(numeric_item_list[!numeric_item_list %in% c('JWDP', 'JWAP')], sep=', ')}]"))

  parsed_tibble <- parsed_tibble |>
    mutate(across(all_of(numeric_item_list[!numeric_item_list %in% c("JWDP", "JWAP")]), as.integer))

  parsed_tibble <- parsed_tibble |>
    remove_categorical_row_items_in_numeric(filtered_var_info) |>
    convert_categorical_to_factor(filtered_var_info)

  # TODO time is not split into a numeric value yet

  return(list(parsed=parsed_tibble, var_info=filtered_var_info, URL=URL))
}

# TODO looping function for year

year <- ""
# year <- readline(prompt="enter year: ")
year <- ifelse(year == "", "2022", year)
variable_list <- NULL
all_valid_variables <- c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL")
variable_list <- all_valid_variables
print(glue("Valid variables: {glue_collapse(all_valid_variables, sep=', ')}"))
# while ( (val <- readline(prompt="enter variable: ")) != "") {
#   variable_list <- c(variable_list, val)
# }
# variable_list <- c("GASP", "GRPIP=10:20", "JWDP")
geography_level <- ""
# geography_level <- readline(prompt="enter geography level (region,state,division,All): ")
geography_level <- ifelse(geography_level == "", "state:10", ifelse(geography_level == "All", "", geography_level))

return_data <- get_data(
  year=year,
  variables=variable_list,
  geography_level = geography_level
)
print(return_data$URL)
str(return_data$parsed)
