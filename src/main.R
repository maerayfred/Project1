source("src/census_query.R")
source("src/census_variables.R")
source("src/input_validation.R")
source("src/post_processing.R")

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
  # some years dont have data
  # remove variables where they don't exist
  for (var in names(filtered_var_info)) {
    if (is.null(filtered_var_info[[var]]) | is.null(filtered_var_info[[var]]$values)) {
      print(paste0("removing ", var, " from year ", year))
      filtered_var_info <- filtered_var_info[!names(filtered_var_info) %in% var]
      for (var2 in variables) {
        if (any(grepl(var, var2))) {
          print(paste0("removing variable ", var2, " from year ", year))
          variables <- variables[variables != var2]
        }
      }
    }
  }
  # use the most updated variable list for geography level - not in prior years but still can filter
  # for those years, use prior variables for rest though, in case mapping changed for some reason
  newest_geography_variables <- get_variable_list(2022, c("ST", "REGION", "DIVISION"))
  filtered_var_info <- c(filtered_var_info, newest_geography_variables)

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
    # print(paste(var, values, sep=" "))

    if (is_numeric_variable(var, filtered_var_info)) {
      numeric_item_list <- c(numeric_item_list, var)
      numeric_items_count <- numeric_items_count + 1
    } else {
      categorical_items_count <- categorical_items_count + 1
    }
    # print(get_valid_variable_values(var, filtered_var_info))
  }

  if (numeric_items_count <= 1) {
    variables <- c(variables, "AGEP")
    numeric_item_list <- c(numeric_item_list, "AGEP")
    # print("AGEP NA")
    # print(get_valid_variable_values("AGEP", filtered_var_info))
  }

  if (categorical_items_count == 0) {
    variables <- c(variables, "SEX")
    # print("SEX NA")
    # print(get_valid_variable_values("SEX", filtered_var_info))
  }

  var_with_filter <- c()
  for (var in variables) {
    if (regexpr("=", var) != -1) {
      var_with_filter <- c(var_with_filter, var)
      variables <- variables[variables != var]
    }
  }

  URL <- census_url(year=year, get_vals=variables, get_vals_subset=var_with_filter, for_val=geography_level)
  parsed_tibble <- census_tibble(URL)

  if ("JWAP" %in% names(parsed_tibble)) {
    parsed_tibble$JWAP <- fix_time_interval_categories(filtered_var_info$JWAP$values$item, parsed_tibble$JWAP)
  }
  if ("JWDP" %in% names(parsed_tibble)) {
    parsed_tibble$JWDP <- fix_time_interval_categories(filtered_var_info$JWDP$values$item, parsed_tibble$JWDP)
  }
  # print(glue("Numeric item list, gets converted to int - [{glue_collapse(numeric_item_list, sep=', ')}]"))
  # print(glue("List without JWAP and JWDP - [{glue_collapse(numeric_item_list[!numeric_item_list %in% c('JWDP', 'JWAP')], sep=', ')}]"))

  parsed_tibble <- parsed_tibble |>
    convert_columns_to_numeric(numeric_item_list, exclude_list=c("JWDP", "JWAP")) |>
    remove_categorical_row_items_in_numeric(filtered_var_info) |>
    convert_categorical_to_factor(filtered_var_info)

  # TODO time is not split into a numeric value yet

  # add census class to the parsed tibble
  class(parsed_tibble) <- c("census", class(parsed_tibble))
  return(list(parsed=parsed_tibble, var_info=filtered_var_info, URL=URL))
}

get_data_years <- function(years="2022", variables=c("AGEP", "PWGTP", "SEX"), geography_level="state:10") {
  # get multiple years of data, removing 2020 if given - year is not valid for some reason
  years <- years[years != 2020]
  results <- lapply(years, function(year) get_data(year=year, variables=variables, geography_level=geography_level))
  names(results) <- years
  return(results)
}