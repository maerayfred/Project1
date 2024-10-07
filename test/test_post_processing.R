source("src/census_query.R")
source("src/census_variables.R")
source("src/post_processing.R")


test_1_pp <- function() {
  # test the time value conversions for the 2 columns JWAP and JWDP
  data <- census_tibble(census_url(year="2022", get_vals=c("JWAP", "JWDP"), for_val="state:10"))
  # lot of duplicate rows - get the unique ones
  data <- data |> distinct(.keep_all = TRUE)
  var_filtered <- get_variable_list("2022", c("JWAP", "JWDP"))
  data$JWAP_fixed <- fix_time_interval_categories(var_filtered$JWAP$values$item, data$JWAP)
  data$JWDP_fixed <- fix_time_interval_categories(var_filtered$JWDP$values$item, data$JWDP)

  # order the var_filtered items columns for better viewing purposes
  var_filtered$JWAP <- var_filtered$JWAP$values$item[order(names(var_filtered$JWAP$values$item))]
  var_filtered$JWDP <- var_filtered$JWDP$values$item[order(names(var_filtered$JWDP$values$item))]

  return (list(data=data, var_info=var_filtered))
}

test_2_pp <- function() {
  # test converting columns to numeric, given a list of numeric columns and a list of columns to exclude
  # str of data will have int where a character was
  data <- census_tibble(census_url(year="2022", get_vals=c("AGEP", "SEX", "JWAP"), for_val="state:10"))
  data <- convert_columns_to_numeric(data, c("AGEP", "JWAP"), "JWAP")
  return (data)
}

test_3_pp <- function() {
  # test the removal of categorical items in numeric columns (removes the 0)
  data <- census_tibble(census_url(year="2022", get_vals="AGEP", for_val="state:10"))
  var_filtered <- get_variable_list("2022", "AGEP")
  data <- data |>
    distinct(.keep_all = TRUE) |>
    convert_columns_to_numeric("AGEP") |>
    remove_categorical_row_items_in_numeric(var_filtered)
  return (list(data=data, var_info=var_filtered))
}

test_4_pp <- function() {
  # test the replacement of the categorical items with the factor labels
  data <- census_tibble(census_url(year="2022", get_vals=c("AGEP", "SEX", "HISPEED"), for_val="state:10"))
  var_filtered <- get_variable_list("2022", c("AGEP", "SEX", "HISPEED", "ST"))
  data$SEX_saved <- data$SEX
  data$HISPEED_saved <- data$HISPEED
  data$ST_saved <- data$state
  data <- data |>
    distinct(.keep_all = TRUE) |>
    convert_columns_to_numeric("AGEP") |>
    remove_categorical_row_items_in_numeric(var_filtered) |>
    convert_categorical_to_factor(var_filtered)

  var_filtered$AGEP <- var_filtered$AGEP$values
  var_filtered$SEX <- var_filtered$SEX$values$item[order(names(var_filtered$SEX$values$item))]
  var_filtered$HISPEED <- var_filtered$HISPEED$values$item[order(names(var_filtered$HISPEED$values$item))]
  var_filtered$ST <- var_filtered$ST$values$item[order(names(var_filtered$ST$values$item))]

  return(list(data=data, var_info=var_filtered))
}
