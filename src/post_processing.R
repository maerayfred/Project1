library(tidyverse)
library(lubridate)

fix_time_interval_categories <- function(item, value_list) {
  # takes time intervals of the format "1:00 a.m. to 1:04 a.m."
  # converts to midpoint "1:02:00"
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
    separate_wider_delim(cols="midpoint", delim=":", names=c("hour", "minute", "second"), cols_remove = FALSE) |>
    mutate(hour_value = as.numeric(hour) + as.numeric(minute)/60 + as.numeric(second)/3600) |>
    select("midpoint", "hour_value")

  value_map <- NULL
  mid_map <- NULL
  for (value in value_list) {
    if (value == "0") {
      value_map <- c(value_map, 0)
      mid_map <- c(mid_map, 0)
      next
    }
    value_map <- c(value_map, item_df_transformed$hour_value[as.numeric(value)])
    mid_map <- c(mid_map, item_df_transformed$midpoint[as.numeric(value)])
  }
  return(list(value_list=value_map, midpoints=mid_map))
}

convert_columns_to_numeric <- function(tibble, numeric_item_list, exclude_list=NULL) {
  tibble <- tibble |>
    mutate(across(all_of(numeric_item_list[!numeric_item_list %in% exclude_list]), as.numeric))

  return (tibble)
}

remove_categorical_row_items_in_numeric <- function(tibble, variable_info) {
  # takes a tibble and removes rows where the numeric column is not in the range given for the variable
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

convert_categorical_to_factor <- function(tibble, variable_info) {
  # takes the character columns in the tibble and converts them to the factors in the variable list given
  # does some specific treatment of JWAP, JWDP, state, region, division - particular to census data
  character_columns <- tibble |> select(where(is.character))
  for (col in names(character_columns)) {
    if (!col %in% names(variable_info) & !col %in% c("state", "region", "division")) {
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