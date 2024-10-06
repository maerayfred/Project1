
source("src/census_query.R")
source("src/post_processing.R")

example_data <- httr::GET(build_url(year="2022", get_vals=c("JWAP", "JWDP"), for_val="state:10"))
example_parsed_data <- fromJSON(rawToChar(example_data$content))
example_parsed_tibble <- as_tibble(example_parsed_data[-1,])
colnames(example_parsed_tibble) <- example_parsed_data[1,]

example_parsed_tibble$JWAP_fixed <- fix_time_interval_categories(example_filtered_var_list$JWAP$values$item, example_parsed_tibble$JWAP)
example_parsed_tibble$JWDP_fixed <- fix_time_interval_categories(example_filtered_var_list$JWDP$values$item, example_parsed_tibble$JWDP)

remove_categorical_row_items_in_numeric(return_data$parsed, return_data$var_info)

# factor_tibble <- convert_categorical_to_factor(return_data$parsed, return_data$var_info)

factor_tibble <- return_data$parsed |>
  remove_categorical_row_items_in_numeric(return_data$var_info) |>
  convert_categorical_to_factor(return_data$var_info)