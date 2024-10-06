is_valid_variable_input <- function(given_list, valid_list, sep="=", with_delim=FALSE) {
  # Checks if the given list of variables is valid, meaning each item in the list is found in the valid list
  # left_most side of the variable is exactly within the valid list

  # take the string element and split it based on the delimiter - taking the first item
  # if with_delim is true, then take the portion up to and including the delimiter
  strip_value <- function(element) {
    if (with_delim) {
      return(substr(element, 1, regexpr(sep, element)))
    }
    return(strsplit(element, sep)[[1]][1])
  }

  # does it match any of the items in the valid list
  # all the items must match an item in the valid list
  # ^ ensures it's the start of the string and $ ensures it's the end of the string, so must be hard match to the valid list
  if (!all(sapply(given_list, function(x) any(grepl(paste0("^", strip_value(x), "$"), valid_list))))) {
    return (FALSE)
  }
  return (TRUE)
}