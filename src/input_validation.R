is_valid_variable_input <- function(given_list, valid_list, sep="=", with_delim=FALSE) {
  #

  #
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