source("src/input_validation.R")

test_1 <- function() {
  # test valid default = delim
  valid_list <- c("AGEP", "PWGTP", "SEX")

  valid_given_lists <- list(
    "AGEP",
    c("AGEP", "PWGTP", "SEX"),
    c("AGEP=abc", "PWGTP=123", "SEX=")
  )

  for (valid_given_list in valid_given_lists) {
    if (!is_valid_variable_input(valid_given_list, valid_list)) {
      return(FALSE)
    }
  }
  return (TRUE)
}

test_2 <- function() {
  # test valid alternate : delim
  valid_list <- c("AGEP", "PWGTP", "SEX")

  valid_given_lists <- list(
    "AGEP",
    c("AGEP", "PWGTP", "SEX"),
    c("AGEP:abc", "PWGTP:123", "SEX:")
  )

  for (valid_given_list in valid_given_lists) {
    if (!is_valid_variable_input(valid_given_list, valid_list, sep=":")) {
      return(FALSE)
    }
  }
  return (TRUE)
}

test_3 <- function() {
  # test valid alternate : delim and including the delim in the match
  valid_list <- c("state:", "division:", "region:")

  valid_given_lists <- list(
    "state:",
    c("state:", "division:5"),
    c("state:", "division:", "region:"),
    c("state:abc", "division:123", "region:")
  )

  for (valid_given_list in valid_given_lists) {
    if (!is_valid_variable_input(valid_given_list, valid_list, sep=":", with_delim = TRUE)) {
      return(FALSE)
    }
  }
  return (TRUE)
}

test_4 <- function() {
  # test invalid default = delim
  valid_list <- c("AGEP", "PWGTP", "SEX")

  invalid_given_lists <- list(
    "AGE",
    "AGEPa",
    c("AGEP", "PWGTP", "SAX=1"),
    c("AGEP", "PWGTP:1")
  )

  for (invalid_given_list in invalid_given_lists) {
    if (is_valid_variable_input(invalid_given_list, valid_list)) {
      return(FALSE)
    }
  }
  return (TRUE)
}

test_5 <- function() {
  # test invalid alternate : delim
  valid_list <- c("AGEP", "PWGTP", "SEX")

  invalid_given_lists <- list(
    "AGE",
    "AGEPa",
    c("AGEP", "PWGTP", "SAX:1"),
    c("AGEP", "PWGTP=1")
  )

  for (invalid_given_list in invalid_given_lists) {
    if (is_valid_variable_input(invalid_given_list, valid_list, sep=":")) {
    return(FALSE)
    }
  }
  return (TRUE)
}

test_6 <- function() {
  # test invalid alternate : delim and including the delim in the match
  valid_list <- c("state:", "division:", "region:")

  invalid_given_lists <- list(
    "state",
    c("state:", "division=5"),
    c("state:", "division:", "region"),
    c("state:abc", "division:123", "ST:1")
  )

  for (invalid_given_list in invalid_given_lists) {
    if (is_valid_variable_input(invalid_given_list, valid_list, sep=":", with_delim = TRUE)) {
      return(FALSE)
    }
  }

  return (TRUE)
}

# test_1()
# test_2()
# test_3()
# test_4()
# test_5()
# test_6()
