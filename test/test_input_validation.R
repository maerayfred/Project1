source("src/input_validation.R")

test_1 <- function() {
  # test valid
  return(is_valid_variable_input(
    c("AGEP", "PWGTP", "SEX=1"),
    c("AGEP", "PWGTP", "SEX")
  ))
}

test_2 <- function() {
  # test invalid
  return(!is_valid_variable_input(
    c("^AGEP", "PWGTP", "SAX=1"),
    c("AGEP", "PWGTP", "SEX")
  ))
}

test_3 <- function() {
  # test include sep
  return(is_valid_variable_input(
    "state:",
    c("state:", "region:", "division:"),
    sep=":",
    with_delim=TRUE
  ))
}

# data_1 <- test_1()
# data_2 <- test_2()
# data_3 <- test_3()
