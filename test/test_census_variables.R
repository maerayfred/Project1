source("src/census_variables.R")

test_1_cv <- function () {
  # test default get all
  var_list <- get_variable_list()
  return (var_list)
}

test_2_cv <- function () {
  # test get specific
  var_list <- get_variable_list(subset="PWGTP")
  return (var_list)
}

test_3_cv <- function () {
  # test if a variable is numeric or not from all the desired variables
  all_var_list <- c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "REGION", "DIVISION", "ST")
  numeric_var_list <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "PWGTP")
  categorical_var_list <- c("FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "SEX", "REGION", "DIVISION", "ST")
  filtered_var_list <- get_variable_list(subset=all_var_list)

  for (var in all_var_list) {
    # print(paste(var, ifelse(is_numeric_variable(var, filtered_var_list), "numeric", "categorical")), sep=" ")

    if (is_numeric_variable(var, filtered_var_list)) {
      if (!var %in% numeric_var_list) {
        return (FALSE)
      }
    } else {
      if (!var %in% categorical_var_list) {
        return (FALSE)
      }
    }
  }
  return (TRUE)
}

test_4_cv <- function () {
  # test get valid variable values
  variable_list <- get_variable_list(subset=c("SEX", "PWGTP", "GRPIP"))
  cat_only <- get_valid_variable_values("SEX", variable_list)
  num_only <- get_valid_variable_values("PWGTP", variable_list)
  both <- get_valid_variable_values("GRPIP", variable_list)
  return (list(cat_only_SEX=cat_only, num_only_PWGTP=num_only, both_GRPIP=both))
}

test_5_cv <- function () {
  # test is valid variable value
  variable_list <- get_variable_list(subset=c("GRPIP", "AGEP", "SEX"))
  print("GRPIP")
  print(get_valid_variable_values("GRPIP", variable_list))
  # valid GRPIP
  for (val in c("0", "101", "1", "100", "45")) {
    if (!is_valid_variable_value(val, "GRPIP", variable_list)) {
      return (FALSE)
    }
  }
  # invalid GRPIP
  for (val in c("102", "00")) {
    if (is_valid_variable_value(val, "GRPIP", variable_list)) {
      return (FALSE)
    }
  }

  print("AGEP")
  print(get_valid_variable_values("AGEP", variable_list))
  # valid AGEP
  for (val in c("00", "1", "99", "45")) {
    if (!is_valid_variable_value(val, "AGEP", variable_list)) {
      print(val)
      return (FALSE)
    }
  }
  # invalid AGEP
  for (val in c("100", "0")) {
    if (is_valid_variable_value(val, "AGEP", variable_list)) {
      return (FALSE)
    }
  }

  print("SEX")
  print(get_valid_variable_values("SEX", variable_list))
  # valid SEX
  for (val in c("1", "2")) {
    if (!is_valid_variable_value(val, "SEX", variable_list)) {
      return (FALSE)
    }
  }
  # invalid SEX
  for (val in c("0", "3")) {
    if (is_valid_variable_value(val, "SEX", variable_list)) {
      return (FALSE)
    }
  }

  return (TRUE)
}

# test_1_cv
# str(tail(test_1_cv(), 3), max.level=4, list.len=6)
# test_2_cv
# str(test_2_cv(), max.level = 4, list.len = 6)
# test_3_cv
# test_3_cv()
# test_4_cv
# test_4_cv()
# test_5_cv
# test_5_cv()