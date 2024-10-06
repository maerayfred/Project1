source("src/census_query.R")

test_1 <- function() {
  # test default
  URL <- census_url()
  
  return (URL)
}

test_2 <- function() {
  # test omissions of variables
  URL <- census_url(get_vals=NULL, get_vals_subset=NULL, for_val=NULL)
  
  return (URL)
}

test_3 <- function() {
  # test providing values use, no validations
  URL <- census_url(
    year=2021,
    get_vals = c("HISPEED", "PWGTP"),
    get_vals_subset = c("AGEP=00", "SEX=2", "GRPIP=30:32"),
    for_val = "region:1"
  )
  return (URL)
}

test_4 <- function() {
  # test using URL to get the same data in a tibble
  URL <- test_3()
  return (census_tibble(URL))
}

# data_1 <- test_1()
# data_2 <- test_2()
# data_3 <- test_3()
# data_4 <- test_4()
