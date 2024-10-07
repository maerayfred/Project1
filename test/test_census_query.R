source("src/census_query.R")

test_1_cq <- function() {
  # test default
  URL <- census_url()
  
  return (URL)
}

test_2_cq <- function() {
  # test omissions of variables
  URL <- census_url(get_vals=NULL, get_vals_subset=NULL, for_val=NULL)
  
  return (URL)
}

test_3_cq <- function() {
  # test providing values use, no validations
  URL <- census_url(
    year=2021,
    get_vals = c("HISPEED", "PWGTP"),
    get_vals_subset = c("AGEP=00", "SEX=2", "GRPIP=30:32"),
    for_val = "region:1"
  )
  return (URL)
}

test_4_cq <- function() {
  # test using URL to get the same data in a tibble
  URL <- test_3_cq()
  return (census_tibble(URL))
}
