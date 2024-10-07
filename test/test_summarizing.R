source("src/summarizing.R")
source("src/main.R")
source("test/test_main.R")

test_1_s <- function() {
  parsed <- test_1_m()$parsed
  return (summary_census(parsed))
}

test_2_s <- function() {
  parsed <- test_1_m()$parsed
  plot_census(parsed)
}
