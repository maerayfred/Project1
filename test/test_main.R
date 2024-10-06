source("src/main.R")

year <- ""
# year <- readline(prompt="enter year: ")
year <- ifelse(year == "", "2022", year)
variable_list <- NULL
all_valid_variables <- c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL")
variable_list <- all_valid_variables
print(glue("Valid variables: {glue_collapse(all_valid_variables, sep=', ')}"))
# while ( (val <- readline(prompt="enter variable: ")) != "") {
#   variable_list <- c(variable_list, val)
# }
# variable_list <- c("GASP", "GRPIP=10:20", "JWDP")
geography_level <- ""
# geography_level <- readline(prompt="enter geography level (region,state,division,All): ")
geography_level <- ifelse(geography_level == "", "state:10", ifelse(geography_level == "All", "", geography_level))

return_data <- get_data(
  year=year,
  variables=variable_list,
  geography_level = geography_level
)
print(return_data$URL)
str(return_data$parsed)