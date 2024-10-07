library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(quantreg)

summary_census <- function(
  x,
  categorical=c("SEX","FER","HHL","SCH","SCHL", "HISPEED", "PWGTP","REGION","DIVISION","STATE"),
  numerical=c("GASP", "GRPIP","AGEP", "JWMNP", "PWGTP") # TODO JWAP and JWDP
) {
  # Finding mean/sd/counts for numerical and categorical data

  if (!("census" %in% class(x))) {
    stop("Input must be a census object")
  }

  # from the inputs, split into categorical and numerical data
  new_data_cat <- x[names(x) %in% categorical]
  new_data_num <- x[names(x) %in% numerical]

  cat_counts_result <- NULL
  num_mean_sd_result <- NULL
  
  for (col in names(new_data_cat)) {
    if (col == "PWGTP") {
      next
    }
    
    cat_counts_result[[col]] <- new_data_cat |>
      group_by(!!sym(col)) |>
      summarize(count=sum(PWGTP))
  }

  for (col in names(new_data_num)) {
    if (col == "PWGTP") {
      next
    }
    num_mean_sd_result[[col]] <- new_data_num |>
      mutate(new_mult = !!sym(col) * PWGTP) |>
      summarize(mean=sum(new_mult) / sum(PWGTP),
                sd=sqrt(sum(PWGTP * (!!sym(col) - mean)^2) / sum(PWGTP)))
  }

  return (list(cat_results=cat_counts_result, num_results=num_mean_sd_result))
}

# # We chose to investigate the populations educational level base on age. We were interested in showing people who are typical school age range.
# parsed2<- parsed %>%
#   filter(AGEP<=40 & AGEP >=5)%>%
#   ggplot(aes(x =SCH, y =AGEP, weight= PWGTP,fill = SCH, color = SCH))+
#   geom_boxplot()
# print(parsed2)

plot_census <- function(x, categorical="SCH", numerical="AGEP") {
  if (!("census" %in% class(x))) {
    stop("Input must be a census object")
  }

  if (length(categorical) != 1 | length(numerical) != 1) {
    stop("Only one categorical and one numerical variable can be plotted")
  }

  x |>
    ggplot(aes(x=!!sym(categorical), y=!!sym(numerical), weight=PWGTP, fill=!!sym(categorical), color=!!sym(categorical))) +
      geom_boxplot()
}
