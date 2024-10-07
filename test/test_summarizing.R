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

# data_1_s <- test_1_s()
# str(data_1_s, max.level = 2)
# data_1_s_summary <- summary_census(data_1_s)
# plot_census(data_1_s)


# parsed <- return_data$parsed
#
# summary_census(parsed)
#
# #Plot of information
#
# # library(ggplot2)
# # library(quantreg)
# # library(reshape2)
# # We chose to investigate the populations educational level base on age. We were interested in showing people who are typical school age range.
# parsed2<- parsed %>%
#   filter(AGEP<=40 & AGEP >=5)%>%
#   ggplot(aes(x =SCH, y =AGEP, weight= PWGTP,fill = SCH, color = SCH))+
#   geom_boxplot()
# print(parsed2)