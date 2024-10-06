library(tidyverse)
library(tidycensus)
library(httr)
library(jsonlite)
library(glue)
library(tidyr)
library(dplyr)
library(ggplot2)
library(quantreg)
library(reshape2)

##Finding mean/sd/counts for numerical and categorical data

parsed<-return_data$parsed
##Looped Columns

summary_census<-function(x,categorical=c("SEX","FER","HHL","SCH","SCHL", "HISPEED", "PWGTP","REGION","DIVISION","STATE"),numerical=c("GASP", "GRPIP","AGEP", "PWGTP")) {
  new_data_cat <- x[names(x) %in% categorical ]
  new_data_num <- x[names(x) %in% numerical ]
  
  for (col in names(new_data_cat)) {
    if (col == "PWGTP") {
      next
    }
    
    print(
      new_data_cat |>
        group_by(!!sym(col)) |>
        summarize(count=sum(PWGTP))
    )
  }
  for (col in names(new_data_num)) {
    if (col == "PWGTP") {
      next
    }
    print(col)
    print(
      new_data_num |>
        mutate(new_mult = !!sym(col) * PWGTP,
               new_mult2 = !!sym(col))|>
        summarize(mean=sum(new_mult) / sum(PWGTP),
                  sd=sqrt(sum(PWGTP*(!!sym(col)-mean)^2)/sum(PWGTP)))
    )
  }
}

summary_census(parsed)

#Plot of information 

# We chose to investigate the populations educational level base on age. We were interested in showing people who are typical school age range. 
parsed2<- parsed %>% 
  filter(AGEP<=40 & AGEP >=5)%>%
  ggplot(aes(x =SCH, y =AGEP, weight= PWGTP,fill = SCH, color = SCH))+
  geom_boxplot()
print(parsed2)