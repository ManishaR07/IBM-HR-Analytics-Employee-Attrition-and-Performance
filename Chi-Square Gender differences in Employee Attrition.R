library(ggplot2)
library(caret)
library(dplyr)

##Loading the data
emp.data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv",header = TRUE,sep=",",stringsAsFactors = FALSE)

#Convert Attrition variable into factor
emp.data$Attrition <- as.factor(emp.data$Attrition)
emp.data$Gender<-as.factor(emp.data$Gender)

#Calculating factor frequencies
Percentcount_attrition <- table(emp.data$Attrition)
Percentcount_attrition
prop.table(Percentcount_attrition)

#Crosstabs Gender by Attrition
Attrition_table=table(emp.data$Gender , emp.data$Attrition)
Attrition_table

#Chi-square Test
chisq.test(Attrition_table) 
## As p-value is not less than 0.05 so we ch-square analysis confirms that there was no significant difference between what you would
## expect to see in gender-wise attrition and what was observed