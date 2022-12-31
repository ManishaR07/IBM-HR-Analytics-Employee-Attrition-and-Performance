## Survival analysis is extremely useful when we are trying to understand general patterns and trens of employee attrition.
## In the last analysis we tried to predict employee attrition using various models and found that Logistic Regression gives us the best accuracy.
## In this analysis we break it down and try to predict patterns of length of service before leaving and how this is different for male vs female employees

library(survival)
library(survminer)


##Loading the data
emp.data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv",header = TRUE,sep=",",stringsAsFactors = FALSE)
names(emp.data)

## Converting Gender and Attrition as factor variable
emp.data$Gender <- as.factor(emp.data$Gender)
emp.data$Attrition<- as.factor(emp.data$Attrition)

##Kaplan-Meier survival analysis - the following sets out the variables to use for the survival analyses leaver status on tenure overall
emp.data.survival <- with(emp.data,Surv(YearsAtCompany,Attrition))

#survival analyses 
fit.survival <- survfit(emp.data.survival~1,data=emp.data)

#prints the event (leaver) data and median survival  
fit.survival

#prints the survival table statistics
summary(fit.survival)

#produce event statistics and mean overall survival statistics 
print(fit.survival, print.rmean=TRUE)

#Plot of the overall turnover/tenure data
plot(fit.survival, xlab="Job Tenure",
     ylab="% Surviving", yscale=100,
     main="Survival Distribution (Overall)")

## From the plot it is evident that just after 30 years survival rate for employees increases considerably.
## The chance of survival at at the highest - later in their careers. As time goes on, there is a steady increase


#conduct survival analyses BY Gender
fit.surv.gender <- survfit(emp.data.survival~Gender,data=emp.data)

#prints the event (leaver) data and median survival for this analyses based on Gender
fit.surv.gender

#prints the survival table statistics
summary(fit.surv.gender)

#Prints the number of events (leaver) and mean survival by gender 
print(fit.surv.gender, print.rmean=TRUE)

#Plot of the different survival curves across males V females 
plot(fit.surv.gender, xlab="Job Tenure",
     ylab="% Surviving", yscale=100, col=c("blue","red"),
     main="Survival Distributions by Gender")
legend("topright", title="Gender", legend=c("Female", "Male"),
       fill=c("blue" , "red"))

## We see a very interesting trend from the above Survival Plot based on Gender. 
## We see that for females after 30 years chance of survival increases considerably
## But the trend varied for males. We observe that with the tenure there is a steady increased chance of survival
## But after 20 years for males the chance of survival almost plateaued 
## Looking at gender split there is a considerable difference in survival rates between males and females after 30 years of tenure

## 
