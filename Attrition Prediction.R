library(ggplot2)
library(caret)
library(dplyr)
library(GGally)
library(corrplot)
library(car)


##Loading the data
emp.data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv",header = TRUE,sep=",",stringsAsFactors = FALSE)
names(emp.data)

## Removing Emp count, Over 18 and Standard Hours columns from the data
emp.data <- subset(emp.data,select=-c(EmployeeCount,Over18,StandardHours))

#Convert Attrition variable into factor
emp.data$Attrition <- as.factor(emp.data$Attrition)
emp.data$Department<-as.factor(emp.data$Department)
emp.data$BusinessTravel<-as.factor(emp.data$BusinessTravel)
emp.data$Gender<-as.factor(emp.data$Gender)
emp.data$JobLevel<-as.factor(emp.data$JobLevel)
emp.data$JobRole<-as.factor(emp.data$JobRole)
emp.data$MaritalStatus<-as.factor(emp.data$MaritalStatus)
emp.data$OverTime<-as.factor(emp.data$OverTime)
emp.data$EducationField<-as.factor(emp.data$EducationField)
emp.data$Department<-as.factor(emp.data$Department)
emp.data$PerformanceRating <- factor(emp.data$PerformanceRating, ordered = TRUE, 
                                                levels = c("5", "4"))

## Checking multicollinearity in data
emp.numeric_data <- select_if(emp.data,is.numeric)
GGally::ggpairs(emp.numeric_data)
cor(emp.numeric_data)
corrplot(cor(emp.numeric_data), method = "number")

## Partitioning the train dataset into train and test
inTrain <- createDataPartition(y=emp.data$Attrition, p=0.9, list=FALSE)
train.data <- emp.data[inTrain, ] 
test.data <- emp.data[-inTrain, ]
dim(train.data) ## 1324 obs with 32 variables
dim(test.data) ## 146 obs with 32 variables

## Partitioning the train dataset into train and validate
inTrain <- createDataPartition(y=train.data$Attrition, p=0.8, list=FALSE)
train.data <- train.data[inTrain, ] 
validate.data <- train.data[-inTrain, ]
dim(train.data) ## 1060 obs with 32 variables
dim(validate.data) #207 obs with 32 variables

## Cross validation using 10 fold
trcontrol <- trainControl(method="cv",number =10)
metric <- "Accuracy"

## Remove column Performance Ratings as 50% of the values are missing
train.data <- subset(train.data,select=-c(PerformanceRating))
test.data <- subset(test.data,select=-c(PerformanceRating))
validate.data <- subset(validate.data,select=-c(PerformanceRating))

##Building model using Logistic Regression

set.seed(10)
fit.lda <- train(Attrition~., data=train.data, method="lda", metric=metric, trControl=trcontrol)


set.seed(10)
fit.glm <- train(Attrition~., data=train.data, method="glm", metric=metric, trControl=trcontrol)

## 3) Recursive Partioning and Regression Trees
set.seed(10)
fit.rpart <- train(Attrition~., data=train.data, method="rpart", metric=metric, trControl=trcontrol)

## 4) kNN
set.seed(10)
fit.knn <- train(Attrition~., data=train.data, method="knn", metric=metric, trControl=trcontrol)

## 5) Random Forest
set.seed(10)
fit.rf <- train(Attrition~., data=train.data, method="rf", metric=metric, trControl=trcontrol)

vif(fit.glm)

## Comparing accuracy of models
results <- resamples(list(lda=fit.lda, GLM = fit.glm, DT=fit.rpart, knn=fit.knn,RF=fit.rf))
summary(results)

model <- glm(Attrition~., family="binomial", data=train.data)
summary(model)
vif(model)

## vif values of Department,Education Field, JobRole, TotalWorkingYears and Yearsatcompany so removing these fields

## Remove columns Department,Education Field, JobRole, TotalWorkingYears and Yearsatcompany
train.data1 <- subset(train.data,select=-c(Department,EducationField,JobRole,TotalWorkingYears,YearsAtCompany,MonthlyIncome))
test.data1 <- subset(test.data,select=-c(Department,EducationField,JobRole,TotalWorkingYears,YearsAtCompany,MonthlyIncome))
validate.data1 <- subset(validate.data,select=-c(Department,EducationField,JobRole,TotalWorkingYears,YearsAtCompany,MonthlyIncome))

model1 <- glm(Attrition~., family="binomial", data=train.data1)
summary(model1)
vif(model1)

##Building model using LDa, Logistic Regression, Recursive Partitioning,knn and Random Forest

set.seed(12)
fit.lda_rev <- train(Attrition~., data=train.data1, method="lda", metric=metric, trControl=trcontrol)


set.seed(12)
fit.glm_rev <- train(Attrition~., data=train.data1, method="glm", metric=metric, trControl=trcontrol)

## 3) Recursive Partioning and Regression Trees
set.seed(12)
fit.rpart_rev <- train(Attrition~., data=train.data1, method="rpart", metric=metric, trControl=trcontrol)

## 4) kNN
set.seed(12)
fit.knn_rev <- train(Attrition~., data=train.data1, method="knn", metric=metric, trControl=trcontrol)

## 5) Random Forest
set.seed(12)
fit.rf_rev <- train(Attrition~., data=train.data1, method="rf", metric=metric, trControl=trcontrol)

## Comparing accuracy of models
results_1 <- resamples(list(lda=fit.lda_rev, GLM = fit.glm_rev, DT=fit.rpart_rev, knn=fit.knn_rev,RF=fit.rf_rev))
summary(results_1)

## LDA and Logistic Regression shows the best mean accuracy of 87%

## Make predictions on the validate dataset using Logistic Regression Model
prediction_validate_model <- predict(fit.glm_rev, validate.data1, na.action = na.pass)
confusionMatrix(prediction_validate_model, validate.data1$Attrition)

## On validate test model shows accuracy of 88%

## Optimizing the model by selecting only the significant variables with p<0.05 from glm model
selected_vars<- c("Attrition","Age","BusinessTravel","DistanceFromHome","EnvironmentSatisfaction","Gender","JobInvolvement","JobLevel","JobSatisfaction","MaritalStatus","NumCompaniesWorked","OverTime","RelationshipSatisfaction","WorkLifeBalance","YearsInCurrentRole","YearsSinceLastPromotion")
emp.data_sig_train <- subset(train.data1,select = selected_vars)
emp.data_sig_validate <- subset(validate.data1,select = selected_vars)
emp.data_sig_test <- subset(test.data1,select = selected_vars)

## Running Logistic Regression on the model
model2 <- glm(Attrition~., family="binomial", data=emp.data_sig_train)
summary(model2)
coefs <- summary(model2)$coefficients
full_coefs <- cbind(coefs[,c("Estimate","Pr(>|z|)")],odds_ratio = exp(model$coefficients))
full_coefs

## All else being equal, age has a negative effect on the likelihood of leaving
## All else being equal distance from home has a positive effect on likelihood of leaving, with one unit increase in distance associated with 62% increased odds of attrition
## All else being equal Environment Satisfaction has negative effect on likelihood of leaving with one full rating associated with 5% lower odds of attrition
## All else being equal Males have higher tendency to leave than females
## All else being equal Job Involvement has negative effect on likelihood of leaving with one full rating associated with 1% increase in odds of attrition
## All else being equal, job levels have a negative effect on attrition
## All else being equal number of companies worked has a positive effect on likelihood of leaving
## All else being equal Over time has a positive effect on likelihood of leaving
## All else being equal Relationship Satisfaction, WLB and Years in Current Role having negative impact on attrition

set.seed(12)
fit.glm_sig <- train(Attrition~., data=emp.data_sig_train, method="glm", metric=metric, trControl=trcontrol)
summary(fit.glm_sig)

## Make predictions on the validate dataset using revised Logistic Regression Model with significant variables
prediction_validate_model_sig <- predict(fit.glm_sig, emp.data_sig_validate, na.action = na.pass)
confusionMatrix(prediction_validate_model_sig, emp.data_sig_validate$Attrition)

## Accuracy of the model is 88%

## Predicting on testing dataset using Logistic Regression
prediction_test_data <- predict(fit.glm_rev, test.data1, type = "raw")
test.data1 <- test.data1 %>% select(everything()) %>% mutate(Attrition_pred = test.data1)
summary(prediction_test_data)

### Writing the predicted output in csv file
write.csv(prediction_test_data,"Attrition_Prediction.csv")