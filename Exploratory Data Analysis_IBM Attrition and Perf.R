library(ggplot2)
library(caret)
library(dplyr)

##Loading the data
emp.data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv",header = TRUE,sep=",",stringsAsFactors = FALSE)

##Checking data structure
str(emp.data)
summary(emp.data)

## We have 1470 observations of  35 variables
## Mean of age is 37 years,Environment Satisfaction, Job Involvement, Relationship Satisfaction have mean of 2.7 each
## average total experience in company is 7 years and average time in current role is 4.2 years and with current manager is 4 years

## Checking for missing values
colSums(is.na(emp.data))
## There is no missing data in the dataset

## EXPLORATORY DATA ANALYSIS

## Removing Emp count, Over 18 and Standard Hours columns from the data
emp.data <- subset(emp.data,select=-c(EmployeeCount,Over18,StandardHours))

## Analyzing gender distribution in data
ggplot(emp.data,aes(x=Gender))+
  geom_bar(fill = "green")+
  xlab("Gender")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Gender Distribution")

prop.table(table(emp.data$Gender))
## Gender distribution is 60% Males and 40% Females

## Analyzing distribution by department in data
ggplot(emp.data,aes(x=Department))+
  geom_bar(fill = "green")+
  xlab("Department")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Department Distribution")

prop.table(table(emp.data$Department))
## We have maximum personnel in R&D (65%), followed by Sales (30%) and HR (4%)

## Analyzing distribution by Education in data
ggplot(emp.data,aes(x=Education))+
  geom_bar(fill = "green")+
  xlab("Education")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Education Distribution")

prop.table(table(emp.data$Education))
## We have maximum employees with education Level 3 (39%), followed by Level 5 (33%) and Level 4(27%)

## Analyzing distribution by Education Field in data
ggplot(emp.data,aes(x=EducationField))+
  geom_bar(fill = "green")+
  xlab("EducationField")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("EducationField Distribution")

prop.table(table(emp.data$EducationField))
## Maximum employees are ine ducation field Life Sciences(41%), Medical (32%),Marketing (10%)

## Analyzing distribution by Job Level Field in data
ggplot(emp.data,aes(x=JobLevel))+
  geom_bar(fill = "green")+
  xlab("JobLevel")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("JobLevel Distribution")

prop.table(table(emp.data$JobLevel))
## Maximum employees are in entry level jobs - JobLevel 1 (37%), Job Level 2 (36%) and Job Level 3(15%)

## Analyzing distribution by Job Role Field in data
ggplot(emp.data,aes(x=JobRole))+
  geom_bar(fill = "green")+
  xlab("JobRole")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("JobRole Distribution")

prop.table(table(emp.data$JobRole))
## Maximum employees are with job role Sales Executive (22%), followed by Research Scientist (20%),Lab Technician (18%), Manufacturing Director (10%)

## Analyzing distribution by Marital Status in data
ggplot(emp.data,aes(x=MaritalStatus))+
  geom_bar(fill = "green")+
  xlab("Marital Status")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Marital Status Distribution")

prop.table(table(emp.data$MaritalStatus))
## 46% are married, 32% are single and 22% are divorced

## Analyzing distribution by Performance Rating in data
ggplot(emp.data,aes(x=PerformanceRating))+
  geom_bar(fill = "green")+
  xlab("Performance Rating")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Performance Rating Distribution")

prop.table(table(emp.data$PerformanceRating))
## 85% have Perf Rating - 3, and 15% have Perf Rating 4

## Analyzing distribution by Attrition in data
ggplot(emp.data,aes(x=Attrition))+
  geom_bar(fill = "green")+
  xlab("Attrition")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Attrition Distribution")

prop.table(table(emp.data$Attrition))
## Attrition percentage is 16%

## Attrition vs Gender
ggplot(emp.data,aes(x=Gender,fill=Attrition))+
  geom_bar()+
  facet_wrap("Attrition")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
  xlab("Gender")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Attrition by Gender")

prop.table(table(emp.data$Attrition,emp.data$Gender))
##Of the total attrition Male leavers are 10% and female leavers are 6%

## Attrition vs Department
ggplot(emp.data,aes(x=Department,fill=Attrition))+
  geom_bar()+
  facet_wrap("Attrition")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
  xlab("Department")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Attrition by Department")

prop.table(table(emp.data$Attrition,emp.data$Department))
## Maximum attrition is in R&D(9%) followed by 6% in Sales

## Attrition in Education Levels
ggplot(emp.data,aes(x=Education,fill=Attrition))+
  geom_bar()+
  facet_wrap("Attrition")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
  xlab("Education")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Attrition by Education")

prop.table(table(emp.data$Attrition,emp.data$Education))
## Maximum attrition is in Education Level 3(7%), followed by Level 4 (4%)

## Attrition in Education Field
ggplot(emp.data,aes(x=EducationField,fill=Attrition))+
  geom_bar()+
  facet_wrap("Attrition")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
  xlab("EducationField")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Attrition by EducationField")

prop.table(table(emp.data$Attrition,emp.data$EducationField))
## Maximu attrition is in Life Sciences (6%) followed by Medical (4%)

## Attrition by Job Level
ggplot(emp.data,aes(x=JobRole,fill=Attrition))+
  geom_bar()+
  facet_wrap("Attrition")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
  xlab("JobRole")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Attrition by JobRole")

prop.table(table(emp.data$Attrition,emp.data$JobRole))
## Maximum attrition in Lab Technician (4%) followed by Sales Executive (3.8%), Research Scientist(3%)

## Attrition by Marital status
ggplot(emp.data,aes(x=MaritalStatus,fill=Attrition))+
  geom_bar()+
  facet_wrap("Attrition")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
  xlab("MaritalStatus")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Attrition by MaritalStatus")

prop.table(table(emp.data$Attrition,emp.data$MaritalStatus))
## Maximum attrition in Employees who are single(8%) followed by Married (6%) and Divorced (2%)

## Attrition by Performance Rating
ggplot(emp.data,aes(x=PerformanceRating,fill=Attrition))+
  geom_bar()+
  facet_wrap("Attrition")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
  xlab("PerformanceRating")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Attrition by Performance Rating")

prop.table(table(emp.data$Attrition,emp.data$PerformanceRating))
## 14% attrition in employees with performance rating - 3 (14%) and performance rating -4 (3%)

## Analyzing age distribution
hist(emp.data$Age,breaks=10,col="blue")
## age distribution is right skewed

## Analyzing distribution of Total Working Years
hist(emp.data$TotalWorkingYears,breaks=5,col="blue")
## Maximum employees with 0-10 yrs experience

## Analyzing distribution of Years at company
hist(emp.data$YearsAtCompany,breaks=5,col="blue")
## Data is right skewed

## Analyzing distribution by years in current role
hist(emp.data$YearsCurrRole,breaks=7,col="blue")
