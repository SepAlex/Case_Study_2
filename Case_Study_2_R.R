library(tidyverse)
library(ggplot2)
library(GGally)
library(plyr)
library(dplyr)
library(tidyverse)
library(tibble)
library(tidyr)
library(caret)
library(SuperLearner)
library(gridExtra)
library(stats)
library(naniar)
library(corrplot)
library(ggcorrplot)
library(corrgram)
library(e1071)
library(MASS)
library(reshape2)
library(RANN)
library(class)
library(magrittr)
library(readr)
library(FNN)
library(scales)
library(purrr)
library(psych)
library(tidyr)
library(forcats)
library(matrixStats)
library(esquisse)
library(naniar)
library(olsrr)
library(stringr)
library(e1071)
library(FNN)
library(MASS)





#####Reading Data
attrition <- read.csv('CaseStudy2-data.csv', header = T) 

#col_types = cols( JobRole = col_factor(),MaritalStatus = col_factor(), OverTime = col_factor(),Age = col_integer(),DailyRate = col_integer(),
#DistanceFromHome = col_integer(),EmployeeNumber = col_integer(),HourlyRate = col_integer(), MonthlyIncome = col_integer(),
#MonthlyRate = col_integer(),Attrition = col_character(),BusinessTravel = col_factor(),Department = col_factor(),
#EducationField = col_factor(),Gender = col_factor(),YearsWithCurrManager = col_integer(),Education = col_integer(),EnvironmentSatisfaction = col_integer(),
#JobInvolvement = col_integer(),JobLevel = col_integer(),NumCompaniesWorked = col_integer(),PercentSalaryHike = col_integer(),TotalWorkingYears = col_integer(),
#TrainingTimesLastYear = col_integer(),YearsAtCompany = col_integer(),YearsInCurrentRole = col_integer(),YearsSinceLastPromotion = col_integer(),JobSatisfaction = col_integer(),
#PerformanceRating = col_integer(),RelationshipSatisfaction = col_integer(),StockOptionLevel = col_integer(),WorkLifeBalance = col_integer() )#



attrition <- data.frame(attrition)

###### EDA #####

########### Exploring factors that contribute to turnover ########## 

#any missing value
gg_miss_var(attrition)

  
summary(attrition) 


### Employee Attrition Status Count

attrition_yes <- gsub("Yes", "Resigned",attrition$Attrition, ignore.case=T)
attrition_no <- gsub("No", "Employed",attrition_yes, ignore.case=T)
attrition$Attrition <- attrition_no


###Variable Types
Non_Cat_var <- attrition %>% dplyr::select(DailyRate,DistanceFromHome,Age,MonthlyIncome,HourlyRate, MonthlyRate, PercentSalaryHike,
                                         TotalWorkingYears,NumCompaniesWorked,WorkLifeBalance, PercentSalaryHike, EmployeeNumber,TrainingTimesLastYear,
                                         YearsAtCompany,Education,EnvironmentSatisfaction,YearsInCurrentRole,YearsWithCurrManager,YearsSinceLastPromotion,
                                         JobInvolvement,PerformanceRating,StockOptionLevel, JobLevel, JobSatisfaction,RelationshipSatisfaction)


attrition1 <- attrition %>% mutate(EnvironmentSatisfaction = as.factor(if_else(EnvironmentSatisfaction == 1,'Low',if_else(EnvironmentSatisfaction == 2, 'Medium', if_else(EnvironmentSatisfaction == 3, 'High', 'Very High')))),
                                   Education = as.factor(if_else(Education == 1,'Below College', if_else(Education == 2, 'College', if_else(Education == 3, 'Bachelor', if_else(Education == 4, 'Master','Doctor'))))),
                                   JobSatisfaction = as.factor(if_else(JobSatisfaction == 1, 'Low',if_else(JobSatisfaction == 2, 'Medium',if_else(JobSatisfaction == 3, 'High','Very High')))),
                                   JobInvolvement = as.factor(if_else(JobInvolvement == 1,'Low',if_else(JobInvolvement == 2, 'Medium',if_else(JobInvolvement == 3, 'High', 'Very High')))),
                                   WorkLifeBalance = as.factor(if_else(WorkLifeBalance == 1, 'Bad',if_else(WorkLifeBalance == 2, 'Good', if_else(WorkLifeBalance == 3, 'Better', 'Best')))),
                                   PerformanceRating = as.factor(if_else(PerformanceRating == 1, 'Poor',if_else(PerformanceRating == 2, 'Good', if_else(PerformanceRating == 3, 'Excellent', 'Outstanding')))),
                                   JobLevel = as.factor(if_else(JobLevel == 1,'Entry Level',if_else(JobLevel == 2, 'Intermediate Level',if_else(JobLevel == 3, 'Junior-Level Management', if_else(JobLevel == 4, 'Senior-Level Management', 'Executive'))))),
                                   RelationshipSatisfaction = as.factor(if_else(RelationshipSatisfaction == 1, 'Low',if_else(RelationshipSatisfaction == 2, 'Medium', if_else(RelationshipSatisfaction == 3, 'High', 'Very High')))))

######################################################################################################

## Heatmap Exploring General Correlation among the variables

######################################################################################################
###Categorical Variables and how they stack up in Correlation ##

correlation_Var <- round(cor(Non_Cat_var), 1)

ggcorrplot(correlation_Var)


## Relationship Type Correlation based on information gathered so far

## Transforming Categorical Variables

#############################################################################################################################################


correlation_Var = attrition %>% dplyr::select(Attrition, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager)
correlation_Var <- data.frame(correlation_Var)
ggpairs(correlation_Var, aes(color = Attrition), progress = FALSE)

attrition$LogYearsAtCompany <- log(attrition$YearsAtCompany + 1)
attrition$LogYearsInCurrentRole <- log(attrition$YearsInCurrentRole + 1)
attrition$LogYearsSinceLastPromotion <-  log(attrition$YearsSinceLastPromotion + 1)
attrition$LogYearsWithCurrManager <- log(attrition$YearsWithCurrManager + 1)
attrition$LogDailyRate <- log(attrition$DailyRate + 1)
attrition$LogHourlyRate <- log(attrition$HourlyRate + 1)
attrition$LogMonthlyRate <- log(attrition$MonthlyRate, + 1)


log_correlation_Var <- attrition1 %>% dplyr::select(Attrition, LogYearsWithCurrManager,LogYearsInCurrentRole, LogYearsAtCompany,LogYearsSinceLastPromotion)

ggpairs(log_correlation_Var, mapping=ggplot2::aes(color = Attrition), progress = FALSE)



Log_Non_Log_Cat_var <- attrition1 %>% dplyr::select(DailyRate,DistanceFromHome,Age,MonthlyIncome,HourlyRate, MonthlyRate, PercentSalaryHike,
                                                   TotalWorkingYears,NumCompaniesWorked,WorkLifeBalance, PercentSalaryHike, EmployeeNumber,TrainingTimesLastYear,
                                                   Education,EnvironmentSatisfaction,YearsWithCurrManager,
                                                   JobInvolvement,PerformanceRating,StockOptionLevel, JobLevel, JobSatisfaction,RelationshipSatisfaction,LogYearsWithCurrManager,
                                                   LogYearsInCurrentRole, LogYearsAtCompany,LogYearsSinceLastPromotion)


#####################################################################################################

### Relationship Satisfaction Rate vs Jobrole by Attrition

######################################################################################################

attrition %>% ggplot(aes(x= JobRole, y = RelationshipSatisfaction, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Boxplot of Job Role vs Relationship Satisfaction by Attrition") +
  coord_flip()


 #ggplotGrid(ncol = 2,lapply(c('Department','EducationField','BusinessTravel','JobRole','OverTime', 'Gender','MaritalStatus'),function(col) {ggplot(attrition, aes_string(col,fill=Attrition)) + geom_bar(aes())}))




#facet_wrap(~ Sex)

#######################################################################

##Percentage of employee that still work or resigned by gender

######################################################################
Employee_status_count <- cbind(attrition$Attrition, attrition$Gender)
Employee_status_count <- with(attrition, table(Gender, Attrition))
Employee_status_count1 <- data.frame(Employee_status_count)


percent_Attrition <- round(Employee_status_count1$Freq/sum(Employee_status_count1$Freq)*100)

Attrition_With_percent <- paste(percent_Attrition,"%",sep="")  # add percents to labels

Gender <- paste(Employee_status_count1$Gender,Employee_status_count1$Attrition,Attrition_With_percent)

pie(percent_Attrition,labels = Gender, col=topo.colors(length(Gender)),
    main="Pie Chart of Total Attrition by Gender and Percentage")


####################################################################################

##Distance Traveled To Work Among Attrition Groups

####################################################################################
#describe(attrition$DistanceFromHome)


#attrition$DistanceFromHome <-  attrition %>% mutate(DistanceFromHome = as.factor(if_else(DistanceFromHome <= mean(attrition$DistanceFromHome), "Low", "High")))

#attrition <- attrition$DistanceFromHome 

#DistanceFromHome_Attrition <- attrition %>% group_by(DistanceFromHome)%>% dplyr::summarize(count = n())

#attrition %>%
  #group_by(Attrition, DistanceFromHome) %>% tally() %>%
  #ggplot(aes(x = Attrition, y = n, fill=Attrition)) +
  #geom_bar(position="dodge2", stat = "identity") +
  ##theme_minimal()+
  #labs(x="Attrition",  
     #  y="Total Distance Traveled to work")+
  #ggtitle("Distance Travelled To Work Among Attrition Groups")+
  #geom_text(aes(label = n), 
           # vjust = -0.5, 
            #position = position_dodge(0.9))


##Distance Travelled To Work Among Attrition Groups
Dis_Trav <- attrition %>%
  group_by(Attrition, DistanceFromHome) %>% tally() %>%
  ggplot(aes(x = Attrition, y = n, fill=Attrition)) +
  geom_bar(position="dodge2", stat = "identity") +
  theme_minimal()+
  labs(x="Distance From Home",  
       y="Total Distance Traveled to work")+
  ggtitle("Distance Travelled To Work Among Attrition Groups")
  


## Age Distribution Among Attrition Groups
Age_Yrs <- attrition %>%
  group_by(Attrition, Age) %>% tally() %>%
  ggplot(aes(x = Age, y = n, fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="AGE",  
       y="Age Count")+
  ggtitle("Age Distribution Among Attrition Groups")


## Number of Years Employee Serve Under Current Manager
Yrs_Serv_Manager <- attrition %>%
  group_by(Attrition, YearsWithCurrManager) %>% tally() %>%
  ggplot(aes(x = YearsWithCurrManager, y = n, fill=Attrition)) +
  geom_bar(position="dodge2", stat = "identity") +
  theme_minimal()+
  labs(x="years",  
       y=" Total Count")+
  ggtitle("Years Served Under Curreent Manager Among Attrition Groups")


##Performance Rating Among Employees
Pef_Rating <- attrition1 %>%
  group_by(Attrition, PerformanceRating) %>% tally() %>%
  ggplot(aes(x = PerformanceRating, y = n, fill=Attrition)) +
  geom_bar(position="dodge2", stat = "identity") +
  theme_minimal()+
  labs(x="Rating",  
       y="Total Count")+
  ggtitle("Performance Rating Among Attrition Groups")


##"Number of Companies an Employee has Worked For By Attrition Groups
Num_Comp_Wrkd4 <- attrition %>%
  group_by(Attrition, NumCompaniesWorked) %>% tally() %>%
  ggplot(aes(x = round(NumCompaniesWorked,0), y = n, fill=Attrition)) +
  geom_bar(position="dodge2", stat = "identity") +
  theme_minimal()+
  labs(x="Number of Companies WOrked For",  
       y="Total Count")+
  ggtitle("Number of Companies an Employee has Worked For By Attrition Groups")


##Total Working Years Among Attrition Groups
Total_Yrs_Work <- attrition %>%
  group_by(Attrition, TotalWorkingYears) %>% tally() %>%
  ggplot(aes(x = TotalWorkingYears, y = n, fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="TotalWorkingYears",  
       y="Total Count")+
  ggtitle("Total Working Years Among Attrition Groups")


grid.arrange(Dis_Trav, Age_Yrs, Yrs_Serv_Manager,Total_Yrs_Work,Pef_Rating,Num_Comp_Wrkd4, nrow = 2, ncol = 3)
####################################################################################################

##How Money influences an Employee's decision to Work or Resign

##################################################################################################

Job_Level <- attrition1 %>%
  group_by(Attrition, JobLevel) %>% tally() %>%
  ggplot(aes(x = reorder(JobLevel, -n), y = n, fill=Attrition)) +
  geom_bar(position="dodge2", stat = "identity") +
  theme_minimal()+
  labs(x="Job Level",y="Total Count")+
  ggtitle("Job Level Among Attrition Groups")+
  geom_text(aes(label=n),  position=position_dodge(width=0.9), vjust=-0.5) + 
  theme(plot.title = element_text(size =10))


##By Monthly Income
Monthly_Income <- attrition %>% ggplot(aes(x= MonthlyIncome, fill= Attrition, col = Attrition)) +
  geom_histogram(position="identity") +
  labs(title = "Effect of Monthly Income Attrition")  

##By Hourly Rqte
Hourly_Rate <- attrition %>% ggplot(aes(x= HourlyRate, fill= Attrition, col = Attrition)) +
  geom_histogram(position="identity") +
  labs(title = "Effect of Hourly Rate on Attrition")  


##By Daily Rqte
Daily_Rate <- attrition %>% ggplot(aes(x= DailyRate, fill= Attrition, col = Attrition)) +
  geom_histogram(position="identity") +
  labs(title = "Effect of Daily Rate on Attrition")  


##By Monthly Rqte
Monthly_Rate <- attrition %>% ggplot(aes(x= MonthlyRate, fill= Attrition, col = Attrition)) +
  geom_histogram(position="identity") +
  labs(title = "Effect of Monthly Rate on Attrition")  

grid.arrange(Job_Level, Monthly_Income, Hourly_Rate, Daily_Rate , Monthly_Rate, nrow = 2, ncol = 3)


###########################################################################################################################

## What Job Role Have the Highest Job Satisfaction

###########################################################################################################################


attrition %>% ggplot(aes(x= JobRole, y = JobSatisfaction, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Boxplot of Job Role vs Relationship Satisfaction by Attrition") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#############################################################################################################################################

### Prediction and Classification Model

##############################################################################################################################################

attrition$Attrition <- attrition$Attrition %>% recode('Resigned'=1,'Employed'= 0)
attrition$OverTime <- attrition$OverTime %>% recode('Yes'=1,'No'= 0)
attrition$BusinessTravel <- attrition$BusinessTravel %>% recode('Travel_Frequently'=2,'Travel_Rarely'=1,'Non-Travel'= 0)
attrition$EducationField <- attrition$EducationField %>% recode('Medical'=0,'Life Sciences'=1,'Other'=2, 'Marketing'=3, 'Technical Degree'=4, 'Human Resources'=5)
attrition$MaritalStatus <- attrition$MaritalStatus %>% recode('Divorced'=2,'Married'=1,'Single'= 0)
attrition$JobRole <- attrition$JobRole %>% recode('Sales Representative'=0, 'Human Resources'=1,'Laboratory Technician'=2,'Sales Executive'=3, 'Research Scientist'=4, 
                                                   'Healthcare Representative'=5, 'Manager'=6, 'Research Director'=7, 'Manufacturing Director'=7 )
attrition$Department <- attrition$Department %>% recode('Sales'=2,'Human Resources'=1,'Research & Development'= 0)
attrition$gender <- attrition$Gender %>% recode('Male'=1,'Female'= 0)



##KNN Internal CV


### Model with LogDailyRate,Attrition,Age, JobSatisfaction, LogHourlyRate As Variables
attrition2 <- attrition %>% dplyr::select(LogDailyRate,Attrition,Age, JobSatisfaction,LogHourlyRate)


accs = data.frame(sens = numeric(30), spec = numeric(30), k = numeric(30))

for(i in 1:30)
{

classifications = knn.cv(attrition2[,1:5],attrition2$Attrition, k = i)
CM1<- confusionMatrix(table(attrition2$Attrition, classifications))
accs$sens[i] = CM1$byClass['Sensitivity']
accs$spec[i] = CM1$byClass['Specificity']
accs$k[i] = i
}

plot(accs$k,accs$accuracy, type = "l", xlab = "k")

print(CM1)

print(CM1)



### Model with LogDailyRate,Attrition,Age, JobSatisfaction,LogHourlyRate,LogMonthlyRate, 
## TotalWorkingYears,LogYearsSinceLastPromotion, DistanceFromHome,EmployeeNumber,TrainingTimesLastYear,Education,EnvironmentSatisfaction


attrition3 <- attrition %>% dplyr::select(LogDailyRate,Attrition,Age, JobSatisfaction,LogHourlyRate,LogMonthlyRate,TotalWorkingYears)
attrition3 <- data.frame(attrition3)
classifications_2 = knn.cv(attrition3[,1:4],attrition3$Attrition, k = 3)
CM2<- confusionMatrix(table(attrition3$Attrition, classifications_2))


print(CM2)


#Best Model to Use
attrition4 <- attrition %>% dplyr::select(LogDailyRate,Attrition,Age, JobSatisfaction,LogHourlyRate,LogMonthlyRate,LogYearsSinceLastPromotion, Education,EnvironmentSatisfaction,
                                          YearsWithCurrManager,JobInvolvement, TotalWorkingYears,LogYearsWithCurrManager,JobLevel, JobSatisfaction,RelationshipSatisfaction,)

attrition4 <- data.frame(attrition4)

accs = data.frame(sens = numeric(30), spec = numeric(30), k = numeric(30))

for(i in 1:30)
{
classifications_3 = knn.cv(attrition4[,1:4],attrition4$Attrition, k = i)
CM3<- confusionMatrix(table(attrition4$Attrition, classifications_3))
accs$sens[i] = CM1$byClass['Sensitivity']
accs$spec[i] = CM1$byClass['Specificity']
accs$k[i] = i
}

plot(accs$k,accs$accuracy, type = "l", xlab = "k")


print(CM3)


####Prediction Model

comp_Set_NoAtt = read.csv('CaseStudy2CompSet No Attrition.csv', header= TRUE)


comp_Set_NoAtt$LogYearsInCurrentRole <- log(comp_Set_NoAtt$YearsInCurrentRole + 1)
comp_Set_NoAtt$LogYearsSinceLastPromotion <-  log(comp_Set_NoAtt$YearsSinceLastPromotion + 1)
comp_Set_NoAtt$LogYearsWithCurrManager <- log(comp_Set_NoAtt$YearsWithCurrManager + 1)
comp_Set_NoAtt$LogDailyRate <- log(comp_Set_NoAtt$DailyRate + 1)
comp_Set_NoAtt$LogHourlyRate <- log(comp_Set_NoAtt$HourlyRate + 1)
comp_Set_NoAtt$LogMonthlyRate <- log(comp_Set_NoAtt$MonthlyRate, + 1)


view(comp_Set_NoAtt )

comp_Set_NoAtt$OverTime <- comp_Set_NoAtt$OverTime %>% recode('Yes'=1,'No'= 0)
comp_Set_NoAtt$BusinessTravel <- comp_Set_NoAtt$BusinessTravel %>% recode('Travel_Frequently'=2,'Travel_Rarely'=1,'Non-Travel'= 0)
comp_Set_NoAtt$EducationField <- comp_Set_NoAtt$EducationField %>% recode('Medical'=0,'Life Sciences'=1,'Other'=2, 'Marketing'=3, 'Technical Degree'=4, 'Human Resources'=5)
comp_Set_NoAtt$MaritalStatus <- comp_Set_NoAtt$MaritalStatus %>% recode('Divorced'=2,'Married'=1,'Single'= 0)
comp_Set_NoAtt$JobRole <- comp_Set_NoAtt$JobRole %>% recode('Sales Representative'=0, 'Human Resources'=1,'Laboratory Technician'=2,'Sales Executive'=3, 'Research Scientist'=4, 
                                                            'Healthcare Representative'=5, 'Manager'=6, 'Research Director'=7, 'Manufacturing Director'=7 )
comp_Set_NoAtt$Department <- comp_Set_NoAtt$Department %>% recode('Sales'=2,'Human Resources'=1,'Research & Development'= 0)
comp_Set_NoAtt$gender <- comp_Set_NoAtt$Gender %>% recode('Male'=1,'Female'= 0)


##Using Transformed subseted data for model


comp_Set_NoAtt1 <-  comp_Set_NoAtt %>% dplyr::select(
  LogDailyRate,DistanceFromHome,Age,LogHourlyRate, LogMonthlyRate, 
  TotalWorkingYears,LogYearsSinceLastPromotion, 
  EmployeeNumber,TrainingTimesLastYear,Education,EnvironmentSatisfaction,
  YearsWithCurrManager,JobInvolvement, LogYearsWithCurrManager,
  JobLevel, JobSatisfaction,RelationshipSatisfaction,
  LogYearsInCurrentRole
)


attrition4 <- data.frame(attrition4)
comp_Set_NoAtt <- data.frame(comp_Set_NoAtt1)

####Employee Attrition Prediction Model
Attrition_Predict <- knn(attrition4[,c(1,2)], comp_Set_NoAtt1[,c(1,2)], attrition4$Attrition, k=3)
AttritionPredctd = data.frame(Attrition_Predict)
comp_Set_NoAtt$ID <- AttritionPredctd

Pred_Results = cbind(ID = comp_Set_NoAtt$ID, Attrition = AttritionPredctd)




dim(comp_Set_NoAtt)
dim(attrition4)















## 80% split to Train
set.seed(7)
split_prob = .80
trainIndices = sample(1:dim(attrition)[1],round(split_prob * dim(attrition)[1]))
attrition_train = attrition[trainIndices,]
attrition_test = attrition[-trainIndices,]

test_label <- attrition_test$Attrition
train_label <- attrition_train$Attrition

train <- attrition_train %>% dplyr::select(-Attrition)
test <- attrition_test %>% dplyr::select(-Attrition)



accs = data.frame(sens = numeric(30), spec = numeric(30), k = numeric(30))

for(i in 1:30)
{
classifications = knn.cv(train[,c(1,2)],test[,c(1,2)],train_label, prob = TRUE, k = i)
table(test_label,classifications)
conf_Mat = confusionMatrix(table(test_label, classifications))

}


print(conf_Mat)
## Loop for many k and one training or test partition

accs = data.frame(Accuracy = numeric(30), k = numeric(30))

for(i in 1:30)
{
    classifications = knn(attrition_train[,c(1,2)],attrition_test[,c(1,2)], attrition_train$Attrition, prob = T, k = i)
  table(attrition_test$Attrition, classifications)
  Conf_Mat = confusionMatrix(table(attrition_test$Attrition, classifications))
  accs$accuracy[i] = Conf_Mat$byClass[1]
  accs$k[i] = i
}

plot(accs$k,accs$accuracy, type = "l", xlab = 30)
print(Conf_Mat)

#############################################################################################################################
iterations = 100
numks = 10

masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs1 = data.frame(Accuracy = numeric(10), k = numeric(10))
  trainIndices = sample(1:dim(attrition)[1],round(split_prob * dim(attrition)[1]))
  attrition_train = attrition[trainIndices,]
  attrition_test = attrition[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(attrition_train[,c(1,2)],attrition_test[,c(1,2)],attrition_train$Attrition, prob = TRUE, k = i)
    table(attrition_test$Attrition,classifications)
    Conf_Mat = confusionMatrix(table(attrition_test$Attrition,classifications))
    masterAcc[j,i] = Conf_Mat$byClass[1]
  }
  
}

MeanAcc = colMeans(masterAcc)

plot(seq(1,numks,1),MeanAcc, type = "l")





#######################################################################################################################################

##

#######################################################################################################################################



Log_Model_CompNoAtt <- as.vector(Log_Model_CompNoAtt)

gg_miss_var(comp_Set_NoAtt)



## KNN ##

set.seed(7)
split_prob = .80
trainIndices = sample(1:dim(Log_Model_CompNoAtt)[1],round(split_prob * dim(Log_Model_CompNoAtt)[1]))
Log_Model_CompNoAtt_train = Log_Model_CompNoAtt[trainIndices,]
Log_Model_CompNoAtt_test = Log_Model_CompNoAtt[-trainIndices,]


classifications = knn(Log_Model_CompNoAtt_train[,c(1,3)],Log_Model_CompNoAtt_test[,c(1,2)],Log_Model_CompNoAtt_train, prob = TRUE, k = 8)
table(classifications,Log_Model_CompNoAtt_test)
confusionMatrix(table(classifications,Log_Model_CompNoAtt_test))



comp_classifications = knn(Log_Model_CompNoAtt_train, Log_Model_CompNoAtt, Log_Model_CompNoAtt_train, k = 8)



trainIndices = sample(1:dim(attr)[1],round(splitPerc * dim(attr)[1]))
train = attr[trainIndices,]
test = attr[-trainIndices,]
train_labels = train$Attrition
test_labels = test$Attrition
test = test %>% select(-Attrition)
train = train %>% select(-Attrition)
accs = data.frame(sens = numeric(30), spec = numeric(30), k = numeric(30))
for(i in 1:30)
{
  classifications = knn(train, test, train_labels, prob = TRUE, k = i)
  table(test_labels, classifications)
  CM = confusionMatrix(table(test_labels, classifications))
  accs$sens[i] = CM$byClass['Sensitivity']
  accs$spec[i] = CM$byClass['Specificity']
  accs$k[i] = i
}
accs[is.na(accs)] = 0
print(max(accs$spec))




set.seed(7)
split_prob = .80
trainIndices = sample(1:dim(comp_Set_NoAtt )[1],round(split_prob * dim(comp_Set_NoAtt )[1]))
Log_Model_train = comp_Set_NoAtt [trainIndices,]
Log_Model_test = comp_Set_NoAtt [-trainIndices,]

accs = data.frame( Accuracy = numeric(8), k = numeric(8))

for(i in 1:8)
{
  classifications = knn(Log_Model_train[,c(1,2)], Log_Model_test[,c(1,2)],Log_Model_train$Attrition, prob = TRUE, k = i)
  table(Log_Model_test$Attrition,classifications)
  Conf_Mat = confusionMatrix(table(Log_Model_test$Attrition, classifications))
  accs$accuracy[i] = Conf_Mat$byClass[1]
  accs$k[i] = i
}

plot(accs$k,accs$accuracy, type = "l", xlab = "k")
print(Conf_Mat)



comp_noattr$Attribution =  comp_classifications
comp_noattr = comp_noattr %>% select(ID, Attribution)
write_csv(comp_noattr, 'Case2PredictionsDhillon Attrition.csv')


#######################################################################################################################################

