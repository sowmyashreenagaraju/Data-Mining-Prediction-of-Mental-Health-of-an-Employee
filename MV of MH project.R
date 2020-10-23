library(caTools)
library(dplyr)
library(tidyverse)
library(ggforce)
library(ggrepel)
library(gridExtra)
library(class)
library(gmodels)
set.seed(125)


df<-read.csv("/Users/sowmyageet/Desktop/Data.csv")
df<-df[,-1]


# Gender
df$Gender<-tolower(df$Gender)
df[which(df$Gender %in% c("male","male","man","cis male","male.","male 9:1 female, roughly","male (cis)","nb masculine",
                          "sex is male","malr","mtf","dude",
                          "mail","m|","male/genderqueer","male (trans, ftm)","cisdude","cis man","cis-male",
                          "male/androgynous","cis hetero male","m",
                          "male, cis","cis male","male-ish","ostensibly male",
                          "malel","cisgender male","identify as male",
                          "masculine","cishet male","masculino","make")),39]<-"Male"


df[which(df$Gender %in% c("female","female ","cis female "," female","femalw","femail",
                          "female (cisgender)","female (cis)","cis-female","cis female","cis woman",
                          "femile")),39]<-"Female"

df[!(df$Gender=="Male"|df$Gender=="Female"),39]<-"Trans"
df$Gender<-factor(df$Gender)

# Age

df_age<-data.frame(df[which(df$Age >= 18 & df$Age<= 75),38])
age<-mean(df_age$df.which.df.Age....18...df.Age....75...38.)
age<-round(age,digits = 0)
df[(which(df$Age < 18 | df$Age > 75 | is.na(df$Age))),38]<-age


# Work interference
df$InterferenceWithWorkEnv <- 
  ifelse(!is.na(df$InterferenceWithWorkEnv) & df$InterferenceWithWorkEnv == 'Never',0,1)
df$InterferenceWithWorkEnv <- factor(df$InterferenceWithWorkEnv)

# Remove
rm(age,df_age)

# Data Preparation

MentalHealth<-df[,c("Age","Gender","FamilyHistory","MentalHealthBenefits","OptionsAvailable",
                    "AnonymityProtected","RequestingMedicalLeave","InterferenceWithWorkEnv",
                    "PersonalHistory","MentalHealthDisorder")]
# Finding the missing values
sapply(MentalHealth, function(x) sum(is.na(x)))
MentalHealth <- MentalHealth[!is.na(MentalHealth$FamilyHistory),]
MentalHealth <- MentalHealth[!is.na(MentalHealth$PersonalHistory),]

must_convert<-sapply(MentalHealth,is.factor) 
M2<-sapply(MentalHealth[,must_convert],unclass)  
out<-cbind(MentalHealth[,!must_convert],M2) 
rm(M2)

# Converting to dummy variables
Dummy_df <- fastDummies::dummy_cols(out, select_columns = c("Gender","MentalHealthBenefits","OptionsAvailable",
                                                            "AnonymityProtected","RequestingMedicalLeave","PersonalHistory"))


Dummy_df<-Dummy_df[,-c(4:9)]

#


sample <- sample.split(Dummy_df, SplitRatio = 0.80)
train <- subset(Dummy_df, sample == TRUE)
test <- subset(Dummy_df, sample == FALSE)
MentalHealth_train<-train[,-4]
MentalHealth_test<-test[,-4]
train.def<-train$MentalHealthDisorder
test.def<-test$MentalHealthDisorder
  
# KNN Method
#For k=1
knn_pred.result1 <- knn(MentalHealth_train, MentalHealth_test, train.def, k=1 )
table(knn_pred.result1, test.def)
misClassificError <- mean(knn_pred.result1 != test.def)
print(paste('Accuracy', 1-misClassificError))
rm(knn_pred.result1)
#For k=2
knn_pred.result2 <- knn(MentalHealth_train, MentalHealth_test, train.def, k=2 )
table(knn_pred.result2, test.def)
misClassificError <- mean(knn_pred.result2 != test.def)
print(paste('Accuracy', 1-misClassificError))
rm(knn_pred.result2)
#For k=3
knn_pred.result3 <- knn(MentalHealth_train, MentalHealth_test, train.def, k=3 )
table(knn_pred.result3, test.def)
misClassificError <- mean(knn_pred.result3 != test.def)
print(paste('Accuracy', 1-misClassificError))
rm(knn_pred.result3)
#For k=4
knn_pred.result4 <- knn(MentalHealth_train, MentalHealth_test, train.def, k=4 )
table(knn_pred.result4, test.def)
misClassificError <- mean(knn_pred.result4 != test.def)
print(paste('Accuracy', 1-misClassificError))
rm(knn_pred.result4)
#For k=5
knn_pred.result5 <- knn(MentalHealth_train, MentalHealth_test, train.def, k=5 )
table(knn_pred.result5, test.def)
misClassificError <- mean(knn_pred.result5 != test.def)
print(paste('Accuracy', 1-misClassificError))
rm(knn_pred.result5)
#For k=6
knn_pred.result6 <- knn(MentalHealth_train, MentalHealth_test, train.def, k=6 )
table(knn_pred.result6, test.def)
misClassificError <- mean(knn_pred.result6 != test.def)
print(paste('Accuracy', 1-misClassificError))
rm(knn_pred.result6)
#For k=7
knn_pred.result7 <- knn(MentalHealth_train, MentalHealth_test, train.def, k=7 )
table(knn_pred.result7, test.def)
misClassificError <- mean(knn_pred.result7 != test.def)
print(paste('Accuracy', 1-misClassificError))
rm(knn_pred.result7)
#For k=8
knn_pred.result8 <- knn(MentalHealth_train, MentalHealth_test, train.def, k=8 )
table(knn_pred.result8, test.def)
misClassificError <- mean(knn_pred.result8 != test.def)
print(paste('Accuracy', 1-misClassificError))
rm(knn_pred.result8)
#For k=9
knn_pred.result9 <- knn(MentalHealth_train, MentalHealth_test, train.def, k=9 )
table(knn_pred.result9, test.def)
misClassificError <- mean(knn_pred.result9 != test.def)
print(paste('Accuracy', 1-misClassificError))
rm(knn_pred.result9)
#For k=10
knn_pred.result10 <- knn(MentalHealth_train, MentalHealth_test, train.def, k=10 )
table(knn_pred.result10, test.def)
misClassificError <- mean(knn_pred.result10 != test.def)
print(paste('Accuracy', 1-misClassificError))
rm(knn_pred.result10)

# K = 4 got the highest accuracy of 76 % 

rm()
#-------------------------------------------------------------------------

# Logistic regression

table(MentalHealth$Gender)
table(MentalHealth$FamilyHistory)
table(MentalHealth$MentalHealthBenefits)
table(MentalHealth$OptionsAvailable)
table(MentalHealth$AnonymityProtected)
table(MentalHealth$RequestingMedicalLeave)
table(MentalHealth$InterferenceWithWorkEnv)
table(MentalHealth$PersonalHistory)
table(MentalHealth$MentalHealthDisorder)


# age categorization

MentalHealth$Age<-cut(MentalHealth$Age, breaks = c(0, 16, 34, 60, 75), labels = c('Fresh', 'Associate', 'Senior', 'Manager'))
table(MentalHealth$Age)

# family History

MentalHealth$FamilyHistory <- 
            ifelse(!is.na(MentalHealth$FamilyHistory) & MentalHealth$FamilyHistory == 1,'Yes','No')
MentalHealth$FamilyHistory <- as.factor(MentalHealth$FamilyHistory)

# Intereference with work

MentalHealth$InterferenceWithWorkEnv <- 
  ifelse(!is.na(MentalHealth$InterferenceWithWorkEnv) & MentalHealth$InterferenceWithWorkEnv == 1,'Yes','No')

MentalHealth$InterferenceWithWorkEnv <- as.factor(MentalHealth$InterferenceWithWorkEnv)

summary(MentalHealth)

lml <- glm(MentalHealthDisorder~.,data = MentalHealth, family = 'binomial')
summary(lml)
coef(lml)

# Splitting the data
levels(MentalHealth$MentalHealthDisorder)
levels(MentalHealth$MentalHealthDisorder)[1] <- "No"
sample <- sample.split(MentalHealth, SplitRatio = 0.8)
train <- subset(MentalHealth, sample == TRUE)
test <- subset(MentalHealth, sample == FALSE)

lml_train <- glm(MentalHealthDisorder~ ., data = train, family = 'binomial', control = glm.control(maxit = 50))
summary(lml)

# Tried using Bayesglm method to avoid the fitting

library(arm)
lm2 <- bayesglm(MentalHealthDisorder~ ., data = train, family = 'binomial',contrasts = NULL)
summary(lm2)

# Predictions on train data
train$pred_prob <- predict(lm2, train, type = 'response')
train$predict <- ifelse(train$pred_prob < 0.5, 'No', 'Yes')

# Predictions on the train set
train$pred_prob <- predict(lml_train, train, type = 'response')
train$predict <- ifelse(train$pred_prob < 0.5, 'No', 'Yes')

#Confusion matrix for training

cm_train <- table(train$MentalHealthDisorder,train$predict, dnn = c("real", "predict"))
cm_train

paste('Accuracy:', round(( cm_train['Yes','Yes'] + cm_train['No','No'] ) / sum(cm_train),2))
paste('Precision:', round(cm_train['Yes','Yes'] / sum(cm_train['Yes',]),2))
paste('Recall:', round(cm_train['Yes','Yes'] / sum(cm_train[,'Yes']),2))

# Predictions on the train set
test$pred_prob <- predict(lml, test, type = 'response')
test$predict <- ifelse(test$pred_prob < 0.5, 'No', 'Yes')

# Confusion Matrix for test data

cm_test <- table(test$MentalHealthDisorder,test$predict, dnn = c("real", "predict"))
cm_test

paste('Accuracy:', round(( cm_test['Yes','Yes'] + cm_test['No','No'] ) / sum(cm_test),2))
paste('Precision:', round(cm_test['Yes','Yes'] / sum(cm_test['Yes',]),2))
paste('Recall:', round(cm_test['Yes','Yes'] / sum(cm_test[,'Yes']),2))

# Optimizing the logistic regression model using step AIC
library(MASS)

step.model <- lml_train %>% stepAIC(trace = FALSE)
coef(step.model)

#Predictions
probabilities <- predict(step.model, test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")
cm_1 <- table(test$MentalHealthDisorder, predicted.classes, dnn = c("real", "predict"))
cm_1
paste('Accuracy:', round(( cm_1['Yes','Yes'] + cm_1['No','No'] ) / sum(cm_1),2))


#Naive Bayes
x <- train[,-10]
y <- train$MentalHealthDisorder

library(e1071)
library(caret)

NBModel <- naiveBayes(x , y , trControl=trainControl(method='cv',number=10))
NBModel

NBPredictTraining <- predict(NBModel, newdata = train)
NBtrainTable=table(train$MentalHealthDisorder, NBPredictTraining)
trainAcc=(NBtrainTable[1,1]+NBtrainTable[2,2])/sum(NBtrainTable)

NBPredictTest <- predict(NBModel, newdata = test)
NBtestTable=table(test$MentalHealthDisorder, NBPredictTest)
testAcc=(NBtestTable[1,1]+NBtestTable[2,2])/sum(NBtestTable)
print(paste('Training Accuracy =', trainAcc))
print(paste('Testing Accuracy =', testAcc))  
  