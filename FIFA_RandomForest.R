library(corrplot)
library(tidyverse)
library(doSNOW)
library(dummies)
library(Boruta)
library(caTools)
library(randomForest)

citation(randomForest)

#Importing the Fifa data file
Fifa<-read.csv("data.csv",header = TRUE)

#Dropping columns
Fifa.dc<-Fifa[-c(1:3,5:7,10:11,14,19:21:26,29:54)]

#Contents
# 0. Investigate data
# 1. Splitting Test and Train Dataset
# 2. Creating Random Forest on Train dataset for different mtry
# 3.Print and plot the variable-importance measures
# 4. Results

# 0. Investigate data
dim(Fifa.dc) #Dimesion rows and columns in the dataset
names(Fifa.dc) # Column names
str(Fifa.dc)
summary(Fifa.dc)

# Checking for Null Values
sum(is.na(Fifa.dc)) 
Clean_Data=na.omit(Fifa.dc)
sum(is.na(Clean_Data))

# 1.Create a training data set so can compare test MSE with 
# that from regression trees
#Train Data-Test Data 50%-50%
set.seed(5)
train = sample(1:nrow(Clean_Data), nrow(Clean_Data)/2)
Fifa.test=Clean_Data[-train,"Overall"]

# 2.Create a random forest with 500 trees and performing prediction on Test Data

#mtry =6
set.seed(123)
rf.Fifa=randomForest(Overall~.,data=Clean_Data,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.Fifa,newdata=Clean_Data[-train,])
mean((yhat.rf-Fifa.test)^2)
sqrt(mean((yhat.rf-Fifa.test)^2))
importance(rf.Fifa)
varImpPlot(rf.Fifa)
rf.Fifa$importance
rf.Fifa


#mtry =12
set.seed(123)
rf.Fifa=randomForest(Overall~.,data=Clean_Data,subset=train,mtry=12,importance=TRUE)
yhat.rf = predict(rf.Fifa,newdata=Clean_Data[-train,])
mean((yhat.rf-Fifa.test)^2)
sqrt(mean((yhat.rf-Fifa.test)^2))

# 3.Print and plot the variable-importance measures
importance(rf.Fifa)
varImpPlot(rf.Fifa)
rf.Fifa$importance
rf.Fifa



#4. Results

    #mtry  #Number of Trees  #MeanofSquareResiduals  #% Var Explained
#1  6      500               0.6791805              98.62  %
#2  12     500               0.4647257               99.05 %

