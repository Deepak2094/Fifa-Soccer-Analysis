library(corrplot)
library(tidyverse)
library(doSNOW)
library(dummies)
library(Boruta)
library(caTools)
library(randomForest)
library(xgboost)
library(AppliedPredictiveModeling)
library(caret)
install.packages("gbm")
library(gbm)
library(ggthemes)
library(scales)

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


#Train Data-Test Data 50%-50%
set.seed(5)
train = sample(1:nrow(Clean_Data), nrow(Clean_Data)/2)
Fifa.test=Clean_Data[-train,"Overall"]

# Gradient Boost
set.seed(123)
Fifa.boost = gbm(Overall~.,data=Clean_Data[train,], distribution = "gaussian",n.trees =500,interaction.depth=4, shrinkage=0.01)
summary(Fifa.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance

importance(Fifa.boost)
varImpPlot(Fifa.boost)
Fifa.boost$importance
Fifa.boost

# summarize model
Fifa_Effects <- tibble::as_tibble(gbm::summary.gbm(Fifa.boost, 
                                                   plotit = FALSE))
Fifa_Effects %>% utils::head()

# plot effects
Fifa_Effects %>% 
  # arrange descending to get the top influencers
  dplyr::arrange(desc(rel.inf)) %>%
  # sort to top 10
  dplyr::top_n(10) %>%
  # plot these data using columns
  ggplot(aes(x = forcats::fct_reorder(.f = var, 
                                      .x = rel.inf), 
             y = rel.inf, 
             fill = rel.inf)) +
  geom_col() +
  # flip
  coord_flip() +
  # format
  scale_color_brewer(palette = "Dark2") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  xlab('Features') +
  ylab('Relative Influence') +
  ggtitle("Top 10 Drivers of Player Ratings")


#Plot of Response variable with lstat variable
plot(Fifa.boost,i="Age") 
#Inverse relation with lstat variable

plot(Fifa.boost,i="Value.in.Million.Euros") 
#as the average number of rooms increases the the price increases

#### --------------- Preicting the error on Test set ------------------------
n.trees = seq(from=100 ,to=500, by=100) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(Fifa.boost,Clean_Data[-train,],n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error<-with(Clean_Data[-train,],apply( (predmatrix-Overall)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged


#Plotting the test error vs number of trees
plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")


