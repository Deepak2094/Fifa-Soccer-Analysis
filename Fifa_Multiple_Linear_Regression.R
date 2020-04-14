#Importing Required packages
library(tidyverse) 
library(gridExtra)
library(corrplot)
library(leaps)
library(glmnet)
library(e1071) 


#Importing the Fifa data file
Fifa<-read.csv("data.csv",header = TRUE)

#Dropping columns
Fifa.dc<-Fifa[-c(1,5,7,11,14,21,24:26,29:54)]

# Content
# 0. Investigate data
# 1. Correlation Plot
# 2. Developing Linear Regression Model
# 3. Redeveloping the regression model to by removing Non Significant Variables
# 4. Plotting Linear Regression Model related Graphs
# 5. Creating Test and Train Dataset
# 6. Prediction of Values on Test Dataset
# 7. Calculation of Model Accuracy and RMSE

# 0. Investigate data
dim(Fifa.dc) #Dimesion rows and columns in the dataset
names(Fifa.dc) # Column names
str(Fifa.dc) # Data Types of each column
summary(Fifa.dc) # Statistical Summary of the dataset

# Checking for Null Values
sum(is.na(Fifa.dc))  #Checking for Null Values in column
Clean_Data=na.omit(Fifa.dc) # Removing the rows with nulls
sum(is.na(Clean_Data)) # Rechecking for NA's in the dataset


# 1. Plotting the Correlation Plot
# Correlations for quantitive variables.Put the predictor variable, "Overall",in the top row and left column
# of the array of correlations.
vars <- select(Clean_Data,-Name,-Nationality,-Club,-Preferred.Foot,-Work.Rate,-Position,
               -Body.Type,-ID) %>%
  select(Overall,Age,Crossing,Finishing,HeadingAccuracy,ShortPassing,Dribbling,Curve,FKAccuracy,LongPassing,
           BallControl,Acceleration,SprintSpeed,Reactions,Balance,ShotPower,Jumping,Stamina,Strength,LongShots,
           Positioning,Vision,Composure,Marking,StandingTackle,SlidingTackle,GKDiving,GKHandling,GKKicking,
           GKPositioning,GKReflexes)
corrplot(cor(vars), type="upper", method="shade",
         tl.srt=45, addCoef.col="black",
         addCoefasPercent=TRUE)

# 2. Develop a regression model to predict the Player's overall rating 
# from the various atrributes assigned to a player

Fifa.lm <- lm(Overall ~ Age+Value.in.Million.Euros+Wage.in.Thousand.Euros+International.Reputation+Weak.Foot+Skill.Moves+International.Reputation+
                Crossing+Finishing+HeadingAccuracy+ShortPassing+Dribbling+Curve+FKAccuracy+LongPassing+	
                BallControl+Acceleration+SprintSpeed+Reactions+Balance+ShotPower+Jumping+Stamina+Strength+LongShots+
                Positioning+Vision+Composure+Marking+StandingTackle+SlidingTackle+GKDiving+GKHandling+GKKicking+
                GKPositioning+GKReflexes, data=Clean_Data)

#Significance for Variable
summary(Fifa.lm) 

Overall<-Clean_Data$Overall
skewness(Overall)

# 3.Redeveloping the regression model to by removing Non Significant Variables

Fifa.lm <- lm(Overall ~ Age+Value.in.Million.Euros+Wage.in.Thousand.Euros+International.Reputation+Skill.Moves+International.Reputation+
                Crossing+Finishing+HeadingAccuracy+ShortPassing+LongPassing+	
                BallControl+Acceleration+SprintSpeed+Reactions+Balance+ShotPower+Jumping+Stamina+Strength+LongShots+
                Positioning+Vision+Composure+Marking+StandingTackle+SlidingTackle+GKDiving+GKHandling+GKKicking+
                GKPositioning+GKReflexes, data=Clean_Data)

#Significance for Variable
summary(Fifa.lm) 

# 4.Plotting Graphs for the Linear Model to show the Performance and Check for Linearity Assumptions

# Summary Plots
par(mfrow=c(2,2))
plot(Fifa.lm)

#Plotting the Residual Vs Fitted Values plot 
ggplot(data=Fifa.lm,
       aes(x=.fitted, y=.resid)) +
  geom_point( ) +
  geom_hline(yintercept=0) + 
  geom_smooth(se=TRUE) +
  labs(x="fitted Values", y="Residuals")

# 5.Creating the Test and Train Dataset.The return for this is row nos.
    # Training-Test Split (80% - 20%)
set.seed(1)
row.number <- sample(1:nrow(Clean_Data),0.8*nrow(Clean_Data))
train = Clean_Data[row.number,]
test = Clean_Data[-row.number,]
dim(train)
dim(test)

#5 . Predicting the Values on the Test dataset
pred <- predict(Fifa.lm, newdata = test)

Overall_Test<-test[-c(1:4,6:54)]
Predicted_Test<-data.frame(pred)
Pred_Vs_Actual<-cbind(Predicted_Test,Overall_Test)

#6 Results:
# Root Mean Square Error : 2.406
# R -Squared : 88.03%
# Calculating RMSE and R2 
RMSE<-sqrt(mean((test$Overall-pred)^2)) 
c(RMSE = RMSE, R2=summary(Fifa.lm)$r.squared)





