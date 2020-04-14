#Importing Required packages
library(tidyverse) 
library(gridExtra)
library(corrplot)
library(leaps)
library(glmnet)

#Importing the Fifa data file
Fifa<-read.csv("data.csv",header = TRUE)

#Dropping columns
Fifa.dc<-Fifa[-c(1,5,7,11,14,21,24:26,29:54)]

# 0. Investigate data
dim(Fifa.dc) #Dimesion rows and columns in the dataset
names(Fifa.dc) # Column names
str(Fifa.dc) # Data Types of each column
summary(Fifa.dc) # Statistical Summary of the dataset

# Checking for Null Values
sum(is.na(Fifa.dc))  #Checking for Null Values in column
Clean_Data=na.omit(Fifa.dc) # Removing the rows with nulls

#1. Categorising the Position into 4 Categories 

#1 GoalKeeper
#2 Defender
#3 Midfielder
#4 Forward

# Creating a Column with NA's
Clean_Data$FieldPosition <-NA 

# Multiple IF Condition is used to create the 4 Categories mentioned above
Clean_Data$FieldPosition <- ifelse(Clean_Data$Position=='GK','GoalKeeper',
                                   ifelse(Clean_Data$Position=='CB'| Clean_Data$Position=='LB'| Clean_Data$Position=='LCB'|Clean_Data$Position=='LWB'|Clean_Data$Position=='RB'|Clean_Data$Position=='RCB'|Clean_Data$Position=='RWB','Defender', 
                                          ifelse(Clean_Data$Position=='RM'| Clean_Data$Position=='RDM'| Clean_Data$Position=='RCM'|Clean_Data$Position=='RAM'|Clean_Data$Position=='LM'|Clean_Data$Position=='LDM'|Clean_Data$Position=='LCM'|Clean_Data$Position=='LAM'|Clean_Data$Position=='CM'|Clean_Data$Position=='CDM'|Clean_Data$Position=='CAM','Midfielder','Forward')))  

#2. Preparation of Dataset for K- Means Clustering

# Removing rows with values ="Unknown" and Goalkeeper Position
cluster_data <- Clean_Data %>%
  filter(FieldPosition != "Unknown") %>%
  filter(FieldPosition != "GK")

# Removing Categorical variables 
Final.Data<-cluster_data[-c(1:4,6:8,9:19,54:60)]
fifa_data<-Final.Data[-c(31:35)]

#3 . Performing k- Means Clustering
set.seed(20)  
Fifa_Cluster <- kmeans(fifa_data[,1:30], 8, nstart = 20) # Specifying column 13 and 29 with 8 groups 
Vector<-Fifa_Cluster$cluster

# Table to show The Cluster and split for the three categories
table(Fifa_Cluster$cluster, cluster_data$FieldPosition)

L#4. Creating a Player List Data Frame with Cluster Result added 
cluster_data[,"Cluster"] <-Fifa_Cluster$cluster
Player_Dataset<-cluster_data[-c(1,5,6,10:55)]

#5. Exporting the dataset as a csv file to create a Dashboard in Tableau
write.csv(Player_Dataset,'\\Users\\deepak\\Documents\\DEAN\\STAT515\\FinalProject\\Player_Daa\.csv', row.names = FALSE)
