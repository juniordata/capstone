#Exploratory Analysis
getwd()
setwd("C:/Users/jenni/OneDrive/Documents/Capstone")

#read file
sale<-read.csv('BlackFridayTrain.csv', stringsAsFactors = FALSE)

#the total and percentage of missing value in Product Category 2 and 3
sum(is.na(sale$Product_Category_2))
mean(is.na(sale$Product_Category_2))

sum(is.na(sale$Product_Category_3))
mean(is.na(sale$Product_Category_3))


#removing all NA values 
sale<-na.omit(sale)

#structure of data
str(sale)

#consistent data
#converting gender to binary
sale$Gender <- ifelse(sale$Gender == "M", 0, 1)

#converting age to numeric
sale$Age[sale$Age == "0-17"] <- "15"
sale$Age[sale$Age == "18-25"] <- "21"
sale$Age[sale$Age == "26-35"] <- "30"
sale$Age[sale$Age == "36-45"] <- "40"
sale$Age[sale$Age == "46-50"] <- "48"
sale$Age[sale$Age == "51-55"] <- "53"
sale$Age[sale$Age == "55+"] <- "60"

#converting city category to numeric
sale$City_Category[sale$City_Category == "A"] <- "0"
sale$City_Category[sale$City_Category == "B"] <- "1"
sale$City_Category[sale$City_Category == "C"] <- "2"

#converting stay in currenty city years to numeric
sale$Stay_In_Current_City_Years[sale$Stay_In_Current_City_Years == "4+"] <- "4"


#change variables into types
sale$Gender <- as.numeric(sale$Gender)
sale$Age <- as.numeric(sale$Age)
sale$Occupation <- as.numeric(sale$Occupation)
sale$City_Category <- as.numeric(sale$City_Category)
sale$Stay_In_Current_City_Years <- as.numeric(sale$Stay_In_Current_City_Years)
sale$Marital_Status <- as.numeric(sale$Marital_Status)
sale$Product_Category_1 <- as.numeric(sale$Product_Category_1)
sale$Product_Category_2 <- as.numeric(sale$Product_Category_2)
sale$Product_Category_3 <- as.numeric(sale$Product_Category_3)


#removing user ID and Product ID, large variance
library(caret)
library(dplyr)
salenew <- select(sale, c(-User_ID, -Product_ID, -Product_Category_3))

#Sampling
set.seed(100)
salesample<- salenew$Purchase %>% 
  createDataPartition(p = 0.1, list = FALSE)
salesample<- salenew[salesample,]

#Partitioning
#divide into 80% training for building predictive model
#and 20% test set for evaluating the model
partdata <- createDataPartition(y = salesample$Purchase, 
                               p = 0.8, list=FALSE)
Train <- salesample[partdata, ]
Test <- salesample[partdata, ]



#Dimensionality reduction using PCA
comp<-prcomp(Train[,-2], scale. = T)
#proportion of variance
sd <- comp$sdev
v <- sd^2
pv <- v/sum(v)
#cumulative scree plot
plot(cumsum(pv), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
#10 components results in variance cloase to 99%
#Thus, selecting number of components as 10.

#add a training set with principal components
TrainPCA <- data.frame(Purchase = Train$Purchase, comp$x)
#we are interested in first 10 PCAs
TrainPCA <- TrainPCA[,1:11]
#transform test into PCA
TestPCA <- predict(comp, newdata = Test)
TestPCA <- as.data.frame(TestPCA)
#select the first 10 components
TestPCA <- TestPCA[,1:10]
#make prediction on test data
# it is concluded that this is not need for further analysis as there are only 10 components.


#Modeling

#Decision tree
library(rpart)
library(rpart.plot)
DT = rpart(Purchase ~ .,method="anova", 
           data = Train)
rpart.plot(DT)
#predict decision tree
DT1<- predict(DT, Test)
#evaluting error RMSE
DTerror<- DT1 - Test$Purchase
sqrt(mean(DTerror^2))
#results
data.frame( R2 = R2(DT1, Test$Purchase),
            RMSE = RMSE(DT1, Test$Purchase),
            MAE = MAE(DT1, Test$Purchase))
#10 fold
DT2 = rpart(Purchase ~ .,method="anova", 
            data = Train,
            control = list(cp = 0, xval = 10))
#prediction
DT3 <- predict(DT2, Test)
#RMSE
DTerror1 <- DT3 - Test$Purchase
sqrt(mean(DTerror1^2))
#results
data.frame( R2 = R2(DT3, Test$Purchase),
            RMSE = RMSE(DT3, Test$Purchase),
            MAE = MAE(DT3, Test$Purchase))



#Random Forest
library(randomForest)
RF <- randomForest(Purchase ~ .,data = Train,ntree = 100)
#predict random forest
RF1<- predict(RF, Test)
#RMSE
RFerror<- RF1 - Test$Purchase
sqrt(mean(RFerror^2))
#results
data.frame( R2 = R2(RF1, Test$Purchase),
            RMSE = RMSE(RF1, Test$Purchase),
            MAE = MAE(RF1, Test$Purchase))



#KNN
library(ISLR)
library(caret)
#normalizing
#10-fold
KN <- train(Purchase ~ ., data = Train, method = "kknn", 
      trControl = trainControl("cv",number = 10), 
      preProcess = c("center","scale"), 
      tuneLength = 10)
#predict knn
KN1 <-predict(KN, Test)
#RMSE
KNerror <- KN1 - Test$Purchase
sqrt(mean(KNerror^2))
#results
data.frame( R2 = R2(KN1, Test$Purchase),
            RMSE = RMSE(KN1, Test$Purchase),
            MAE = MAE(KN1, Test$Purchase))
#10-folds repeat 3 times
KN2 <- train(Purchase ~ ., data = Train, method = "kknn", 
       trControl = trainControl("repeatedcv",number = 10, repeats = 3),
       preProcess = c("center","scale"), 
       tuneLength = 10)
#prediction
KN3 <- predict(KN2, Test)
#RMSE
KNerror1 <- KN3 - Test$Purchase
sqrt(mean(KNerror1^2))
#results
data.frame( R2 = R2(KN3, Test$Purchase),
            RMSE = RMSE(KN3, Test$Purchase),
            MAE = MAE(KN3, Test$Purchase))



#Linear Regression
LM <- lm(Purchase ~ .,data = Train)
#prediction
LM1 <- predict(LM, Test)
#RMSE
LMerror <- LM1 - Test$Purchase
sqrt(mean(LMerror^2))
#results
data.frame( R2 = R2(LM1, Test$Purchase),
            RMSE = RMSE(LM1, Test$Purchase),
            MAE = MAE(LM1, Test$Purchase))
#10-folds
LM2 <- train(Purchase ~ ., data = Train, method = "lm", 
      trControl = trainControl("cv",number = 10))
#prediction
LM3 <- predict(LM2, Test)
#RMSE
LMerror1 <- LM3 - Test$Purchase
sqrt(mean(LMerror1^2))
#results
data.frame( R2 = R2(LM3, Test$Purchase),
            RMSE = RMSE(LM3, Test$Purchase),
            MAE = MAE(LM3, Test$Purchase))
#10-folds repeat 3 times
LM4 <- train(Purchase ~ ., data = Train, method = "lm", 
             trControl = trainControl("repeatedcv",number = 10, repeats = 3))
#prediction
LM5 <- predict(LM4, Test)
#RMSE
LMerror2 <- LM5 - Test$Purchase
sqrt(mean(LMerror2^2))
#results
data.frame( R2 = R2(LM5, Test$Purchase),
            RMSE = RMSE(LM5, Test$Purchase),
            MAE = MAE(LM5, Test$Purchase))



