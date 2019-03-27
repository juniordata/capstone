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
sale$User_ID <- as.factor(sale$User_ID)
sale$Gender <- as.numeric(sale$Gender)
sale$Age <- as.numeric(sale$Age)
sale$Occupation <- as.numeric(sale$Occupation)
sale$City_Category <- as.numeric(sale$City_Category)
sale$Stay_In_Current_City_Years <- as.numeric(sale$Stay_In_Current_City_Years)
sale$Marital_Status <- as.numeric(sale$Marital_Status)
sale$Product_Category_1 <- as.numeric(sale$Product_Category_1)
sale$Product_Category_2 <- as.numeric(sale$Product_Category_2)
sale$Product_Category_3 <- as.numeric(sale$Product_Category_3)


#removing user ID and Product ID, 0 variance
library(caret)
library(dplyr)
salenew <- sale %>%
  select(-User_ID, -Product_ID)

#Sampling
set.seed(100)
salesample<- salenew$Purchase %>% 
  createDataPartition(p = 0.8, list = FALSE)
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
DT = rpart(Purchase ~ Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years+Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3,data = Train)
plot(DT)
text(DT)
#predict decision tree
DT1<- predict(DT, Test)
#evaluting error
DTerror<- DT1 - Test$Purchase
sqrt(mean(DTerror^2))

#Random Forest
library(randomForest)
RF = randomForest(Purchase ~ Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years+Marital_Status+Product_Category_1,data = Train,ntree = 100)
#predict random forest
RF1<- predict(RF, Test)
#RMSE
RFerror<- RF1 - Test$Purchase
sqrt(mean(RFerror^2))

#KNN
set.seed(111)
kn <- knn(Train,Test,cl=Train$Purchase,k=10)
#Confusion Matrix
matrix <- table(kn,Train$Purchase)
#accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(matrix)

#Testing
sale1<-read.csv('BlackFridayTest.csv', stringsAsFactors = FALSE)
#removing all NA values 
sale1<-na.omit(sale)

#structure of data
str(sale1)

#consistent data
#converting gender to binary
sale1$Gender <- ifelse(sale$Gender == "M", 0, 1)

#converting age to numeric
sale1$Age[sale1$Age == "0-17"] <- "15"
sale1$Age[sale1$Age == "18-25"] <- "21"
sale1$Age[sal1e$Age == "26-35"] <- "30"
sale1$Age[sale1$Age == "36-45"] <- "40"
sale1$Age[sale1$Age == "46-50"] <- "48"
sale1$Age[sale1$Age == "51-55"] <- "53"
sale1$Age[sale1$Age == "55+"] <- "60"

#converting city category to numeric
sale1$City_Category[sale1$City_Category == "A"] <- "0"
sale1$City_Category[sale1$City_Category == "B"] <- "1"
sale1$City_Category[sale1$City_Category == "C"] <- "2"

#converting stay in currenty city years to numeric
sale1$Stay_In_Current_City_Years[sale1$Stay_In_Current_City_Years == "4+"] <- "4"

#removing user ID and Product ID, 0 variance
library(caret)
library(dplyr)
salenew1 <- sale1 %>%
  select(-User_ID, -Product_ID)



