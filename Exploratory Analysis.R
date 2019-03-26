#Exploratory Analysis
getwd()
setwd("C:/Users/jenni/OneDrive/Documents/Capstone")

#read file
sale<-read.csv('BlackFriday.csv', stringsAsFactors = FALSE)

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

#adding a new column when customer has higher purchasing amount, set as 1st quertile
High = ifelse(sale$Purchase<=7871, "No", "Yes")
sale = data.frame(sale, High)

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


#Sampling
library(caret)
library(dplyr)
set.seed(100)
training.samples <- sale$Purchase %>% 
  createDataPartition(p = 0.8, list = FALSE)


#divide into 80% training for building predictive model
#and 20% test set for evaluating the model
Train<- sale[training.samples,]
Test <- sale[-training.samples,]


#Modeling

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


#Decision tree
library(rpart)
DT = rpart(High ~ Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years+Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3,data = Train)
plot(DT)
text(DT)
DT1<- predict(DT, Test, type="class")
#evaluting error
with(Test, table(DT1, High))

#Random Forest
library(randomForest)
trainrantree = randomForest(Purchase ~ .,data = Train,ntree = 100)


#knn
library(kknn)
m2 = kknn(train=TrainPCA, test=TestPCA, k=10)
Conf_matrix = table(pred_knn, trainSet$Purchase)
model_kknn <- kknn(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`, trainSet, test = testSet,k=10, kernel="rectangular", distance=2)
pred_knn$fitted.values

predictedKNN = predict(pred_knn,testSet)

accuracy = sum(diag(Conf_matrix))/2000
RMSE_knn <- sqrt(mean((testSet$Purchase - pred_knn$fitted.values)^2))

#10 fold cross validation
library(klaR)
set.seed(100)
control <- trainControl(method = "cv", number = 3)
# Train the model
model <- train(Purchase ~., data = Train, method = "lm",trControl = control)
# Summarize the results
print(model)



