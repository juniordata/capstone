#check and set working directory
getwd()
setwd("C:/Users/jenni/OneDrive/Documents/Capstone")

#read file
sale<-read.csv('BlackFriday.csv',stringsAsFactors = FALSE)

#explore data type/class
sapply(sale, class)

#summary on sale dataset
summary(sale)

#structure of dataset
str(sale)

#check for NA values
sum(is.na(sale))

#check for NA on each column
sum(is.na(sale$Product_ID))
sum(is.na(sale$Gender))
sum(is.na(sale$Age))
sum(is.na(sale$Occupation))
sum(is.na(sale$City_Category))
sum(is.na(sale$Stay_In_Current_City_Years))
sum(is.na(sale$Marital_Status))
sum(is.na(sale$Product_Category_1))
sum(is.na(sale$Product_Category_2))
sum(is.na(sale$Product_Category_3))

#remove NA values
sale[is.na(sale)] <-0

#plot amount of males and females
plot(sale$Gender)

#plot age group
plot(sale$Age)

#plot histogram on Purchases
hist(sale$Purchase)

#install ggplot
library(ggplot2)
#Plot graph to show the amount purchased between male and female among different age group.
ggplot(sale, aes(x=Age, fill=Gender, colour=Gender)) + geom_bar(position=position_dodge())

#average amount of purchase, sorted by male and female
sapply(split(sale$Purchase, sale$Gender), mean)

#change variables into factor
sale$Product_ID <- as.factor(sale$Product_ID)
sale$Gender <- as.factor(sale$Gender)
sale$Age <- as.factor(sale$Age)
sale$Occupation <- as.factor(sale$Occupation)
sale$City_Category <- as.factor(sale$City_Category)
sale$Stay_In_Current_City_Years <- as.factor(sale$Stay_In_Current_City_Years)
sale$Marital_Status <- as.factor(sale$Marital_Status)
sale$Product_Category_1 <- as.factor(sale$Product_Category_1)
sale$Product_Category_2 <- as.factor(sale$Product_Category_2)
sale$Product_Category_3 <- as.factor(sale$Product_Category_3)

#divide into training and test set
data <- sample(nrow(sale), floor(nrow(sale) * 0.8))
Train<- sale[data,]
Test <- sale[-data,]

#Decision Tree
library(rpart)
library(rpart.plot)
train = sample (1:nrow(sale), nrow(sale)/2)
set.seed(1)
treetrain<-rpart(Purchase~.,subset=train, data=sale)
rpart.plot(treetrain, type = 4)
