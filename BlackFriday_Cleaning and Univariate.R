#check and set working directory
getwd()
setwd("C:/Users/jenni/OneDrive/Documents/Capstone")

#read file
sale<-read.csv('BlackFriday.csv',stringsAsFactors = FALSE)

#head of dataframe
head(sale)

#explore data type/class
sapply(sale, class)
sapply(sale, typeof)

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

#check again for any missing values
sum(is.na(sale))

#summary on numeric variables in sale dataset
summary(sale$User_ID)
summary(sale$Occupation)
summary(sale$Marital_Status)
summary(sale$Product_Category_1)
summary(sale$Product_Category_2)
summary(sale$Product_Category_3)
summary(sale$Purchase)

#univariate analysis of categorical variables
#Product ID, the first 6 products
library(dplyr)
library(tidyr)
library(pander)
sale %>%
  group_by(Product_ID) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(relative_frequency = frequency/sum(frequency)) %>%
  head
#Gender
sale %>%
  group_by(Gender) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(relative_frequency = frequency/sum(frequency)) %>%
  pander
#Age
sale %>%
  group_by(Age) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(relative_frequency = frequency/sum(frequency)) %>%
  pander
#City Category
sale %>%
  group_by(City_Category) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(relative_frequency = frequency/sum(frequency)) %>%
  pander
#Stay In Current City years
sale %>%
  group_by(Stay_In_Current_City_Years) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(relative_frequency = frequency/sum(frequency)) %>%
  pander



#checking for outliers in genders
library(ggplot2)
qplot(x=Gender, y=Purchase,data=subset(sale, !is.na(Gender)),geom='boxplot')

#consistant data
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


#change variables into numeric
sale$Product_ID <- as.numeric(sale$Product_ID)
sale$Gender <- as.numeric(sale$Gender)
sale$Age <- as.numeric(sale$Age)
sale$Occupation <- as.numeric(sale$Occupation)
sale$City_Category <- as.numeric(sale$City_Category)
sale$Stay_In_Current_City_Years <- as.numeric(sale$Stay_In_Current_City_Years)
sale$Marital_Status <- as.numeric(sale$Marital_Status)
sale$Product_Category_1 <- as.numeric(sale$Product_Category_1)
sale$Product_Category_2 <- as.numeric(sale$Product_Category_2)
sale$Product_Category_3 <- as.numeric(sale$Product_Category_3)

#check data structure
str(sale)

#check variance on each variable
var(sale$User_ID)
var(sale$Product_ID)
var(sale$Gender)
var(sale$Age)
var(sale$Occupation)
var(sale$City_Category)
var(sale$Stay_In_Current_City_Years)
var(sale$Marital_Status)
var(sale$Product_Category_1)
var(sale$Product_Category_2)
var(sale$Product_Category_3)

#corplot for the dataset
library(ggcorrplot)
sale %>%
  select_if(is.numeric) %>%
  cor %>% 
  ggcorrplot()

#histograms on all variables
library(psych)
library(plyr)
multi.hist(sale[,sapply(sale, is.numeric)])



