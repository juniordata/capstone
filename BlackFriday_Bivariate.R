#check and set working directory
getwd()
setwd("C:/Users/jenni/OneDrive/Documents/Capstone")

#read file
sale<-read.csv('BlackFriday.csv',stringsAsFactors = FALSE)

#remove NA values
sale[is.na(sale)] <-0

#bivariate analysis
#change variables into factor
sale$Product_ID <- as.factor(sale$Product_ID)
sale$Gender <- as.factor(ifelse(sale$Gender=='M', 'Male', 'Female'))
sale$Age <- as.factor(sale$Age)
sale$Occupation <- as.factor(sale$Occupation)
sale$City_Category <- as.factor(sale$City_Category)
sale$Stay_In_Current_City_Years <- as.factor(sale$Stay_In_Current_City_Years)
sale$Marital_Status <- as.factor(ifelse(sale$Marital_Status == 0, 'Single', 'Married'))
sale$Product_Category_1 <- as.factor(sale$Product_Category_1)
sale$Product_Category_2 <- as.factor(sale$Product_Category_2)
sale$Product_Category_3 <- as.factor(sale$Product_Category_3)



#Plot graph to show the amount purchased between male and female among different age group.
library(ggplot2)
ggplot(sale, aes(x=Age, fill=Gender, colour=Gender)) + geom_bar(position=position_dodge())

#Gender and Marital Status
table(sale$Gender, sale$Marital_Status)
ggplot(sale, aes(x=Gender, fill=Marital_Status, colour=Marital_Status)) + geom_bar(position=position_dodge())

#Occupation and Gender
ggplot(sale, aes(x=Occupation, fill=Gender, colour=Gender)) + geom_bar(position=position_dodge())
#Occupation and Martial Status
ggplot(sale, aes(x=Occupation, fill=Marital_Status, colour=Marital_Status)) + geom_bar(position=position_dodge())

#Stay In Current City Years and City Category  
ggplot(sale, aes(x=Stay_In_Current_City_Years, fill=City_Category, colour=City_Category)) + geom_bar(position=position_dodge())

#City Category and Age
ggplot(sale, aes(x=City_Category, fill=Age, colour=Age)) + geom_bar(position=position_dodge())




#Martial Status, Gender, and purchase
ggplot(sale,aes(x=Age,fill=Marital_Status))+geom_bar(position = "dodge")+facet_grid(Gender~.)

#Product ID, Gender and Occupation
ggplot(sale, aes(x=Occupation, fill=Gender, colour=Marital_Status)) + geom_bar(position=position_dodge())

#Age, Gender, and Martial Status
ggplot(sale, aes(x=Occupation, fill=Age))+geom_bar()+facet_grid(Gender~Marital_Status)

#Stay In Currenty City Years and Age and City Category
ggplot(sale, aes(x=Age,fill=Stay_In_Current_City_Years))+geom_bar()+facet_grid(City_Category~ Stay_In_Current_City_Years)




#Multivariate Analysis
#converting gender to binary
sale$Gender <- ifelse(sale$Gender == "M", 0, 1)
sale$Gender <- as.numeric(sale$Gender)


#converting age to numeric
sale$Age[sale$Age == "0-17"] <- "15"
sale$Age[sale$Age == "18-25"] <- "21"
sale$Age[sale$Age == "26-35"] <- "30"
sale$Age[sale$Age == "36-45"] <- "40"
sale$Age[sale$Age == "46-50"] <- "48"
sale$Age[sale$Age == "51-55"] <- "53"
sale$Age[sale$Age == "55+"] <- "60"
sale$Age<- as.numeric(sale$Age)


#converting city category to numeric
sale$City_Category[sale$City_Category == "A"] <- "0"
sale$City_Category[sale$City_Category == "B"] <- "1"
sale$City_Category[sale$City_Category == "C"] <- "2"
sale$City_Category <- as.numeric(sale$City_Category)

#converting stay in currenty city years to numeric
sale$Stay_In_Current_City_Years[sale$Stay_In_Current_City_Years == "4+"] <- "4"
sale$Stay_In_Current_City_Years <- as.numeric(sale$Stay_In_Current_City_Years)

#kruskal wallis test
kruskal.test(Purchase ~ Gender, data = sale)
kruskal.test(Purchase ~ Age, data = sale)
kruskal.test(Purchase ~ Occupation, data = sale)
kruskal.test(Purchase ~ City_Category, data = sale)
kruskal.test(Purchase ~ Stay_In_Current_City_Years, data = sale)
kruskal.test(Purchase ~ Marital_Status, data = sale)



#average amount of purchase, sorted by male and female
sapply(split(sale$Purchase, sale$Gender), mean)



