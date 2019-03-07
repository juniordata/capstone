#read file
saletrain<-read.csv('BlackFridayTrain.csv',stringsAsFactors = FALSE)
saletest<-read.csv('BlackFridayTest.csv',stringsAsFactors = FALSE)

#explore data type/class
sapply(saletrain, class)
sapply(saletest, class)

#summary on sale dataset
summary(saletrain)
summary(saletest)

#structure of dataset
str(saletrain)
str(saletest)

#check for NA values
sum(is.na(saletrain))
sum(is.na(saletest))

#remove NA values
saletrain[is.na(saletrain)] <-0
saletest[is.na(saletest)] <-0

#plot amount of males and females
plot(saletrain$Gender)

#plot age group
plot(saletrain$Age)

#plot histogram on Purchases
hist(saletrain$Purchase)

#install ggplot
install.packages('ggplot2')
#Plot graph to show the amount purchased between male and female among different age group.
ggplot(saletrain, aes(x=Age, fill=Gender, colour=Gender)) + geom_bar(position=position_dodge())

#average amount of purchase, sorted by male and female
sapply(split(saletrain$Purchase, saletrain$Gender), mean)

#converting age to numerics
saletrain$Age[saletrain$Age == "0-17"] <- "15"
saletrain$Age[saletrain$Age == "18-25"] <- "21"
saletrain$Age[saletrain$Age == "26-35"] <- "30"
saletrain$Age[saletrain$Age == "36-45"] <- "40"
saletrain$Age[saletrain$Age == "46-50"] <- "48"
saletrain$Age[saletrain$Age == "51-55"] <- "53"
saletrain$Age[saletrain$Age == "55+"] <- "60"

saletest$Age[saletest$Age == "0-17"] <- "15" 
saletest$Age[saletest$Age == "18-25"] <- "21"
saletest$Age[saletest$Age == "26-35"] <- "30"
saletest$Age[saletest$Age == "36-45"] <- "40"
saletest$Age[saletest$Age == "46-50"] <- "48"
saletest$Age[saletest$Age == "51-55"] <- "53"
saletest$Age[saletest$Age == "55+"] <- "60"

saletrain$Age <- as.integer(saletrain$Age)
saletest$Age <- as.integer(saletest$Age)
