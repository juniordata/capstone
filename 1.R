#read file
sale<-read.csv('BlackFriday.csv')
#explore data type/class
sapply(sale, class)
sapply(sale, typeof)

#remove NA values
sale[is.na(sale)] <-0
#install ggplot
install.packages('ggplot2')
#Plot graph to show the amount purchased between male and female among different age group.
ggplot(sale, aes(x=Age, fill=Gender, colour=Gender)) + geom_bar(position=position_dodge())

#plot amount of males and females
plot(sale$Gender)

#average amount of purchase, sorted by male and female
sapply(split(sale$Purchase, sale$Gender), mean)
