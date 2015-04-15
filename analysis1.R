# Titanic: Machine Learning from Disaster
# Following http://trevorstephens.com/post/72916401642/titanic-getting-started-with-r


# Project set-up ----------------------------------------------------------

# Setting working directory
setwd("~/Documents/GitHub/kaggle-titanic")

# Loading libraries
library(ggplot2)

# Import data
genderclassmodel <- read.csv("~/Documents/GitHub/kaggle-titanic/Data/genderclassmodel.csv", stringsAsFactors = FALSE)
gendermodel <- read.csv("~/Documents/GitHub/kaggle-titanic/Data/gendermodel.csv", stringsAsFactors = FALSE)
test <- read.csv("~/Documents/GitHub/kaggle-titanic/Data/test.csv")
train <- read.csv("~/Documents/GitHub/kaggle-titanic/Data/train.csv")


# All passengers perish ---------------------------------------------------

# Understanding the structure of the data
str(train)

# Looking at the data to see how many people in the training dataset has survived
table(train$Survived)
prop.table(table(train$Survived))

# Create a 'Survived' column in the test dataset where everyone does not survive
test$Survived <- rep(0, 418)

# Create first submission file assuming all passengers perish (score = 0.6269)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "Submissions/submission1.csv", row.names = FALSE)



# Gender-class model ------------------------------------------------------

# Looking at the split between genders
summary(train$Sex)

# Looking at the proportion of males and females that survived
prop.table(table(train$Sex, train$Survived), 1) # Most females survive (0.74), and most males perish (0.81)

# Create submission where all females survive and all males perish
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

# Create submission where all females survive and all males perish (score = 0.7655)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "Submissions/submission2.csv", row.names = FALSE)

# Looking and plotting at the age variable
summary(train$Age)
qplot(Age, data = train, geom = "histogram", binwidth = 5)

# Creating a child variable, where age is less than 18
train$Child <- 0
train$Child[train$Age < 18] <- 1

# Creating a table with both gender and child status to see who survived
aggregate(Survived ~ Child + Sex, data = train, FUN = sum) # Number of survivors 
aggregate(Survived ~ Child + Sex, data = train, FUN = length) # Totals for each category
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {sum(x) / length(x)}) # Proportions for each category - does not appear to have significant difference regardless whether person was a child or not. Still large majority of passengers survived if they were a female.

# Create a factor variable that bins the fare variable
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

# Create a table with class, ticket price,  gender and child status to see who survived
aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x) {sum(x) / length(x)}) # Females in third class paying for more than $20 for a ticket has a lower chance of survival.

# Create submission where all females except for those in third class paying more than $20 for a ticket survive, and the rest perish (score = 0.7799)
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "Submissions/submission3.csv", row.names = FALSE)


# Decision trees ----------------------------------------------------------

# Installing relevant packages
install.packages("rpart")
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Using rpart for the first time!
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

# Plotting the decision tree
fancyRpartPlot(fit)

# Generationg submission for Kaggle using this decision tree (score = 0.78469)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "submission4.csv", row.names = FALSE)
