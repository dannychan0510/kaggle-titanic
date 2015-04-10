# Titanic: Machine Learning from Disaster
# Following http://trevorstephens.com/post/72916401642/titanic-getting-started-with-r


# Project set-up ----------------------------------------------------------

# Setting working directory
setwd("~/Documents/GitHub/kaggle-titanic")

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

# Create first submission file assuming all passengers perish (0.6269)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "Submissions/theyallperish.csv", row.names = FALSE)



# Gender-class model ------------------------------------------------------

# Looking at the split between genders
summary(train$Sex)

# Looking at the proportion of males and females that survived
prop.table(table(train$Sex, train$Survived), 1)


