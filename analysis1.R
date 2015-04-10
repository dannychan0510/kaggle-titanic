# Titanic: Machine Learning from Disaster
# Following http://trevorstephens.com/post/72916401642/titanic-getting-started-with-r


# Project set-up ----------------------------------------------------------

# Setting working directory
setwd("~/Documents/GitHub/kaggle-titanic")

# Import data
genderclassmodel <- read.csv("~/Documents/GitHub/kaggle-titanic/Data/genderclassmodel.csv", stringsAsFactors = FALSE)
gendermodel <- read.csv("~/Documents/GitHub/kaggle-titanic/Data/gendermodel.csv", stringsAsFactors = FALSE)
test <- read.csv("~/Documents/GitHub/kaggle-titanic/Data/test.csv", stringsAsFactors = FALSE)
train <- read.csv("~/Documents/GitHub/kaggle-titanic/Data/train.csv", stringsAsFactors = FALSE)


# Exploratory data analysis -----------------------------------------------

# Understanding the structure of the data
str(train)

# Looking at the data to see how many people in the training dataset has survived
table(train$Survived)
prop.table(table(train$Survived))

# Create a 'Survived' column in the test dataset where everyone does not survive
test$Survived <- rep(0, 418)

# Create first submission file assuming all passengers perish
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
