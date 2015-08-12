# Titanic: Machine Learning from Disaster
# Following http://trevorstephens.com/post/72916401642/titanic-getting-started-with-r

# Project set-up ----------------------------------------------------------

# Setting working directory
setwd("C:\\Users\\895284\\Documents\\GitHub\\kaggle-titanic")
setwd("~/Documents/GitHub/kaggle-titanic")

# Loading libraries
library(ggplot2)

# Import data
genderclassmodel <- read.csv("Data\\genderclassmodel.csv", stringsAsFactors = FALSE)
gendermodel <- read.csv("Data\\gendermodel.csv", stringsAsFactors = FALSE)
test <- read.csv("Data\\test.csv")
train <- read.csv("Data\\train.csv")

genderclassmodel <- read.csv("Data/genderclassmodel.csv", stringsAsFactors = FALSE)
gendermodel <- read.csv("Data/gendermodel.csv", stringsAsFactors = FALSE)
test <- read.csv("Data/test.csv")
train <- read.csv("Data/train.csv")


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
genderclassmodel <- read.csv("Data\\genderclassmodel.csv", stringsAsFactors = FALSE)
gendermodel <- read.csv("Data\\gendermodel.csv", stringsAsFactors = FALSE)
test <- read.csv("Data\\test.csv")
train <- read.csv("Data\\train.csv")

genderclassmodel <- read.csv("Data/genderclassmodel.csv", stringsAsFactors = FALSE)
gendermodel <- read.csv("Data/gendermodel.csv", stringsAsFactors = FALSE)
test <- read.csv("Data/test.csv")
train <- read.csv("Data/train.csv")

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

# Generating submission for Kaggle using this decision tree (score = 0.78469)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "submission4.csv", row.names = FALSE)



# Feature Engineering -----------------------------------------------------

# Reloading in data to start from blank slate


# Creating a combined dataset with both training and test data
test$Survived <- NA
combi <- rbind(train, test)

# Changing name back to character
combi$Name <- as.character(combi$Name)

# Creating a Title variable from the Name variable
combi$Title <- sapply(combi$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][2]})
combi$TItle <- sub(' ', '', combi$Title)

# Combining a few of the unusual titles
combi$Title[(combi$Title %in% c('Mme', 'Mlle'))] <- 'Mlle'
combi$Title[(combi$Title %in% c('Capt', 'Don', 'Major', 'Sir'))] <- 'Sir'
combi$Title[(combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer'))] <- 'Lady'

# Changing the Title variable back into a factor variable
combi$Title <- factor(combi$Title)

# Creating a variable which captures the size of the family
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Creating a Surname variable
combi$Surname <- sapply(combi$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][1]})

# Combining the FamilySize and Surname varaiables
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")

# Identifying families with small sizes
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

# Data cleaning - some small families slipped through the net
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2, ]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

# Resplitting the training and test datasets
train <- combi[1:891, ]
test <- combi[892:1309, ]

# Fitting a new prediction tree using feature engineered variables
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, 
             data=train, method="class")

# Generating a prediction and submission file for Kaggle
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "submission5.csv", row.names = FALSE)



# Random Forest -----------------------------------------------------------

# Growing a tree for Age
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

# Fixing the Embared and Fare varibles with missing values
combi$Embarked[which(combi$Embarked == '')] = "S"
combi$Fare[which(is.na(combi$Fare))] <- median(combi$Fare, na.rm=TRUE)

# Generating a new FamilyID variable because the random forest function in R cannot take in too many factors
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

# Re-splitting new training and test data
train <- combi[1:891, ]
test <- combi[892:1309, ]

# Loading the randomForest package
# install.packages('randomForest')
library(randomForest)

# Resetting the seed
set.seed(415)

# Fitting a random forest
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2, data=train, importance=TRUE, ntree= 2000)

# Analysing the most important features of the random forest
varImpPlot(fit)

# Generating Kaggle prediction file
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

# Installing and loading the party package
# install.packages('party')
library(party)

# Setting the seed for consistent results and build a model in a similar way to random forests
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls = cforest_unbiased(ntree=2000, mtry=3))

# Generating a prediction
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "cforest.csv", row.names = FALSE)
