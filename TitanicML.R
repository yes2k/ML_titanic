library(e1071)
library(caret)
library(dplyr)
library(reshape2)
library(ggplot2)
library(magrittr)

setwd("~/Documents/r_wd/machine learning projects/Titanic")
train <- read.csv('train.csv')
test <- read.csv('test.csv')
survived <- read.csv('gender_submission.csv')

test <- merge(test, survived, by='PassengerId')

#==Proccessing==

#Changing male to 0 and female to 1
train$Sex <- as.character(train$Sex)
train[train == 'male'] <- 0
train[train == 'female'] <- 1
train$Sex <- as.numeric(train$Sex)

test$Sex <- as.character(test$Sex)
test[test == 'male'] <- 0
test[test == 'female'] <- 1
test$Sex <- as.numeric(test$Sex)


#We have missing ages
table(is.na(train$Age))
table(is.na(test$Age))

#Since we have missing ages, we will fill them in by randomly sampling from
#the ages we have
age <- train$Age[!is.na(train$Age)]
for (i in 1:length(train$Age)){
  if(is.na(train$Age[i])){
    train$Age[i] <- age[sample(1:length(age), 1)]
  }
}

t.age <- test$Age[!is.na(test$Age)]
for (i in 1:length(test$Age)){
  if(is.na(test$Age[i])){
    test$Age[i] <- t.age[sample(1:length(t.age), 1)]
  }
}


#combinding two features into 1, number of family members
train$NumFamMeb <- train$SibSp + train$Parch
test$NumFamMeb <- test$SibSp + test$Parch
  

#Changing Embarked to use one hot encoding
train$Embarked <- as.character(train$Embarked)
C <- numeric(0)
Q <- numeric(0)
S <- numeric(0)
for (i in train$Embarked){
  if(i == 'C'){
    C <- c(C,1)
    Q <- c(Q,0)
    S <- c(S,0)
  }
  if(i == 'Q'){
    C <- c(C,0)
    Q <- c(Q,1)
    S <- c(S,0)
  }
  if(i == 'S'){
    C <- c(C,0)
    Q <- c(Q,0)
    S <- c(S,1)
  }
}
train <- train[train$Embarked != '', ]
train %<>% cbind(C) %>% cbind(Q) %>% cbind(S)


test$Embarked <- as.character(test$Embarked)
C2 <- numeric(0)
Q2 <- numeric(0)
S2 <- numeric(0)
for (i in test$Embarked){
  if(i == 'C'){
    C2 <- c(C2,1)
    Q2 <- c(Q2,0)
    S2 <- c(S2,0)
  }
  if(i == 'Q'){
    C2 <- c(C2,0)
    Q2 <- c(Q2,1)
    S2 <- c(S2,0)
  }
  if(i == 'S'){
    C2 <- c(C2,0)
    Q2 <- c(Q2,0)
    S2 <- c(S2,1)
  }
}
test %<>% cbind(C2) %>% cbind(Q2) %>% cbind(S2)

#Have an NA in the fare column
summary(test)
test <- test[!is.na(test$Fare), ] #removing it

#===Data exploration ===

#male vs female survival
ggplot(train, aes(Survived, fill=factor(Sex))) + geom_bar() + facet_grid(.~train$Sex)
#We can see that a larger proportion of females survived than males.

#how does age, pclass affect surivial?
ggplot(train, aes(Survived, fill=factor(Pclass))) + geom_bar()
#We can see that as we go higher in Pclass, the proportion of Survived to not Survived
#increases.

#We can run hypothesis test's to answer some questions about trend in our data

#1. Does a higher fare result in a greater chance of surival?
#We can run a two sample t-test to see if the mean fare is higher for the people who
#survived vs the people who didn't
surv.fare <- t.test(x=train[train$Survived==1,]$Fare, y=train[train$Survived==0,]$Fare)
surv.fare
#We can see that the p-value is very small (<0.05), which means that there is a significant
#difference between mean fare of the people that survived vs the people who didn't

#2.

#==Predicting==
#I will use Pclass, Sex, Age, Fare, C, S, Q and NumFamMeb to predict who will survive
trainX <- train[c('Pclass', 'Sex', 'Age', 'Fare', 'C', 'S', 'Q', 'NumFamMeb')]
trainY <- train$Survived

svm_fit <- train(x=trainX, 
                 y=as.factor(trainY),
                 method = "svmRadial",
                 trControl=trainControl(method = "cv", number = 10))

testX <- test[c('Pclass', 'Sex', 'Age', 'Fare', 'C2', 'S2', 'Q2', 'NumFamMeb')]
testY <- test$Survived

pred <- predict(svm_fit, newdata = data.frame(testX))
confusionMatrix(pred, as.factor(testY))


  
  
