  
  #Setting Working Directory
setwd("D:/Machine Learning In R/Titanic Machine Learning")

#Importing the data set
library(readr)
train <- read_csv("D:/Machine Learning In R/Titanic Machine Learning/train.csv")
View(train)

library(readr)
test <- read_csv("D:/Machine Learning In R/Titanic Machine Learning/test.csv")
View(test)

# Getting the proportion

table(train$Survived)
prop.table((table(train$Survived)))

# Making the first prediction
test$Survived <- rep(0,418)

#Everyone dies save as a new file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# Second Prediction
summary(train$Sex)

train$Sex <- as.factor(train$Sex)

#Finding deaths by Gender
prop.table(table(train$Sex, train$Survived))
#Row wise proportion
prop.table(table(train$Sex, train$Survived),1)

#Updating prediction for predicting all females survived
test$Survived <- 0
test$Survived[test$Sex =='female'] <- 1
#Writing on a new file for females survived
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "allfemalessurvived.csv", row.names = FALSE)

#Digging into the age variable

summary(train$Age)

# Making a new variable for children
train$Child <- 0
train$Child[train$Age <18] <-1

aggregate(Survived ~ Child + Sex, data=train, FUN=sum)

aggregate(Survived ~ Child + Sex, data=train, FUN=length)

aggregate(Survived ~ Child + Sex, data=train,
          FUN=function(x) {sum(x)/length(x)})

#Predicitng based on class and fares
#Splitting the fares into bins
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

#Getting aggreagate for class and fares
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})




#New predicition based on class

test$Survived <- 0
test$Survived[test$Sex =='female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >=20] <- 0

#Writing onto the submit file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "allrichfemalessurvived.csv", row.names = FALSE)



#prediction by decision tree

library(rpart)
fit<-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
           data = train,
           method = "class")
plot(fit)
text(fit)

#Decision tree by packages to groom beautiful trees
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RcolorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

prediction<- predict(fit,test,type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = "myfirsttreeprediction.csv", row.names = FALSE)


#Decision tree for deeper prediction using minisplit funda

fit<-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
           data = train,
           method = "class",
           control = rpart.control(minsplit = 2,cp=0))
fancyRpartPlot(fit)


