#
# Project: Predict Bike Sharing Demand using R
# Author : Gopinath Munisifreddy
# Date   : Aug-18-2014
#

#Loading training file as well as test file - CHANGE THIS PATH APPROPRIATELY
train <- read.csv('../data/train.csv')
test <- read.csv('../data/test.csv')

#install required libraries
install.packages('lubridate') 
install.packages('randomForest')
library(lubridate)
library(randomForest)

#Compute hour of the day
train$hour <-hour(train$datetime)
test$hour <- hour(test$datetime)

#Compute day of the week
train$dow <- wday(train$datetime)
test$dow <- wday(test$datetime)

test$count<-0
#Create a random forest
fit <- randomForest(as.factor(count) ~ season + holiday + weather + dow+ hour + temp + atemp+humidity+windspeed , data=train, ntree = 700, importance=TRUE)
#Uncomment the following line if you want to see how your model plot looks like
varImpPlot(fit)

#Predict values and save output
Prediction <- predict(fit, test)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = 'random-forest.csv', row.names = FALSE)