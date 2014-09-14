#
# Project: Predict Bike Sharing Demand using R
# Author : Gopinath Munisifreddy
# Date   : Aug-18-2014
#

# Source data pre-processing routines
source('./process_raw_data_file.R')

#Loading training file as well as test file - CHANGE THIS PATH APPROPRIATELY
train <- read.csv('../data/train.csv')
test <- read.csv('../data/test.csv')

# Pre-process both training and test data
processed_train <- process.bike.data(train)
processed_test <- process.bike.data(test)


#install required libraries
install.packages('lubridate') 
install.packages('randomForest')
library(lubridate)
library(randomForest)


processed_test$casual <- 0
processed_test$registered <- 0

#Create a random forest
casual_fit <- randomForest(as.factor(casual) ~ season + holiday + weather + 
                                   dayofweek + hourofday + temp + atemp + humidity +
                                   windspeed , data = processed_train,
                           ntree = 50, importance = TRUE)

registered_fit <- randomForest(as.factor(registered) ~ season + holiday +
                                       weather + dayofweek + hourofday + temp + atemp +
                                       humidity + windspeed ,
                               data = processed_train, ntree = 50,
                               importance = TRUE)

#Uncomment the following line if you want to see how your model plot looks like
#varImpPlot(fit)

#Predict values and save output
casual_prediction <- predict(casual_fit, processed_test)
registered_prediction <- predict(registered_fit, processed_test)

submit <- data.frame(datetime = test$datetime, casual=casual_prediction,
                     registered=registered_prediction)
submit$casual <- as.numeric(submit$casual)
submit$registered <- as.numeric(submit$registered)
submit$count <- as.numeric(submit$casual) + as.numeric(submit$registered)

write.csv(submit, file = 'random-forest.csv', row.names = FALSE)