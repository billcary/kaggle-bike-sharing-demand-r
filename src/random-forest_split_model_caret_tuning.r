#
# Project: Predict Bike Sharing Demand using R
# Author : Bill Cary
# Date   : 14 Sept 20014
#

# Source data pre-processing routines
source('./process_raw_data_file.R')

#Loading training file as well as test file - CHANGE THIS PATH APPROPRIATELY
train <- read.csv('../data/train.csv')
test <- read.csv('../data/test.csv')

# Pre-process both training and test data
processed_train <- process.bike.data(train)
processed_test <- process.bike.data(test)


#install required libraries for modeling and data manipulation
# install.packages('lubridate') 
# install.packages('randomForest')
library(lubridate)
library(randomForest)

#install required libraries for performance measurement & improvement
# install.packages('caret')
# install.packages('Metrics')
library(caret)
library(Metrics)


processed_test$casual <- 0
processed_test$registered <- 0

# Set up the caret performance tuning control parameters
tc <- trainControl(method = "repeatedcv", number=5, repeats=2, classProbs=FALSE,
                   savePred=T)

#Create random forests for both the casual and registered ridership
set.seed(300)
# casual_fit <- train(casual ~ season + holiday + weather + 
#                                    dayofweek + hourofday + temp + atemp + humidity +
#                                    windspeed + workingday + dayofmonth + israiny,
#                     data = processed_train,
#                     method = 'rf',
#                     trControl = tc,
#                     metric = 'RMSE')

casual_fit <- randomForest(as.factor(casual) ~ season + holiday + weather + 
                                   dayofweek + hourofday + temp + atemp + humidity +
                                   windspeed + workingday + dayofmonth + israiny,
                           data = processed_train,
                           method = 'rf',
                           trControl = tc,
                           metric = 'RMSE')

registered_fit <- randomForest(as.factor(registered) ~ season + holiday + weather + 
                                       dayofweek + hourofday + temp + atemp + humidity +
                                       windspeed + workingday + dayofmonth + israiny,
                               data = processed_train,
                               method = 'rf',
                               trControl = tc,
                               metric = 'RMSE')

#Uncomment the following line if you want to see how your model plot looks like
#varImpPlot(fit)

#Predict values and save output
casual_prediction <- predict(casual_fit, processed_test)
registered_prediction <- predict(registered_fit, processed_test)

# Predict values on training set for performance eval purposes
# (Not ideal, but don't have true values for test data set)
casual_prediction_train <- predict(casual_fit, processed_train)
registered_prediction_train <- predict(registered_fit, processed_train)

# Create submission file based on predictions for test data
predict_train <- data.frame(datetime = train$datetime, casual=casual_prediction_train,
                     registered=registered_prediction_train)

# Convert columns to numeric to allow addition
predict_train$casual <- as.numeric(predict_train$casual)
predict_train$registered <- as.numeric(predict_train$registered)

# Add 'casual' and 'registered' columns to determine the total ridership
predict_train$count <- as.numeric(predict_train$casual) + as.numeric(predict_train$registered)

# Drop 'casual' and 'registered' columns in order to match submission format
# required by Kaggle
predict_train$casual <- NULL
predict_train$registered <- NULL

actual_train <- data.frame(datetime = train$datetime, count = train$count)

#------------------------------------------------------------------------


#------------------------------------------------------------------------
# Create submission file based on predictions for test data
submit <- data.frame(datetime = test$datetime, casual=casual_prediction,
                     registered=registered_prediction)

# Convert columns to numeric to allow addition
submit$casual <- as.numeric(submit$casual)
submit$registered <- as.numeric(submit$registered)

# Add 'casual' and 'registered' columns to determine the total ridership
submit$count <- as.numeric(submit$casual) + as.numeric(submit$registered)

# Drop 'casual' and 'registered' columns in order to match submission format
# required by Kaggle
submit$casual <- NULL
submit$registered <- NULL

write.csv(submit, file = '../results/random-forest-tuned.csv', row.names = FALSE)
#------------------------------------------------------------------------