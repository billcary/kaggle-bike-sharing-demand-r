#
# Project: Predict Bike Sharing Demand using R
# Author : Bill Cary
# Date   : 24 Dec 2014
# Method : Gradient Boosted Machine
# Desc   : Model and predict casual and registered riders separately, then add
#          predictions to obtain final result.  Conduct intial model development
#          and testing using 75/25 holdout on the training data.  Then take best
#          model and train on full training dataset before predicting full
#          test set for submission to Kaggle.  Plots included to assess
#          model performance for registered riders, casual riders and total
#          riders.

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Environment Preparation
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Get the local path on Domino
path_cloud <- getwd()

## Define other paths
path_source <- paste0(path_cloud, "/src/")
path_train <- paste0(path_cloud, '/data/train.csv')
path_test <- paste0(path_cloud, '/data/test.csv')
path_results <- paste0(path_cloud, '/results/kaggle_gbm_submission_file.csv')

## Source helper scripts
#source(paste0(path_source, 'install.R'))  # Install required libraries
source(paste0(path_source, 'process_raw_data_file.R')) # pre-processing

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load required libraries
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(caret)
library(Metrics) # performance measurement & improvement
library(doParallel)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Register parallel backend
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cl <- makeCluster(4)  # set approapriately for server on which job will run
registerDoParallel(cl)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Import Data from Filesystem
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Loading training file as well as test file - CHANGE THIS PATH APPROPRIATELY
train <- read.csv(path_train)
test <- read.csv(path_test)

# Pre-process both training and test data
processed_train <- process.bike.data(train)
processed_test <- process.bike.data(test)


processed_test$casual <- 0
processed_test$registered <- 0

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Holdout 25% of training data for prelim testing/RMSLE estimates
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(107)
inTrain <- createDataPartition(y = processed_train$count,
                               p = .75,
                               list = FALSE)

training <- processed_train[inTrain,]
testing <- processed_train[-inTrain,]


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Train a GBM model for each variable (Casual and Registered)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create single-column dataframe holding datetime values from test data
test.prediction <- data.frame(testing[, 1])

# # Set up training control parameters
# fitControl <- trainControl(## 10-fold CV
#         method = "repeatedcv",
#         number = 10,
#         ## repeated ten times
#         repeats = 10)
# 
# # Set up the tuning grid
# gbmGrid <- expand.grid(interaction.depth = c(3:6)
#                        ,n.trees = c(40:50) * 50
#                        ,shrinkage = 0.05)

# Set up training control parameters
fitControl <- trainControl(## 10-fold CV
        method = "repeatedcv",
        number = 1,
        ## repeated ten times
        repeats = 1)

# Set up the tuning grid
gbmGrid <- expand.grid(interaction.depth = 6
                       ,n.trees = 1000
                       ,shrinkage = 0.05)

## One Variable at at Time
ls_label <- c("Casual Riders", "Registered Riders")

for (n_label in 1:2) {
        
        ## Display
        cat("\n\nNow training a GBM model for", ls_label[n_label], "...\n")
        
        ## Train a gbm
        set.seed(300)
        formula = training[, 16 + n_label] ~ season  + holiday +
                workingday + weather + atemp + humidity + windspeed +
                dayofweek + hourofday + heatindex
        
        model <- train(formula
                        ,data = training
                        ,method = 'gbm'
                        ,trControl = fitControl
                        ,tuneGrid = gbmGrid
                        ,verbose = TRUE)
        
        ## Print the Model and Model Summary        
        print(model)
        summary(model)
        
        index <- n_label + 1
        
        ## Use the model for prediction and store the results in a new column
        ## in the submission template dataframe
        test.prediction[, index] <- predict(model, testing[, 2:15])
        
        # summarize the results of the prediction
        summary(test.prediction[, index])
        
        # Take absolute value to eliminate negative predictions
        test.prediction[, index] <- abs(test.prediction[, index])
        
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Add Casual and Registered to get total ridership
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test.prediction[, 4] <- test.prediction[, 2] + test.prediction[, 3]
colnames(test.prediction) <- c('datetime', 'casual', 'registered', 'count')

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create data frame matching submission file format
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
submit_file <- data.frame(test.prediction$datetime, test.prediction$count)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Write results to csv file for upload to Kaggle
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.table(submit_file, file = path_results, row.names = FALSE, sep = ','
            ,col.names = c('datetime', 'count'), quote = FALSE, eol = '\r\n')

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Print System and Session Info
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print(sessionInfo())

print(Sys.info())
