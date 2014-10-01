#
# Project: Predict Bike Sharing Demand using R
# Author : Bill Cary
# Date   : 30 Sept 20014
#

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Environment Preparation
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Get the local path on Domino
path_cloud <- getwd()

## Define other paths
path_source <- paste0(path_cloud, "/src/")
path_train <- paste0(path_cloud, '/data/train.csv')
path_test <- paste0(path_cloud, '/data/test.csv')
path_results <- paste0(path_cloud, '/results/kaggle_submission_file.csv')

## Source helper scripts
source(paste0(path_source, 'install.R'))  # Install required libraries
source(paste0(path_source, 'process_raw_data_file.R')) # pre-processing

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load required libraries
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(h2o) # parallel processing on Domino
library(Metrics) # performance measurement & improvement

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initiate and Connect to a Local H2O Cluster on Domino
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
localH2O <- h2o.init(max_mem_size = '12g') ## using a max 1GB of RAM

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
## Pass Data to H2O
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
train_h2o <- as.h2o(localH2O, processed_train)
test_h2o <- as.h2o(localH2O, processed_test)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Train a Deep Neural Networks model for each variable (Casual and Registered)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

raw_sub <- data.frame(processed_test[, 1])

## One Variable at at Time
ls_label <- c("Casual Riders", "Registered Riders")

for (n_label in 1:2) {
        
        ## Display
        cat("\n\nNow training a DNN model for", ls_label[n_label], "...\n")
        
        ## Train a DNN
        model <- h2o.deeplearning(x = 2:15,
                                  y = (15 + n_label),
                                  data = train_h2o,
                                  nfolds = 5,
                                  activation = "Rectifier",
                                  hidden = c(50, 50, 50),
                                  epochs = 100,
                                  classification = FALSE,
                                  balance_classes = FALSE)
        
        ## Print the Model Summary
        print(model)
        
        index <- n_label + 1
        
        ## Use the model for prediction and store the results in submission template
        raw_sub[, index] <- as.matrix(h2o.predict(model, test_h2o[, 2:15]))
        
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Add Casual and Registered to get total ridership
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
raw_sub[, 4] <- raw_sub[, 2] + raw_sub[, 3]


#Uncomment the following line if you want to see how your model plot looks like
#varImpPlot(fit)

# #Predict values and save output
# casual_prediction <- predict(casual_fit, processed_test)
# registered_prediction <- predict(registered_fit, processed_test)
# 
# # Predict values on training set for performance eval purposes
# # (Not ideal, but don't have true values for test data set)
# casual_prediction_train <- predict(casual_fit, processed_train)
# registered_prediction_train <- predict(registered_fit, processed_train)
# 
# # Create submission file based on predictions for training data
# predict_train <- data.frame(datetime = train$datetime, casual=casual_prediction_train,
#                      registered=registered_prediction_train)
# 
# # Convert columns to numeric to allow addition
# predict_train$casual <- as.numeric(predict_train$casual)
# predict_train$registered <- as.numeric(predict_train$registered)
# 
# # Add 'casual' and 'registered' columns to determine the total ridership
# predict_train$count <- as.numeric(predict_train$casual) + as.numeric(predict_train$registered)
# 
# # Drop 'casual' and 'registered' columns in order to match submission format
# # required by Kaggle
# predict_train$casual <- NULL
# predict_train$registered <- NULL


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create data frame matching submission file format
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

submit_file <- data.frame(datetime = raw_sub[, 1], count = raw_sub[, 4])

# #------------------------------------------------------------------------
# 
# 
# #------------------------------------------------------------------------
# # Create submission file based on predictions for test data
# submit <- data.frame(datetime = test$datetime, casual=casual_prediction,
#                      registered=registered_prediction)
# 
# # Convert columns to numeric to allow addition
# submit$casual <- as.numeric(submit$casual)
# submit$registered <- as.numeric(submit$registered)
# 
# # Add 'casual' and 'registered' columns to determine the total ridership
# submit$count <- as.numeric(submit$casual) + as.numeric(submit$registered)
# 
# # Drop 'casual' and 'registered' columns in order to match submission format
# # required by Kaggle
# submit$casual <- NULL
# submit$registered <- NULL


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Write results to csv file for upload to Kaggle
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.csv(submit_file, file = path_results, row.names = FALSE)

#------------------------------------------------------------------------

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Print System and Session Info
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print(sessionInfo())

print(Sys.info())