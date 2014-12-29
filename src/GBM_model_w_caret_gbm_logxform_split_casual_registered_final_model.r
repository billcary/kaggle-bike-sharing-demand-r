#
# Project: Predict Bike Sharing Demand using R
# Author : Bill Cary
# Date   : 28 Dec 2014
# Method : Gradient Boosted Machine
# Desc   : Model and predict casual and registered riders separately, then add
#          predictions to obtain final result.  Conduct intial model development
#          and testing using 75/25 holdout on the training data.  Then take best
#          model and train on full training dataset before predicting full
#          test set for submission to Kaggle.  Plots included to assess
#          model performance for registered riders, casual riders and total
#          riders.  Utilize Bernoulli distribution for counts.

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
source(paste0(path_source, 'install.R'))  # Install required libraries
source(paste0(path_source, 'process_raw_data_file.R')) # pre-processing

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load required libraries
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(caret)
library(gbm)
library(Metrics) # performance measurement & improvement
library(doParallel)
library(gridExtra)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Register parallel backend
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cl <- makeCluster(32)  # set approapriately for server on which job will run
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

# processed_test$casual <- 0
# processed_test$registered <- 0


# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Train final caret model on full dataset prior to running Kaggle predictions
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up training control parameters
final.fit.Control <- trainControl(## 10-fold CV
        method = "repeatedcv",
        number = 10,
        ## repeated ten times
        repeats = 10)

# Set up the tuning grid
final.gbm.casual.grid <- expand.grid(interaction.depth = 7
                              ,n.trees = 2500
                              ,shrinkage = 0.05)

final.gbm.registered.grid <- expand.grid(interaction.depth = 7
                                     ,n.trees = 2850
                                     ,shrinkage = 0.05)

set.seed(300)
casual.formula = log.casual ~ season  + holiday +
        workingday + weather + atemp + humidity + windspeed +
        dayofweek + hourofday + heatindex

registered.formula = log.registered ~ season  + holiday +
        workingday + weather + atemp + humidity + windspeed +
        dayofweek + hourofday + heatindex

final.casual.model <- train(casual.formula
                            ,data = processed_train
                            ,method = 'gbm'
                            ,trControl = final.fit.Control
                            ,tuneGrid = final.gbm.casual.grid
                            ,verbose = TRUE)

final.registered.model <- train(registered.formula
                          ,data = processed_train
                          ,method = 'gbm'
                          ,trControl = final.fit.Control
                          ,tuneGrid = final.gbm.registered.grid
                          ,verbose = TRUE)

# Predict casual rider counts
processed_test$log.casual.predict <- predict(final.casual.model,
                                         processed_test[, 2:15])

# Reverse the log transformation
processed_test$casual.predict <- exp(processed_test$log.casual.predict) - 1

# Set any negative predictions to zero
processed_test$casual.predict[processed_test$casual.predict < 0] <- 0

# Predict registered rider counts
processed_test$log.registered.predict <- predict(final.registered.model,
                                             processed_test[, 2:15])

# Reverse the log transformation
processed_test$registered.predict <- 
        exp(processed_test$log.registered.predict) - 1

# Set any negative predictions to zero
processed_test$registered.predict[processed_test$registered.predict < 0] <- 0

processed_test$count.predict <- processed_test$casual.predict +
        processed_test$registered.predict


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create data frame matching submission file format
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
submit_file <- data.frame(processed_test$timestamp, processed_test$count.predict)
colnames(submit_file) <- c('datetime', 'count')

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
