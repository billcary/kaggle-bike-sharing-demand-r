#
# Project: Predict Bike Sharing Demand using R
# Author : Bill Cary
# Date   : 30 Sept 20014
# Method : Gradient Boosted Machine
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
cl <- makeCluster(4)
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
## Train a GBM model for each variable (Casual and Registered)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create single-column ddataframe holding datetime values from test data
raw_sub <- data.frame(processed_test[, 1])

# Set up training control parameters
fitControl <- trainControl(## 10-fold CV
        method = "repeatedcv",
        number = 10,
        ## repeated ten times
        repeats = 10)

gbmGrid <- expand.grid(interaction.depth = c(1, 5, 9),
                       n.trees = (1:40)*50,
                       shrinkage = 0.1)

## One Variable at at Time
ls_label <- c("Casual Riders", "Registered Riders")

for (n_label in 1:2) {
        
        ## Display
        cat("\n\nNow training a GBM model for", ls_label[n_label], "...\n")
        
        ## Train a gbm
        set.seed(300)
        formula = processed_train[, 16 + n_label] ~ season  + holiday +
                workingday + weather + atemp + humidity + windspeed +
                dayofweek + hourofday + heatindex
        
        model <- train(formula
                     ,data = processed_train
                     ,method = 'gbm'
                     ,trControl = fitControl
                     ,verbose = TRUE
                     ,tuneGrid = gbmGrid)
        
        ## Print the Model Summary        
        model
        
        index <- n_label + 1
        
        ## Use the model for prediction and store the results in a new column
        ## in the submission template dataframe
        raw_sub[, index] <- predict(model, processed_test[, 2:15])
        
        # summarize the results of the prediction
        summary(raw_sub[, index])
        
        # Take absolute value to eliminate negative predictions
        raw_sub[, index] <- abs(raw_sub[, index])
        
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Add Casual and Registered to get total ridership
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
raw_sub[, 4] <- raw_sub[, 2] + raw_sub[, 3]
colnames(raw_sub) <- c('datetime', 'casual', 'registered', 'count')

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create data frame matching submission file format
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
submit_file <- data.frame(raw_sub$datetime, raw_sub$count)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Write results to csv file for upload to Kaggle
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.table(submit_file, file = path_results, row.names = FALSE, sep = ','
            ,col.names = c('datetime', 'count'), quote = FALSE)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Print System and Session Info
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print(sessionInfo())

print(Sys.info())