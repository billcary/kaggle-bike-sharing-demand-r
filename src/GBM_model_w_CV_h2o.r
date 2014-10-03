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
        cat("\n\nNow training a GBM model for", ls_label[n_label], "...\n")
        
        ## Train a gbm
        model <- h2o.gbm(x = 2:15,
                         y = (15 + n_label),
                         data = train_h2o,
                         distribution = 'gaussian',
                         n.trees = 10,
                         interaction.depth = 3,
                         n.minobsinnode = 10,
                         shrinkage = 0.1,
                         n.bins = 100,
                         importance = FALSE,
                         nfolds = 10)
        
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
colnames(raw_sub) <- c('datetime', 'casual', 'registered', 'count')

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create data frame matching submission file format
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
submit_file <- data.frame(raw_sub$datetime, raw_sub$count)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Write results to csv file for upload to Kaggle
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.csv(submit_file, file = path_results, row.names = FALSE)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Print System and Session Info
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print(sessionInfo())

print(Sys.info())