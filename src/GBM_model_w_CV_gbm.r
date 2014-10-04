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
library(gbm)
library(Metrics) # performance measurement & improvement

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

raw_sub <- data.frame(processed_test[, 1])

## One Variable at at Time
ls_label <- c("Casual Riders", "Registered Riders")

for (n_label in 1:2) {
        
        ## Display
        cat("\n\nNow training a GBM model for", ls_label[n_label], "...\n")
        
        ## Train a gbm
        model <- gbm(processed_train[, 16 + n_label]~.
                     ,data = processed_train[, -c(1, 6, 11, 13, 15, 16, 17, 18, 19)]
                     ,var.monotone=NULL # which vars go up or down with target
                     ,distribution="poisson"
                     ,n.trees=1500
                     ,shrinkage=0.05
                     ,interaction.depth=4
                     ,bag.fraction = 0.5
                     ,train.fraction = 1
                     ,n.minobsinnode = 10
                     ,cv.folds = 10
                     ,keep.data=TRUE
                     ,verbose=TRUE)
        
        ## Print the Model Summary        
        best.iter <- gbm.perf(model,method="cv") ##the best iteration number
        
        cat("\n\nBest iteration:\n")
        print(pretty.gbm.tree(model, best.iter))
        
        cat("\n\nModel Summary:\n")
        print(summary(model, n.trees=best.iter))
        
        index <- n_label + 1
        
        ## Use the model for prediction and store the results in submission template
        raw_sub[, index] <- predict(model, processed_test[, 2:15], best.iter
                                    , type = 'response')
        
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
write.csv(submit_file, file = path_results, row.names = FALSE)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Print System and Session Info
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print(sessionInfo())

print(Sys.info())