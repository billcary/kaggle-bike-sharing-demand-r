#
# Project: Predict Bike Sharing Demand using R
# Author : Bill Cary
# Date   : 25 Dec 2014
# Method : Gradient Boosted Machine
# Desc   : Model and predict total rider count.  (Do not model casual and
#          registered riders seperately to obtain final result.  Conduct intial
#          model development and testing using 75/25 holdout on the training 
#          data.  Then take best model and train on full training dataset before
#          predicting full test set for submission to Kaggle.  Plots included to
#          assess model performance for total riders.

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
library(gridExtra)

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

# Set up training control parameters
fitControl <- trainControl(## 10-fold CV
        method = "repeatedcv",
        number = 10,
        ## repeated ten times
        repeats = 10)

# Set up the tuning grid
gbmGrid <- expand.grid(interaction.depth = c(3:7)
                       ,n.trees = c(50:60) * 50
                       ,shrinkage = 0.05)

# # Set up training control parameters
# fitControl <- trainControl(## 10-fold CV
#         method = "repeatedcv",
#         number = 2,
#         ## repeated ten times
#         repeats = 2)
# 
# # Set up the tuning grid
# gbmGrid <- expand.grid(interaction.depth = 6
#                        ,n.trees = 1000
#                        ,shrinkage = 0.05)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Train a model for total rider count
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        

## Display
cat("\n\nNow training a GBM model...\n")

## Train a gbm
set.seed(300)
formula = count ~ season  + holiday +
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

## Use the model for prediction and store the results in a new column
## in the submission template dataframe
testing$count.predict <- predict(model, testing[, 2:15])

# summarize the results of the prediction
summary(testing$count.predict)

# Take absolute value to eliminate negative predictions
testing$count.predict <- abs(testing$count.predict)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Performance diagnostics
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rmsle.count <- rmsle(testing$count, testing$count.predict)

cat("\n\nRMSLE for total riders is", rmsle.count,"...\n")

p1 <- ggplot(testing, aes(x=count.predict, y=count.predict - 
                                  count)) +
        geom_point(shape=1) +    # Use hollow circles
        geom_smooth() +          # Add a loess smoothed fit curve w/ conf region
        labs(title = "Total Ridership Residuals vs Predicted Values") +
        ylab("Total Ridership Residuals ") +
        xlab("Predicted Values")

p2 <- ggplot(testing, aes(x=count.predict, y=count)) +
        geom_point(shape=1) +    # Use hollow circles
        geom_smooth() +          # Add a loess smoothed fit curve w/ conf region
        labs(title = "Total Ridership Actual vs Predicted Values") +
        ylab("Actual Values") +
        xlab("Predicted Values")

grid.arrange(p1, p2, nrow=1, ncol=2)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Train final caret model on full dataset prior to running Kaggle predictions
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up training control parameters
final.fit.Control <- trainControl(## 10-fold CV
        method = "repeatedcv",
        number = 10,
        ## repeated ten times
        repeats = 10)

# Set up the tuning grid
final.gbm.grid <- expand.grid(interaction.depth = 7
                              ,n.trees = 2950
                              ,shrinkage = 0.05)

final.model <- train(formula
                     ,data = processed_train
                     ,method = 'gbm'
                     ,trControl = final.fit.Control
                     ,tuneGrid = final.gbm.grid
                     ,verbose = TRUE)

processed_test$count.predict <- predict(final.model,
                                        processed_test[, 2:15])

processed_test$count.predict <- abs(processed_test$count.predict)


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
