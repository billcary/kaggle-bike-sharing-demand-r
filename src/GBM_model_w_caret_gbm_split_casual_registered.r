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
## Train a model for Casual riders
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        

## Display
cat("\n\nNow training a GBM model for Casual Riders...\n")

## Train a gbm
set.seed(300)
casual.formula = casual ~ season  + holiday +
        workingday + weather + atemp + humidity + windspeed +
        dayofweek + hourofday + heatindex

casual.model <- train(casual.formula
               ,data = training
               ,method = 'gbm'
               ,trControl = fitControl
               ,tuneGrid = gbmGrid
               ,verbose = TRUE)

## Print the Model and Model Summary        
print(casual.model)
summary(casual.model)

## Use the model for prediction and store the results in a new column
## in the submission template dataframe
testing$casual.predict <- predict(casual.model, testing[, 2:15])

# summarize the results of the prediction
summary(testing$casual.predict)

# Take absolute value to eliminate negative predictions
testing$casual.predict <- abs(testing$casual.predict)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Train a model for Registered riders
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        

## Display
cat("\n\nNow training a GBM model for Registered Riders...\n")

## Train a gbm
set.seed(300)
registered.formula = registered ~ season  + holiday +
        workingday + weather + atemp + humidity + windspeed +
        dayofweek + hourofday + heatindex

registered.model <- train(registered.formula
                      ,data = training
                      ,method = 'gbm'
                      ,trControl = fitControl
                      ,tuneGrid = gbmGrid
                      ,verbose = TRUE)

## Print the Model and Model Summary        
print(registered.model)
summary(registered.model)

## Use the model for prediction and store the results in a new column
## in the submission template dataframe
testing$registered.predict <- predict(registered.model, testing[, 2:15])

# summarize the results of the prediction
summary(testing$registered.predict)

# Take absolute value to eliminate negative predictions
testing$registered.predict <- abs(testing$registered.predict)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Add Casual and Registered to get total predicted ridership
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
testing$count.predict <- testing$casual.predict +
        testing$registered.predict

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Performance diagnostics
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rmsle.casual <- rmsle(testing$casual, testing$casual.predict)
rmsle.registered <- rmsle(testing$registered, testing$registered.predict)
rmsle.count <- rmsle(testing$count, testing$count.predict)

cat("\n\nRMSLE for casual riders is", rmsle.casual,"...\n")
cat("\n\nRMSLE for registered riders is", rmsle.registered,"...\n")
cat("\n\nRMSLE for total riders is", rmsle.count,"...\n")

p1 <- ggplot(testing, aes(x=casual.predict, y=casual.predict - casual)) +
        geom_point(shape=1) +    # Use hollow circles
        geom_smooth() +          # Add a loess smoothed fit curve w/ conf region
        labs(title = "Casual Ridership Residuals vs Predicted Values") +
        ylab("Casual Ridership Residuals ") +
        xlab("Predicted Values")

p2 <- ggplot(testing, aes(x=casual.predict, y=casual)) +
        geom_point(shape=1) +    # Use hollow circles
        geom_smooth() +          # Add a loess smoothed fit curve w/ conf region
        labs(title = "Casual Ridership Actual vs Predicted Values") +
        ylab("Actual Values") +
        xlab("Predicted Values")

p3 <- ggplot(testing, aes(x=registered.predict, y=registered.predict - 
                                  registered)) +
        geom_point(shape=1) +    # Use hollow circles
        geom_smooth() +          # Add a loess smoothed fit curve w/ conf region
        labs(title = "Registered Ridership Residuals vs Predicted Values") +
        ylab("Registered Ridership Residuals ") +
        xlab("Predicted Values")

p4 <- ggplot(testing, aes(x=registered.predict, y=registered)) +
        geom_point(shape=1) +    # Use hollow circles
        geom_smooth() +          # Add a loess smoothed fit curve w/ conf region
        labs(title = "Registered Ridership Actual vs Predicted Values") +
        ylab("Actual Values") +
        xlab("Predicted Values")

p5 <- ggplot(testing, aes(x=count.predict, y=count.predict - 
                                  count)) +
        geom_point(shape=1) +    # Use hollow circles
        geom_smooth() +          # Add a loess smoothed fit curve w/ conf region
        labs(title = "Total Ridership Residuals vs Predicted Values") +
        ylab("Total Ridership Residuals ") +
        xlab("Predicted Values")

p6 <- ggplot(testing, aes(x=count.predict, y=count)) +
        geom_point(shape=1) +    # Use hollow circles
        geom_smooth() +          # Add a loess smoothed fit curve w/ conf region
        labs(title = "Total Ridership Actual vs Predicted Values") +
        ylab("Actual Values") +
        xlab("Predicted Values")

grid.arrange(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Print System and Session Info
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print(sessionInfo())

print(Sys.info())
