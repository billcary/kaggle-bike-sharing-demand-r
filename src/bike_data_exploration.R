#
# Project: Predict Bike Sharing Demand using R
# Author : Bill Cary
# Date   : 14 Sept 20014
#

# Source data pre-processing routines
source('./process_raw_data_file.R')

#------------------------------------------------------------------
# Helper Functions

panel.hist <- function(x, ...)
{
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks; nB <- length(breaks)
        y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
        
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- abs(cor(x, y))
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste0(prefix, txt)
        if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
        text(0.5, 0.5, txt, cex = cex.cor * r)
}
#-----------------------------------------------------------------------

#Loading training file as well as test file - CHANGE THIS PATH APPROPRIATELY
train <- read.csv('../data/train.csv')
test <- read.csv('../data/test.csv')

# Pre-process both training and test data
processed_train <- process.bike.data(train)
processed_test <- process.bike.data(test)

# Examine structure of training data
str(processed_train)

# Create scatterplot matrix to investigate pairwise relationships
pairs(processed_train[, 2:18],
      diag.panel = panel.hist,
      lower.panel = panel.smooth)
