# Exploratory analysis

# Loading training file as well as test file
train <- read.csv('../data/train.csv')
test <- read.csv('../data/test.csv')

# Examine training file
str(train)

# Convert season, holiday, workingday, weather into factor variables
