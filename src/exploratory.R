# Exploratory analysis

# Loading training file as well as test file
train <- read.csv('../data/train.csv')
test <- read.csv('../data/test.csv')

# Examine training file
str(train)

# Convert season, holiday, workingday, weather into factor variables
train$season <- factor(train$season, levels = c(1, 2, 3, 4),
                       labels = c('Spring', 'Summer', 'Fall', 'Winter'))

train$holiday <- factor(train$holiday, levels = c(0, 1),
                       labels = c('No', 'Yes'))

train$workingday <- factor(train$workingday, levels = c(0, 1),
                        labels = c('No', 'Yes'))

train$weather <- factor(train$weather, levels = c(1, 2, 3, 4),
        labels = c('1: Clear, Few clouds, Partly cloudy, Partly cloudy',
              '2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist',
              '3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds',
              '4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog'))


# Add column that converts datatime into a true datetime data type
train$timestamp <- strptime(train$datetime, '%Y-%m-%d %H:%M:%S')

# Add factor column specifying day of week
train$dayofweek <- factor(weekdays(train$timestamp))

# Add column specifying day of month
train$dayofmonth <- train$timestamp$mday

# Rexamine structure of data
str(train)
