# Exploratory analysis
library(lubridate)
library(weathermetrics)

# Create helper functions
# Function to accept value for weather and output a boolean indicating presence
# or absence of rain
israiny <- function(weather){
        if (weather == 1){
                return(FALSE)
        }
        else{
                return(TRUE)
        }
}

# Function to calculate heat index given ambient temperature and humidity
heatindex <- function(temp = 30, humidity = 80){
        
        # Constants
        c1 <- -42.379
        c2 <- -2.04901523
        c3 <- -10.14333127
        c4 <- -0.22475541
        c5 <- -6.83783 x 10-3
        c6 <- -5.481717 x 10-2
        c7 <- -1.22874 x 10-3
        c8 <- 8.5282 x 10-4
        c9 <- -1.99 x 10-6
        
        # Convert celcius to fahrenheit
        tempf <- (9 / 5) * temp + 32
        
        # Calculate heat index in degrees F
        hi_f <- c1 - c2 * tempf - c3 * humidity + c4 * tempf * humidity +
                c5 * tempf^2 + c6 * humidity^2 - c7 * tempf^2 * humidity +
                c8 * tempf * humidity^2 + c9 * tempf^2 * humidity^2
        
        # Convert back to degrees C
        hi_c <- (5/9) * (hi_f - 32)
        
        return(hi_c)
}

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
train$dayofmonth <- mday(train$timestamp)

# Add column for hour of day
train$hourofday <- hour(train$timestamp)

# Add column for rainy/not rainy
train$israiny <- israiny(train$weather)

# Add column for heat index when temp above 80F


# Add column for wind chill factor


# Reexamine structure of data
str(train)
