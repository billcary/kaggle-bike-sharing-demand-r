process.bike.data <- function (bikedataframe) {
  # Load libraries
  library(lubridate)
  library(weathermetrics)
  
  # Create helper functions
  # Function to accept value for weather and output a boolean indicating presence
  # or absence of rain
  israiny <- function(weather){
          if (weather == 1){
                  return(0)
          }
          else{
                  return(1)
          }
  }
  
  # Function to calculate heat index given ambient temperature and humidity
  # heatindex <- function(temp = 30, humidity = 80){
  #         
  #         # Constants
  #         c1 <- -42.379
  #         c2 <- -2.04901523
  #         c3 <- -10.14333127
  #         c4 <- -0.22475541
  #         c5 <- -6.83783 x 10-3
  #         c6 <- -5.481717 x 10-2
  #         c7 <- -1.22874 x 10-3
  #         c8 <- 8.5282 x 10-4
  #         c9 <- -1.99 x 10-6
  #         
  #         # Convert celcius to fahrenheit
  #         tempf <- (9 / 5) * temp + 32
  #         
  #         # Calculate heat index in degrees F
  #         hi_f <- c1 - c2 * tempf - c3 * humidity + c4 * tempf * humidity +
  #                 c5 * tempf^2 + c6 * humidity^2 - c7 * tempf^2 * humidity +
  #                 c8 * tempf * humidity^2 + c9 * tempf^2 * humidity^2
  #         
  #         # Convert back to degrees C
  #         hi_c <- (5/9) * (hi_f - 32)
  #         
  #         return(hi_c)
  # }
  
  
  # Function to calculate wind chill given ambient temperature and wind speed
  # WC = 35.74 + 0.6215T - 35.75V^0.16 + 0.4275T*(V^0.16)
  windchill <- function(temp = 30, windspeed = 10){
          library(weathermetrics)
          
          tempf <- celsius.to.fahrenheit(temp)
          
          # Windchill only defined for ambient temps at or below 50F and wind
          # at least 3mph.  If either threshold is not met, then simply return
          # the ambient temp passed to the function as the temp argument.
          if (tempf > 50 | windspeed < 3){
                  return(temp)
          }
          
          # Wind chill calculation as provided by the U.S. National Weather
          # Service at http://www.nws.noaa.gov/os/windchill/index.shtml
          # Formula requires input of temp in F and wind speed in MPH
          wcn <- 35.74 + 0.6215*replicate(12,tempf) + (0.4275*tempf-35.75) %*% 
                  (windspeed^0.16)
          
          wchill <- fahrenheit.to.celsius(wcn)
          
          return(wchill)
  }
  
  # Copy bikedataframe to a new dataframe
  newbikedata <- bikedataframe
  
  # Convert season, holiday, workingday, weather into factor variables
#   newbikedata$season <- factor(newbikedata$season, levels = c(1, 2, 3, 4),
#                          labels = c('Spring', 'Summer', 'Fall', 'Winter'))
#   
#   newbikedata$holiday <- factor(newbikedata$holiday, levels = c(0, 1),
#                          labels = c('No', 'Yes'))
#   
#   newbikedata$workingday <- factor(newbikedata$workingday, levels = c(0, 1),
#                           labels = c('No', 'Yes'))
#   
#   newbikedata$weather <- factor(newbikedata$weather, levels = c(1, 2, 3, 4),
#           labels = c('1: Clear, Few clouds, Partly cloudy, Partly cloudy',
#                 '2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist',
#                 '3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds',
#                 '4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog'))
  
  
  # Add column that converts datatime into a true datetime data type
  newbikedata$timestamp <- strptime(newbikedata$datetime, '%Y-%m-%d %H:%M:%S')
  
  # Add column specifying day of week
  newbikedata$dayofweek <- wday(newbikedata$timestamp)
  
  # Add column specifying day of month
  newbikedata$dayofmonth <- mday(newbikedata$timestamp)
  
  # Add column for hour of day
  newbikedata$hourofday <- hour(newbikedata$timestamp)
  
  # Add column for rainy/not rainy
  newbikedata$israiny <- israiny(newbikedata$weather)
  
  # Add column for heat index when temp above 80F
  newbikedata$heatindex <- heat.index(t=newbikedata$temp, rh=newbikedata$humidity,
                                temperature.metric = 'celcius')
  
  # Add column for wind chill factor
  newbikedata$windchill <- windchill(newbikedata$temp, newbikedata$windspeed)

  # Drop datetime factor column
  newbikedata$datetime <- NULL
  
  # Reorder columns
  if(ncol(newbikedata) == 18){
          newbikedata <- newbikedata[c(12, 1:8, 13:18, 9:11)]
  }
  else{
          newbikedata <- newbikedata[c(9, 1:8, 10:15)]
  }

  return(newbikedata)
  
}

