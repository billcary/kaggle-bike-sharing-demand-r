add.lag.variables <- function (bikedataframe) {
  # Load libraries
  library(DataCombine)
  
  # Copy bikedataframe to a new dataframe
  newbikedata <- bikedataframe
  
  newbikedata <- newbikedata[order(newbikedata$timestamp),]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 24 hours of lag variables for atemp rider count
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for(x in seq(-1,-24,-1)) { 
          newbikedata <- slide(newbikedata, Var = "atemp", slideBy = x) 
  } 

  return(newbikedata)
  
}

