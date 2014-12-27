add.lag.variables <- function (bikedataframe) {
  # Load libraries
  library(datacombine)
  
  # Copy bikedataframe to a new dataframe
  newbikedata <- bikedataframe

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create 24 lag variables for casual rider count
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -1)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -2)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -3)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -4)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -5)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -6)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -7)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -8)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -9)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -10)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -11)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -12)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -13)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -14)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -15)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -16)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -17)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -18)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -19)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -20)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -21)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -22)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -23)
  newbikedata <- slide(newbikedata, Var = "casual", slideBy = -24)

  return(newbikedata)
  
}

