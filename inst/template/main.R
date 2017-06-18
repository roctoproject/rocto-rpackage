main <- function(param1, param2){
  # For loading data, we assume the working dir is the current dir
  # Never use absolute paths!
  df <- read.csv("data/myData.csv")
  
  # Put additional R scripts, such as helper functions, in the data folder too.
  source("data/helper.R")
  
  
  # Compute your output inside this function!
  varA <- helperFunc(df$Test * param1)
  varB <- helperFunc(df$Test * param2)
  
  output <- additionalRoutine(varA, varB)
  
  # Return a single object that can be saved to a data file.
  return(output)
}

additionalRoutine <- function(a,b) {
  # you can also add additional functions directly in this file.
  return(sqrt(a^2 + b^2))
}