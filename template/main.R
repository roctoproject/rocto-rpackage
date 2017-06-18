main <- function(param1, param2){
  
  # For loading data, we assume the working dir is the current dir
  # Never use absolute paths!
  df <- read.csv("data/mydat.csv")
  
  # Put additional R scripts, such as helper functions, in the data folder too.
  source("data/helper.r")
  
  
  
  
  
  # Return a single object that will be saved to a data file.
  return(foo)
}