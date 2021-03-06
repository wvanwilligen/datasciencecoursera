pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  old_dir <- getwd()
  setwd(directory)
  
  data <- numeric()
  
  for (i in id) {
    filename = paste("00", as.character(i), ".csv", sep="")
    filename <- substr(filename, nchar(filename)-6, nchar(filename))

    newData <- read.csv(filename)
    
    data <- c(data, newData[[pollutant]])
    
  }
  
  setwd(old_dir)
  
  mean(data, na.rm=T)
  
}
