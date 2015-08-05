complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  result <- data.frame(id=integer(), nobs=integer())
  
  old_dir <- getwd()
  setwd(directory)
  
  for (i in id) {
    filename = paste("00", as.character(i), ".csv", sep="")
    filename <- substr(filename, nchar(filename)-6, nchar(filename))
    data <- read.csv(filename)
    count <- length(data$ID[!is.na(data[["nitrate"]]) & !is.na(data[["sulfate"]])]) 
    result<-merge(result, data.frame(id=i, nobs=count), all=T, sort=F)
  }
  
  setwd(old_dir)
  result
}