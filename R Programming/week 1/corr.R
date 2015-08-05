corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  result <- numeric()
  old_dir <- getwd()
  setwd(directory)
  
  for (filename in dir()) {
    data <- read.csv(filename)
    
    complete_data <- complete(getwd(), data[1,4])
    if (complete_data[['nobs']] >= threshold) {
      result <- c(result, 
          cor(as.vector(data$nitrate[!is.na(data$nitrate)&!is.na(data$sulfate)]), 
              as.vector(data$sulfate[!is.na(data$nitrate)&!is.na(data$sulfate)])))
    } 
    
  }
  
  setwd(old_dir)
  result
}