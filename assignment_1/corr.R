corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations

  digits <- c("00", "0", "")  # used in pre-padding digits with '0' for file names
  n <- length(list.files(directory))  # next function needs to know how many files to look at
  df_complete <- complete(directory, 1:n)  # call my 'complete' function to get how many observations are complete
  df_meet_threshold <- subset(df_complete, nobs >= threshold) # only keep rows that meet threshold
  v_file_ids <- df_meet_threshold$id
  out <- numeric()
  for(i in v_file_ids) {
    nstring <- paste(digits[nchar(i)], i, sep="") 
    filename <- paste(directory, "/", nstring, ".csv", sep="")
    data <- read.csv(filename)  
    c <- cor(data$sulfate, data$nitrate, use="p")
    out <- c(out, c)  
  }
  return(out)
}

