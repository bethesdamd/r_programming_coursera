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
  
  # -------------- MY NOTES
  
  # GET THE NUMBER OF COMPLETE DATA CASES IN A DATA FRAME:
  # sum(complete.cases(df))
  
  # SAVE: Create new data frame and append to it:
  # a=data.frame(matrix(NA, nrow=0, ncol=2))
  # names(a) <- c("id", "nobs")
  # newrow <- c(1,20)
  # a <- rbind(a, newrow)
  
  digits <- c("00", "0", "")
  final <- data.frame(matrix(NA, nrow=0, ncol=2))
  for(i in id) { 
    nstring <- paste(digits[nchar(i)], i, sep="")
    filename <- paste(directory, "/", nstring, ".csv", sep="")
    df <- read.csv(filename)
    num_complete <- sum(complete.cases(df))
    this_result <- c(i, num_complete)
    final <- rbind(final, this_result)
  }
  names(final) <- c("id", "nobs")
  return(final)
}