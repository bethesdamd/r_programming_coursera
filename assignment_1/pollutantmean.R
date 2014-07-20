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
  
  # ------ MY NOTES
  # merge two dataframes
  # merge(df1,df2,all=T)
  
  # get the column types of a dataframe: sapply(df, class)
  
  # create empty dataframe with particular column types:
  # nodata <- data.frame(x= numeric(0), y= integer(0), z = character(0))
  
  # merge two datasets (simpler:
  # rbind(emptydf, somedf)
  
  digits <- c("00", "0", "")
  
  for(i in id) { 
    nstring <- paste(digits[nchar(i)], i, sep="")
    filename <- paste(directory, "/", nstring, ".csv", sep="")
    if (i == id[1]) {
      out <- read.csv(filename)
    } else {
      newdata <- read.csv(filename, as.is=T)  # as.is for dates vs. factors
      out <- rbind(out, newdata)
    }
  }
  calc <- mean(out[[pollutant]], na.rm=T)
  return(calc)
}

