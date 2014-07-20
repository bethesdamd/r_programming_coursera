rankhospital <- function(state, outcome, num = "best") {
      # Example of ordering data frame on two columns: 
      # dd[with(dd, order(-z, b)), ]
      
      # Native data columns:
      # 2: Hospital Name
      # 7: State
      # 11: Heart Attack
      # 17: Heart Failure
      # 23: Pneumonia
      outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      # Extract only the potential columns of interest
      d <- outcome_data[,c(2,7,11,17,23)]
      
      # Convert from character to numeric
      d[, 3] <- suppressWarnings(as.numeric(d[, 3]))
      d[, 4] <- suppressWarnings(as.numeric(d[, 4]))
      d[, 5] <- suppressWarnings(as.numeric(d[, 5]))
      
      colnames(d) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
      
      ## Check that state and outcome are valid
      if(!is.element(state, d$state)) {
            stop("invalid state")
      }
      
      if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
            stop("invalid outcome")
      }
      
      filtered <- d[d$state == state, ]  # Dataframe containing only rows with the state of interest
      
      # vals = filtered[outcome]  # just the outcome column of interest
      # print(filtered)
      # indexes_of_min <- which(vals == min(vals, na.rm=T))  # the index(es) of the minimum row(s)

      n <- cbind(filtered, filtered[outcome])  # append the outcome column of interest
      n <- n[c(1,2,6)]  # now remove the original outcome columns
      out <- n[order(n[[3]], n[[1]], na.last=NA), ]
      # out <- cbind(out, c(1:nrow(out)))  # add numerical series column to the end, used for ranking
      
      if (num == "best") {
            return(unlist(out[1,1]))
      } else if(num == "worst") {
            return(unlist(out[nrow(out),1]))
      } else {
            return(unlist(out[as.integer(num),1]))  # grab the nth row, where n = the 'num' argument
      }
}