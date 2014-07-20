rankall <- function(outcome, num="best") {
      outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      # Extract only the potential columns of interest
      d <- outcome_data[,c(2,7,11,17,23)]
      
      # Convert from character to numeric
      d[, 3] <- suppressWarnings(as.numeric(d[, 3]))
      d[, 4] <- suppressWarnings(as.numeric(d[, 4]))
      d[, 5] <- suppressWarnings(as.numeric(d[, 5]))
      
      colnames(d) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
      if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
            stop("invalid outcome")
      }
      
      unique_states <- unique(d["state"])
      for(s in unique_states[,1]) {
            filtered <- d[d$state == s, ]  # the dataframe rows for this state
            n <- cbind(filtered, filtered[outcome])  # append a copy of the outcome column of interest
            n <- n[c(1,2,6)]  # now remove the original outcome columns
            print(n)
      }
      

#       out <- n[order(n[[3]], n[[1]], na.last=NA), ]
#       
#       if (num == "best") {
#             return(unlist(out[1,1]))
#       } else if(num == "worst") {
#             return(unlist(out[nrow(out),1]))
#       } else {
#             return(unlist(out[as.integer(num),1]))  # grab the nth row, where n = the 'num' argument
#       }
}