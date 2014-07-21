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
      dftmp <- unique(d["state"])
      unique_states <- sort(dftmp[['state']])
      final <- data.frame()
      for(s in unique_states) {
            filtered <- d[d$state == s, ]  # the dataframe rows for this state
            if(num > nrow(filtered)) {  # if the row requested is larger than the number of hospitals in the state
                  final <- rbind(final, data.frame(hospital = NA, state = s))                  
            } else {
                  n <- cbind(filtered, filtered[outcome])  # append a copy of the outcome column of interest
                  n <- n[c(1,2,6)]  # now remove the original outcome columns
                  out <- n[order(n[[3]], n[[1]], na.last=NA), ]   # sorting by two columns
                  if (num == "best") {
                        val <- unlist(out[1,1])
                        # print(val)
                        if(!is.na(val)) {
                              res <- data.frame(hospital = val, state = s)
                              final <- rbind(final, res)
                        }
                  } else if(num == "worst") {
                        print("worst")
                        val <- unlist(out[nrow(out), 1])
                        print(num)
                        if(!is.na(val)) {
                              res <- data.frame(hospital = val, state = s)
                              final <- rbind(final, res)
                        }     
                  } else {
                        val <- unlist(out[as.integer(num), 1])  # grab the nth row, where n = the 'num' argument
                        if(!is.na(val)) {
                              # print(val)
                              res <- data.frame(hospital = val, state = s)
                              final <- rbind(final, res)
                        }
                  }
            }
      }
      # return(final)
}