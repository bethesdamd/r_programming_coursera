# best.R

best <- function(state, outcome) {
      # Check the validity of arguments
      
      # 2: Hospital Name
      # 7: State
      # 11: Heart Attack
      # 17: Heart Failure
      # 23: Pneumonia
      outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      # Extract only the columns of interest
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
      vals = filtered[outcome]  # just the outcome column
      # print(filtered)
      indexes_of_min <- which(vals == min(vals, na.rm=T))  # the index(es) of the minimum row(s)
      print(length(indexes_of_min))
      if (length(indexes_of_min == 1)) {
            print(filtered[indexes_of_min, 1])
      } else {  # more than one has the minimum value
            
      }
}