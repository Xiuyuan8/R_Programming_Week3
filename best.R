# Best function 

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data_ned <- as.data.frame(cbind(data[,2], # hospital name 
                                  data[,7], # state name
                                  data[,11], # heart attack 
                                  data[,17],  # heart failure 
                                  data[,23],stringsAsFactors = FALSE))
  colnames(data_ned) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia") # pneumonia
  ## Check that state and outcome are valid
  if (!state %in% data_ned[,"state"]){
    stop('invalid state')
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else {
    si <- which(data_ned[,"state"]==state)
    ts <- data_ned[si,] # extracting data for that state 
    oi <- as.numeric(as.character(ts[,eval(outcome)]))  # need to add as.character! 
    # value of outcome for hospitals in that state 
    min_val <- min(oi,na.rm=TRUE)
    result <-ts[,"hospital"][order(oi)]
    output <- result[order(result)]
  }
  ## Return hospital name in that state with lowest 30-day death
  return(output)
  ## rate
}



