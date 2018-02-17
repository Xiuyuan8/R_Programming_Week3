# Rank function 

rankall <- function(outcome,id) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data_ned <- as.data.frame(cbind(data[,2], # hospital name 
                                  data[,7], # state name
                                  data[,11], # heart attack 
                                  data[,17],  # heart failure 
                                  data[,23],stringsAsFactors = FALSE))
  colnames(data_ned) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia") # pneumonia
  data_ned[, eval(outcome)] <- as.numeric(data_ned[, eval(outcome)])
  
  ## Check that state and outcome are valid
  if (!outcome %in% c("heart attack", "heart failure","pneumonia")){
    stop('invalid outcome')
  } else if ( is.numeric(id)){
    by_state <- with(data_ned,split(data_ned,state))
    ordered <- list()
    for (i in seq_along(by_state)){
      by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                           by_state[[i]][, "hospital"]), ]
      ordered[[i]]  <- c(by_state[[i]][id, "hospital"], by_state[[i]][, "state"][1])
    }
    result <- do.call(rbind,ordered)
    output <- as.data.frame(result,row.names= result[,2],stringsAsFactors= FALSE)
    names(output) <- c("hospital","state")
  } else if (!is.numeric(id)) {
    if (id=="best") {
      by_state <- with(data_ned,split(data_ned,state))
      ordered <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"]),]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital","state")])
      }
      result <- do.call(rbind,ordered)
      output <- as.data.frame(result,stringsAsFactors= FALSE)
      rownames(output) <- output[,2]
      
    } else if (id =="worst") { 
      by_state <- with(data_ned,split(data_ned,state))
      ordered <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"],decreasing = TRUE),]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital","state")])
      }
      result <- do.call(rbind,ordered)
      output <- as.data.frame(result,stringsAsFactors= FALSE)
      rownames(output) <- output[,2]
      
    } else {
      stop('invalid id')
    }
  }
  return (output)
}

