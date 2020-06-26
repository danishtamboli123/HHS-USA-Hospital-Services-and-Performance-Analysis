rankhospital <- function(state,outcome,Rank){
  
  #Importing the CSV file in to data.frame "file_data".
  file_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Creating a vector containing Unique states.
  unique_states <- unique(file_data$State)
  
  # Taking state in to account, also Valid State check.
  if (state %in% unique_states) {
    
    #Creating data.frame for required state.
    required_state <- subset(file_data, file_data$State == state)
    
    if (outcome == "heart attack"){
      colnum <- 11
    }
    else if(outcome == "heart failure")
    {
      colnum <- 17
    }
    else if(outcome == "pneumonia")
    {
      colnum <- 23
    }
    else{
      
      #Throwing error and exiting for Invalid Outcome.
      stop("invalid outcome")
    }
    
    #change of required classes from Character to Numeric and Omitting of NA and NAN values.
    required_state[,colnum] <- as.numeric(required_state[,colnum])
    required_state <- na.omit(required_state,cols=colnum)
    
    #Selecting Required Fields from the Sorted list.
    new_hospital_list <- required_state[,c(2,colnum)]
    new_hospital_list <- new_hospital_list[order(new_hospital_list[,2], new_hospital_list[,1]),]
    colnames(new_hospital_list)[2] <- c("rate")
    
    #Adding Ranks for required field,with ties treated by first come rather than average.
    new_hospital_list <- cbind(new_hospital_list,Rank = rank(new_hospital_list$rate, ties.method = "first"))
    
    # Check for Rank Input,to accept Best and Worst other than Numeric Input.
    if (Rank == "worst"){
      Rank = nrow(new_hospital_list)
    }
    else if ( Rank == "best"){
      Rank = 1
    }
    else{
      Rank = Rank
    }
    
    # Returning the Hospital corresponding to the Input arguments (State,Outcome and Rank).
    return(new_hospital_list[Rank,])
  }
  
  else{
    
    #Throwing error and exiting for Invalid State selection.
    stop("invalid state")
  }
  
}