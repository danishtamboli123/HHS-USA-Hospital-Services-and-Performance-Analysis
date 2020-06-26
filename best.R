#best function to suggest best hospital in a state for a particular treatment.
best <- function(state, outcome){
  
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
     
    #Selection of best Hospital from the Sorted list 
    best_hosptial <- subset(required_state, required_state[,colnum] == min(required_state[,colnum] ))
    
    
    return(best_hosptial[,2])
    
  }
  
  else{
    
    #Throwing error and exiting for Invalid State selection.
    stop("invalid state")
  }
} 