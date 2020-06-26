rankall <- function(outcome,Rank){
  
  df_hospital_rank <- data.frame()
  
  #Importing the CSV file in to data.frame "file_data".
  file_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Creating a vector containing Unique states.
  unique_states <- unique(file_data$State)
  
  #Checking Input of Argument Outcome.
  if (outcome == "heart attack"){
    colnum <- 11
  }
  
  else if(outcome == "heart failure"){
    colnum <- 17
  }
  
  else if(outcome == "pneumonia"){
    colnum <- 23
  }
  
  else{
    
    #Throwing error and exiting for Invalid Outcome.
    stop("invalid outcome")
    
  }
  
  # Check for Rank Input,to accept Best and Worst other than Numeric Input.
  if (Rank == "worst"){
    Rank = nrow(new_hospital_list)
  }
  else if ( Rank == "best"){
    Rank = 1
  }
  else{
    
  }
  
  
  # Taking state in to account, also Valid State check.
  for (state in unique_states) {
    
    #Creating data.frame for required state.
    required_state <- subset(file_data, file_data$State == state)
    
    #change of required classes from Character to Numeric and Omitting of NA and NAN values.
    required_state[,colnum] <- as.numeric(required_state[,colnum])
    required_state <- na.omit(required_state,cols=colnum)
    
    
    #Selecting Required Fields from the Sorted list.
    new_hospital_list <- required_state[,c(2,colnum,7)]
    new_hospital_list <- new_hospital_list[order(new_hospital_list[,2], new_hospital_list[,1]),]
    colnames(new_hospital_list)[2] <- c("rate")
    
    

    #Adding Ranks for required field,with ties treated by first come rather than average.
    new_hospital_list <- cbind(new_hospital_list,Rank = rank(new_hospital_list$rate, ties.method = "first"))
    
    #Output corresponding to Rank and Max Rank in respective State.
    if (Rank <= length(new_hospital_list$Rank)){
      
    # Returning the Hospital corresponding to the Input arguments (State,Outcome and Rank).
    df_hospital_rank <- rbind(df_hospital_rank,new_hospital_list[Rank,])
    
    }
    else{
      #Binding Row with NA Values where RANK> Max State Rank.
      df_hospital_rank <- rbind(df_hospital_rank,c(NA,NA,state,Rank))
    }
  }
  #Scoping of Data Frame to Global Enviorment for Later use.
  df_hospital_rank <<- df_hospital_rank
  
  #Arrangement to COlumns with order specificed.
  df_hospital_rank <- df_hospital_rank[order(df_hospital_rank[,3], df_hospital_rank[,2], df_hospital_rank[,1]),]

  #Returning Data Frame as requested to user.
  return(df_hospital_rank)
}
