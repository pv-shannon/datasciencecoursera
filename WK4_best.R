### Task 2 - Best Function ###

best <- function(state, outcome) {
  
  # read outcome data 
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # create a smaller dataframe for ease of use
  df_two <- as.data.frame(cbind(df[, 2], #hospital
                                df[, 7], #state
                                df[, 11], #heart attack
                                df[, 17], #heart failure
                                df[, 23]), #pneumonia 
                          stringsAsFactors = FALSE)
  
  # give the columns relevent names 
  colnames(df_two) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")     
  
  #################################################################################################
  
  # check for validity 
  if(!state %in% unique(df_two$state)) {
    stop("invalid state")
  }
  
  # check for validity of disease
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("invalid outcome")
  
  #################################################################################################
  
  # choose the variables needed to answer the question 
  selectedData <- df_two[, c("hospital", "state", outcome)]
  
  # change outcome variable to numeric 
  selectedData[, 3] <- suppressWarnings(as.numeric(selectedData[,3]))
  
  # sort the data by lowest mortality and alphabetically 
  selectedData <- selectedData[order(selectedData[, 3], selectedData[, 1]), ]
  
  # pick out the hospitals
  updatedSelection <- selectedData[which(selectedData[, "state"] == state), "hospital"]
  updatedSelection <- updatedSelection[1]
  
  return(updatedSelection)
  
}

# Example questions 
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

# Quiz questions 
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
