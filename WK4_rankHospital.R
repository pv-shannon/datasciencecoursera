### Task 3 - Rank Hospital Function ###

rankhospitals <- function(state, outcome, num) {
  
  # read outcome data 
  frame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # create a smaller dataframe for ease of use
  frame_two <- as.data.frame(cbind(frame[, 2], #hospital
                                   frame[, 7], #state
                                   frame[, 11], #heart attack
                                   frame[, 17], #heart failure
                                   frame[, 23]), #pneumonia 
                             stringsAsFactors = FALSE)
  
  # give the columns relevent names 
  colnames(frame_two) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  #################################################################################################
  
  # check for validity of state
  if(!state %in% unique(frame_two$state)) {
    stop("invalid state")
  }
  
  # check for validity of outcome 
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  #################################################################################################
  
  # chose the variables needed to answer the question 
  frame_selected <- frame_two[, c("hospital", "state", outcome)]
  
  # change the column of interest to numeric and suppress warnings of NA coercion
  frame_selected[, 3] <- suppressWarnings(as.numeric(frame_selected[, 3]))
  
  # remove N/A
  frame_selected <- na.omit(frame_selected)
  
  # filter for the relevent state 
  frame_selected <- frame_selected[which(frame_selected[, "state"] == state), c("hospital", outcome)]
  
  # order 
  frame_selected <- frame_selected[order(frame_selected[, 2], frame_selected[, 1]),]
  
  # create rank column 
  frame_selected$rank <- seq(from = 1, to = nrow(frame_selected))
  
  #################################################################################################
  
  # return the hospital by rank 
  if(num == "best") {
    frame_return <- frame_selected$hospital[1] 
  } else if(num == "worst") {
    frame_return <- frame_selected[which(frame_selected[, "rank"] == max(nrow(frame_selected))), "hospital"]
  } else if(num > nrow(frame_selected)) {
    frame_return <- NA
  } else {
    frame_return <- frame_selected[which(frame_selected[, "rank"] == num), "hospital"]
  }
  
  return(frame_return)
  
}

# Example questions 
rankhospitals("TX", "heart failure", 4)
rankhospitals("MD", "heart attack", "worst")
rankhospitals("MN", "heart attack", 5000)

# Quiz questions 
rankhospitals("NC", "heart attack", "worst")
rankhospitals("WA", "heart attack", 7)
rankhospitals("TX", "pneumonia", 10)
rankhospitals("NY", "heart attack", 7)

