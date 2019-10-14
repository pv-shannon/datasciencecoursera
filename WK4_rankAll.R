## Task 4 - Rank All ##

rankall <- function(outcome, num = "best") {
  
  # read data
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- as.data.frame(cbind(data[, 2], #hospital
                              data[, 7], #state
                              data[, 11], #heart attack
                              data[, 17], #heart failure
                              data[, 23]), #pneumonia 
                        stringsAsFactors = FALSE)
  
  colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ################################################################################################
  
  # check validity
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  if(class(num) == "character") {
    if(!(num == "best" || num == "worst")) {
      stop("invalid number")
    }
  }
  
  ################################################################################################
  
  # create smaller data.frames based on the problem and as.numeric
  
  if(outcome == "heart attack") {
    data <- data[, 1:3]
  } else if(outcome == "heart failure") {
    data <- data[, c(1,2,4)]
  } else if(outcome == "pneumonia") {
    data <- data[, c(1,2,5)]
  }
  
  names(data)[3] <- "deaths"
  data[, 3] <- suppressWarnings(as.numeric(data[, 3]))
  data <- data[!is.na(data[,3]), ]
  
  ################################################################################################
  
  # for each state, find the hospital of the given rank 
  
  splitted <- split(data, data$state)
  rank <- lapply(splitted, function(x, num) {
    x <- x[order(x$deaths, x$hospital), ]
    
    if(class(num) == "character") {
      if(num == "best") {
        return(x$hosptial[1])
      } 
      else if(num == "worst") {
        return(x$hospital[nrow(x)])
      }
    } else {
      return(x$hospital[num])
    }
    
  }, num)
  
  ################################################################################################
  
  # return data.frame
  
  ans <- as.data.frame(cbind(hopsital=unlist(rank), state=names(rank)))
  return(ans)
  
}

# example questions 
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure", 1), 10)

# test questions 
r <- rankall("heart attack", 4) ##find HI
r[which(r[, "state"] == "HI"), "hopsital"]

r <- rankall("pneumonia", "worst")
r[which(r[, "state"] == "NJ"), "hopsital"]

r <- rankall("heart failure", 10)
r[which(r[, "state"] == "NV"), "hopsital"]

#################################################################
