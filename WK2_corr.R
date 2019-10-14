############################# ASSIGNMENT 1.PT 3 ##############################

corr <- function(directory, threshold = 0) {
  files_full <- list.files(directory, full.names = TRUE)
  dat <- vector()
  
  for(i in 1:length(files_full)) {
    data <- read.csv(files_full[i])
    data <- data[complete.cases(data), ]
    csum <- nrow(data)
    
    if(csum > threshold) {
      dat <- c(dat, cor(data$nitrate, data$sulfate))
    }
  }
  
  return(dat)
  
}

cr <- corr("specdata")
cr <- sort(cr)
RNGversion("3.5.1")
set.seed(868)
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

