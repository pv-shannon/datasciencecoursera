############################# ASSIGNMENT 1.PT 2 ##############################

complete <- function(directory, id = 1:332) {
  files_full <- list.files(directory, full.names = TRUE)
  nobs <- numeric(0)
  
  for(i in id) {
    data <- read.csv(files_full[i])
    nobs <- c(nobs, sum(complete.cases(data)))
  }
  comp <- data.frame(id, nobs)
  comp
}

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])