############################# ASSIGNMENT 1.PT 1 ##############################
pollutantmean <- function(directory, pollutant, id = 1:332) {
  files_full <- list.files(directory, full.names = TRUE)
  dat <- data.frame()
  for(i in id) {
    dat <- rbind(dat, read.csv(files_full[i]))
  }
  return(mean(dat[ ,pollutant], na.rm = TRUE))
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
