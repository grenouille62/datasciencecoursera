library(stringr)
#Basic check arguments validity
checkAgument <- function (d, p, i) {
    dir.exists(d) & (p == "sulfate" | p == "nitrate") & length(i) <= 332
  
}

#Read file helper
# read file into a datafame excludinf NA values
readDataFrame <- function(directory, pollutant, iMonitor) {
  dfI <- read.csv (file.path(directory,paste(str_pad(iMonitor,3,side="left","0"),".csv", sep="")), header = TRUE)
  # compute mean of pollutant excluding NA value
  dfI[!is.na(dfI[,pollutant]),pollutant]
}

# Main function : computing weighted accross monitors specified in id argument
# algo : as we cannot load all 332 file in memory, we compute mean value file by file and
# compute final mean of all these means.
pollutantmean <- function(directory, pollutant, id = 1:332) {
  # fisrt of all : check arguments
  if (!checkAgument(directory, pollutant, id)) {
    print("Error")
  } else {
    #Init a vertor of mean for each monitor
    means <- rep(NA, 332)
    #Init the vector of number of rows (weight) for each monitor 
    weightByMonitor <- rep(NA,332)
    
    #Compute mean of each monitor
    for (i in id) {
      dfI <- readDataFrame(directory, pollutant, i)
      means[i] <- mean (dfI)
      weightByMonitor[i] <- length(dfI)
    }
    #finally, compute weightedmean of means
    result <- weighted.mean(means[!is.na(means)], weightByMonitor[!is.na(weightByMonitor)])
    #print the result
    print(paste("Result : ", result))
    
    #Return the result
    result
  }
}


