library(stringr)
#Basic check arguments validity
checkAgument <- function (d, p, i) {
    dir.exists(d) & (p == "sulfate" | p == "nitrate") & length(i) <= 332
  
}

#Read file helper
readFile <- function(directory, iMonitor) {
  dfI <- read.csv (file.path(directory,paste(str_pad(iMonitor,3,side="left","0"),".csv", sep="")), header = TRUE)
  dfI
}

# read file into a datafame excludinf NA values
readDataFrame <- function(directory, pollutant, iMonitor) {
  dfI <- readFile(directory, iMonitor)
  # compute mean of pollutant excluding NA value
  dfI[!is.na(dfI[,pollutant]),pollutant]
}

readCompleteDataFrame <- function(directory, iMonitor) {
  dfI <- readFile(directory, iMonitor)
  # compute mean of pollutant excluding NA value
  dfReturn <- dfI[!is.na(dfI[,"nitrate"]) & !is.na(dfI[,"sulfate"]),]
  dfReturn
}

# Part 1
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

# Part 2
# Main function
complete <- function(directory, id = 1:332){
  if (!dir.exists(directory)  | length(id) > 332) {
    print("Error")
    return
  }
  #Init dataframe
  dataframeComplete <- NULL
  # append the number of observations of each monitor
  for (i in id) {
    dfI <- readCompleteDataFrame(directory, i)
    dataframeComplete<-rbind(dataframeComplete, c(i, nrow(dfI)))
  }
  #Naming the colums of the result
  colnames(dataframeComplete) <- c("id", "nobs")
  #Naming the rows of the result
  dataframeComplete <- data.frame(dataframeComplete, row.names = seq(1:length(id)))
  
  #return dataframe
  dataframeComplete
}

#Part 3
corr <- function(directory, threshold = 0) {
  dfComplete <- complete(directory)
  dfCompleteThreshold <- dfComplete[dfComplete[,"nobs"] > threshold,]
  correlations <- as.numeric(NULL)
  if (nrow(dfCompleteThreshold) > 0) {
    for (i in 1:nrow(dfCompleteThreshold)) {
      idMonitor = dfCompleteThreshold[i,"id"]
      dfI <- readFile(directory, idMonitor)
      varSulfate <- dfI[,"sulfate"]
      varNitrate <- dfI[,"nitrate"]
      correlation <- cor(varNitrate, varSulfate, use = "na.or.complete")
      correlations <- cbind(correlations, correlation)
    }
  }
  as.numeric(correlations)
}