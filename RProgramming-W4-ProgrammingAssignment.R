
makeReadCacheData <- function (fileName = "outcome-of-care-measures.csv") {
  m <- NULL
  ## save the input fileName into the environment different from the current environment
  set <- function(y) {
    fileName <<- y
    m <<- NULL
  }
  ## retrieve the input fileName from the environment different from the current environment
  get <- function() fileName
  
  ## save the data into memory
  setData <- function(data) m <<- data
  ## retrieve the data from memory
  getData <- function()  m
  
  ## list of 4 functions : set/get/setData/getData
  ## these function are callable
  list(set = set, get = get,
       setData =   setData,
       getData = getData)
}

readCacheData <- function(fileData) {
  ## Return a matrix that is the inverse of 'x'
  data <- NULL
  
  ## is x a list made by makeCacheMatrix ?
  if (identical(names(fileData), c("set", "get", "setData", "getData"))) {
    message ("Cacheable data")
    
    ## try to retrieve de result from x
    data <- fileData$getData()
    ## result found
    if(!is.null(data)) {
      ## return the result with a message
      message("Getting data from cache")
      return(data)
    }
    
    ## no result found
    ## retrieve the input data from x
    ## call solve
    data <- read.csv(fileData, header = TRUE, colClasses = "character")
    data <- data[data[,"State"] != "Not Available",]
    ## save the result into memory of x
    fileData$setResult(data)
    ## return the data
    return(data)
  } else {
      ## read and return data
    data <- read.csv(fileData, header = TRUE, colClasses = "character")
    data <- data[data[,"State"] != "Not Available",]
    return (data)
  }
}

checkState <- function (state, data) {
  return (!is.na(state) & nrow(data[data[,"State"] == state,]) > 0)
}

checkOutcome<- function (outcome, data) {
  outcomes <- (c("heart attack", "heart failure", "pneumonia") == outcome)
  return (!is.na(outcome) & length(outcomes[outcomes == TRUE]) > 0) 
}

bestMapOutcomeColumn <- function (outcome, data) {
  df <- data.frame(rbind(c(names(data)[11], names(data)[17], names(data)[23])))
  names(df) <- c("heart attack", "heart failure", "pneumonia")
  as.character(df[1, outcome])
}

#This function reads the outcome-of-care-measures.csv and returns a character vector
#with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
#in that state.
best <- function(state=NA, outcome="") {
  
  ## Read and cache outcome data  
  data <- readCacheData (file.path("rprog_data_ProgAssignment3-data","outcome-of-care-measures.csv"))

  ## Check that state and outcome are valid
  if (checkState(state, data) == FALSE) {stop ("Invalid state") }
  if (checkOutcome(outcome, data) == FALSE) {stop ("Invalid outcome")}
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ##Extract state data outcome
  outcomeColName = bestMapOutcomeColumn(data=data, outcome = outcome)
  sdo <- data[data[, outcomeColName] != "Not Available" & data[,"State"] == state,]
  if (nrow(sdo) == 0) {
    message (paste ("No available outcome ", outcome, " for the state ", state) )
  }
  
  ## compute lowest value of outcome
  an <- as.numeric(sdo[, outcomeColName])
  m <- min(an)
  
  ## Sort an return the first element in case of ex equo
  sort(sdo[an == m, "Hospital.Name"]) [1]
  
}
# this function reads the outcome-of-care-measures.csv and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument.
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Read and cache outcome data  
  data <- readCacheData (file.path("rprog_data_ProgAssignment3-data","outcome-of-care-measures.csv"))
  
  ## Check that state and outcome are valid
  if (checkState(state, data) == FALSE) {stop ("Invalid state") }
  if (checkOutcome(outcome, data) == FALSE) {stop ("Invalid outcome")}
  if (num != "best" & num != "worst" & !is.numeric(num)) {
    stop ("Invalid num rank")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  dorankhospital (state, outcome, num, data)
}

## this function return hospital name in that state with the given rank
## 30-day death rate. The input argument "data" came from caller function
## which read datafile
dorankhospital <- function (state, outcome, num = "best", data) {
  ##Extract state data outcome
  outcomeColName = bestMapOutcomeColumn(data=data, outcome = outcome)
  sdo <- data[data[, outcomeColName] != "Not Available" & data[,"State"] == state,]
  if (nrow(sdo) == 0) {
    message (paste ("No available outcome ", outcome, " for the state ", state) )
  }
  
  an <- as.numeric(sdo[, outcomeColName])
  m <- NULL
  if (num == "best") {
    ## compute lowest value of outcome
    return(sort(sdo[an == min(an), "Hospital.Name"]) [1])
  } else if (num == "worst") {
    ## compute greatest value of outcome
    return(sort(sdo[an == max(an), "Hospital.Name"]) [1])
  } else {
    if (nrow(sdo) < num) {
      return(NA)
    }
    
    ## reorder state data outcome by outcome then hospital name
    ## and extract the list of hospital name 
    orderedRatesHospital <- sdo[order(as.numeric(sdo[,outcomeColName]), sdo[,"Hospital.Name"]),"Hospital.Name"]
    
    ## lookup for the hospital value corresponding to the num rank
    orderedRatesHospital[num]
  }
}

## This function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num.
## this function reuses the dorankhospital function above in applying to all states
rankall <- function(outcome, num = "best") {
  ## Read and cache outcome data  
  data <- readCacheData (file.path("rprog_data_ProgAssignment3-data","outcome-of-care-measures.csv"))
  
  ## Check that state and outcome are valid
  if (checkOutcome(outcome, data) == FALSE) {stop ("Invalid outcome")}
  if (num != "best" & num != "worst" & !is.numeric(num)) {
    stop ("Invalid num rank")
  }
  
  ## For each state, find the hospital of the given rank
  ## reuse dorankhospital and apply it to all state  
  df <- data.frame(sapply(unique(data[,"State"]), function(x) {dorankhospital(state = x, outcome = outcome, num = num, data = data)}))
  
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name order by state the hospital
  df <- cbind(df,row.names(df))
  names(df) <- c("hospital", "state")
  df[order(df$state, df$hospital),]
}