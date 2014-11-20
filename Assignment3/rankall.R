## rankall function receives an outcome
## and a rank
## lists the hospitals with the specified rank
## for the given outcome in each state
rankall <- function(disease, num = "best") {
  outcome <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  disease <- tolower(disease)
  if (!(disease %in% c("heart failure","heart attack", "pneumonia"))) {
    stop("invalid outcome")
  }
  ## Split the data on the basis of states
  
  relevant <- outcome[,c(2,7,11,17,23)]
  relevant <- relevant[!(relevant[,3]== "Not Available") & !(relevant[,4]== "Not Available") & !(relevant[,5]== "Not Available"),]
  cnames_allrank <- c("hospital", "state")
  cnames <- c("Hospital.Name","State","heart attack","heart failure","pneumonia")
  colnames(relevant) <- cnames
  rank <- function(x) {
    if (num > length(x)) {
      print(x)
      return(NA)
    }
      else {
        x[order(x)][num]
    }
  }
# Method 1: split by state, run a loop through the list, find the hospital of the given rank
# method 2: create a vector of names of state, for each iteration
# reduce data by the name of the state, order
  allrank <- data.frame()
## List of mortality rates for each state
  hosp_state <- split(relevant[,disease], relevant$State)
## NA condition if num > number of hospitals in a state 
## Finding the num'th lowest by lapply 
  final <- lapply(hosp_state, rank)
  states <- names(final)
  for(nm in states) {
    allrank <- rbind(allrank, c(nm, final[[nm]]))
  }
  all
}  