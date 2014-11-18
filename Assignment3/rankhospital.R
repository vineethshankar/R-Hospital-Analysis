## rankhospital takes a state, an outcome
## and a ranking to prints the name
## of the hospital for that outcome (num)
## num > number of hospitals, return NA
rankhospital <- function(state,disease, num) {
  outcome <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  disease <- tolower(disease)
  state <- toupper(state)
  if (!(state %in% outcome[,7])) {
    stop("invalid state")
  } else if (!(disease %in% c("heart failure","heart attack", "pneumonia"))) {
    stop("invalid outcome")
  }
    ## Split the data on the basis of states  
    relevant <- outcome[,c(2,7,11,17,23)]
    relevant <- relevant[!(relevant[,3]== "Not Available") & !(relevant[,4]== "Not Available") & !(relevant[,5]== "Not Available"),]
    hosp_by_state <- relevant[relevant$State == state,]
    cnames <- c("Hospital.Name","State","heart attack","heart failure","pneumonia")
    colnames(hosp_by_state) <- cnames
    
    ## Converting outcome columns to numeric for proper sorting
    hosp_by_state[,c(3,4,5)] <- sapply(hosp_by_state[,c(3,4,5)],as.numeric)
    
    ## ordering by ascending order of mortality rates (1st arg)
    ## breaking ties by hospital name (2nd arg)
    hosp_by_state <- hosp_by_state[order(hosp_by_state[,disease],hosp_by_state[,1]),]
    
    staterank <- cbind(hosp_by_state[,1], Rate = hosp_by_state[,disease], Rank = 1:nrow(hosp_by_state))
    staterank[staterank[,3] == num,1]
}