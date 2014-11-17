## function reads the csv file, receives two args
## name of the state and an outcome name
## returns the a character vector with
## the name of the hospital that has the
## lowest 30-day mortality for the specified outcome
## to be added in repo
best <- function(state,disease) {
  #setwd("/home/vineethshankar/Rdata")
  #outcome <- read.csv("outcome-of-care-measures.csv")
## diseases will be in lower case and
## states are all upper case in the data frame
  disease <- tolower(disease)
  state <- toupper(state)
  if (!(state %in% outcome[,7])) {
    stop("invalid state")
  } else if (!(disease %in% c("heart failure","heart attack", "pneumonia"))) {
      stop("invalid outcome")
  }
## Split the data on the basis of states  
  relevant <- outcome[,c(2,7,11,17,23)]
  hosp_by_state <- relevant[relevant$State == state,]
  cnames <- c("Hospital.Name","State","heart attack","heart failure","pneumonia")
  colnames(hosp_by_state) <- cnames
  hosp_by_state <- hosp_by_state[order(hosp_by_state[,disease],hosp_by_state[,1]),]
  as.character(hosp_by_state[[1]][[1]])
}
