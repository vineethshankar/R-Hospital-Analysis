## function reads the csv file, receives two args
## name of the state and an outcome name
## returns the a character vector with
## the name of the hospital that has the
## lowest 30-day mortality for the specified outcome
best <- function(state,disease) {
  setwd("/home/vineethshankar/Rdata")
  outcome <- read.csv("outcome-of-care-measures.csv")
  if (!(toupper(state) %in% outcome[,7])) {
    stop("invalid state")
  } else if (!(tolower(disease) %in% c("heart failure","heart attack", "pneumonia"))) {
      stop("invalid outcome")
  }
    hosp_disease <- outcome[,c(2,11,17,23)]
  head(hosp_disease[order(hosp_disease[2]),])
}
