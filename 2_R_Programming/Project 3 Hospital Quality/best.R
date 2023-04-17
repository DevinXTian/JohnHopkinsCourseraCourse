getwd()
setwd('coursera R//CourseraProgrammingAssignments//Project 3 Hospital Quality')
outcome <- read.csv("outcome-of-care-measures.csv",
                                     colClasses = 'character')
library(dplyr)

head(outcome)
str(outcome)
colnames(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])



best <- function(state, outcome) {
  dt <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
  dt<-dt %>% 
    rename(hosp_name = 2)
  
  dt <- subset(dt, State == state) # why can't you use filter in the hospitals part instead of subset
  if (!any(state == dt$State)){
    stop("Invalid state")
  }
  if (!outcome %in% c("heart attack", 'heart failure', 'pneumonia')){
    stop("Invalid outcome")
  }
  if (outcome == 'heart attack'){
    column <- dt[, 11]
  } else if (outcome == 'heart failure') {
    column <- dt[, 17]
  } else if (outcome == 'pneumonia') {
    column <- dt[, 23]
}
  
  hospitals <- dt %>% 
    select(hosp_name) %>% 
    arrange(suppressWarnings(as.numeric(column)), hosp_name) %>% 
    slice(1) %>% 
    filter(complete.cases(.))
    
  hospitals
}

best("TX", "heart failure")

