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


# function to find the hospital with the lowest 30-day mortality for a given outcome in a given state
best <- function(state, outcome) {
  dt <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
  
  # rename column name for personal ease
  dt<-dt %>% 
    rename(hosp_name = 2)
  
  # check if the state is a valid state
  dt <- subset(dt, State == state) # why can't you use filter in the hospitals part instead of subset
  if (!any(state == dt$State)){
    stop("Invalid state")
  }
  
  # check if the outcome is a valid outcome
  if (!outcome %in% c("heart attack", 'heart failure', 'pneumonia')){
    stop("Invalid outcome")
  }
  
  # make the data frame only return the column for the corresponding outcome
  if (outcome == 'heart attack'){
    column <- dt[, 11]
  } else if (outcome == 'heart failure') {
    column <- dt[, 17]
  } else if (outcome == 'pneumonia') {
    column <- dt[, 23]
}
  # provide numeric values for the columns and arrange them in ASC order
  # only return first row and filter out any NA values for column
  hospitals <- dt %>% 
    select(hosp_name) %>% 
    arrange(suppressWarnings(as.numeric(column)), hosp_name) %>% 
    slice(1) %>% 
    filter(complete.cases(.))
    
  hospitals
}

best("TX", "heart failure")

