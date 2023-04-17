library(dplyr)

rankall <- function(outcome, num ='best') {
  dt <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
  dt<-dt %>% 
    rename(hospital = 2, state = 7)
  
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
    mutate(rate = suppressWarnings(as.numeric(column))) %>% 
    filter(complete.cases(rate)) %>% 
    group_by(state) %>% 
    select(hospital, rate, state) %>% 
    arrange(state, rate, hospital, .by_groups = TRUE) %>% 
    # could not figure out how to keep the na values for hospital (tried order function)
    mutate(rank = row_number()) 
  
  if (num == 'best'){
    hospitals %>% 
      filter(rank == 1) %>% 
      select(hospital, state)
  }
  else if (num == 'worst') {
    hospitals %>% 
      filter(rank == max(rank)) %>% 
      select(hospital, state)
  }
  
  else if (num <= nrow(hospitals)) {
    hospitals %>% 
      filter(rank == num) %>% 
      select(hospital, state)
  }
  else {
    return('NA')
  }
  
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
