library(dplyr)


rankhospital <- function(state, outcome, num ='best') {
  dt <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
  dt<-dt %>% 
    rename(hosp_name = 2)
  
  dt <- subset(dt, State == state) 
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
    mutate(rank = row_number(),
           rate = suppressWarnings(as.numeric(column))) %>% 
    select(hosp_name, rate) %>% 
    arrange(rate, hosp_name) %>% 
    na.omit()
  
  if (num == 'best'){
    slice(hospitals, 1) %>% 
      select(hosp_name)
  }
  else if (num == 'worst') {
    slice_tail(hospitals, n= 1) %>% 
      select(hosp_name)
  }
  
  else if (num <= nrow(hospitals)) {
    hospitals %>% 
      slice(num) %>% 
      select(hosp_name)
    }
    else {
      return('NA')
    }

}

rankhospital("NC", "heart attack", "worst")
