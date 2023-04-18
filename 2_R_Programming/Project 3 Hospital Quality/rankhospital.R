library(dplyr)

# function to find a hospital based on the ranking of its 30-day mortality rate 
# compared to other hospitals in that same given state
rankhospital <- function(state, outcome, num ='best') {
  dt <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
 
  # rename column name for personal ease
  dt<-dt %>% 
    rename(hosp_name = 2)
  
  # check if the state is a valid state
  dt <- subset(dt, State == state) 
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
  # filter out any NA values for column
  hospitals <- dt %>% 
    mutate(rank = row_number(),
           rate = suppressWarnings(as.numeric(column))) %>% 
    select(hosp_name, rate) %>% 
    arrange(rate, hosp_name) %>% 
    na.omit()
  
  # return the correct rows depending on the input for num
  if (num == 'best'){
    slice(hospitals, 1) %>% 
      select(hosp_name)
  }
  else if (num == 'worst') {
    slice_tail(hospitals, n= 1) %>% 
      select(hosp_name)
  }
  
  # make sure num is not bigger than the number of rows available
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
