best <- function(state,outcome = 3||4||5){
  setwd("C:/Users/Dell/Desktop/Coursera/DataScienceCoursera/DataScienceCoursera/data_assignment_3")
  files <- dir()
  data <- read.csv(files[3]) 
  data.required <- data[,c(7,2,11,17,23)]
 if(is.character(state)){
   My_format <- split(data.required,data.required$State)
    my_data <- My_format[state]
    mydata <- data.frame(my_data)
    b <- split(mydata[,2],mydata[,outcome])
    print(b[1])
    }
}
## to get the correct answer install 'dplyr' packages
rankhospital <- function(state, outcome, rank){
  setwd("C:/Users/Dell/Desktop/Coursera/DataScienceCoursera/DataScienceCoursera/data_assignment_3")
  output <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE, na.strings = c("Not Available"))
  r <- output[,c(2,7,11,17,23)]
  if(!(state %in% r$State)){
    stop("invalid state")
  }
  if(outcome=="heart attack"){
    coln <-3
  }
  else if(outcome=="heart failure"){
    coln <-4
  }
  else if(outcome=="pneumonia"){
    coln <-5
  }
  else{
    stop("invalid outcome")
  }
  df_state <-filter(r, State == state)
  df_op <-arrange(df_state,df_state[,coln],Hospital.Name)
  df_op <- na.omit(df_op)
  if(rank=="best"){
    return(df_op[1,1]) 
  }
  else if(rank=="worst"){
    return(df_op[nrow(df_op),1])
  }
  else{
    return(df_op[rank,1])
  }
}
