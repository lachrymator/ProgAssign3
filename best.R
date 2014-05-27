best<-function(state, outcome){
  
  ## Read Outcome Data
  setwd("C:/Coursera/RProgramming/ProgAssign3/ProgAssign3")
  
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[,c(11,17,23)]<- sapply(data[,c(11,17,23)], function(x) suppressWarnings(as.numeric(gsub("Not Available","NA",x))))
  
  ## Check that state and outcome data are valid
  
  if(!state %in% data$State) stop("invalid state")
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")  

  
  #Return hospital name with lowest mortality rate for specified indication
  
  if(outcome=="heart attack") colindex <- 11
  if(outcome=="heart failure") colindex <- 17
  if(outcome=="pneumonia") colindex <- 23
  
  #best in state
    
  data<-data[order(data[colindex],data[2]),]
  return(data[data$State==state,2][1])
}