rankall <- function(outcome, num = "best") {
  
  hospital<-character()
  
  ## Read Outcome Data
  setwd("C:/Coursera/RProgramming/ProgAssign3/ProgAssign3")
  
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[,c(11,17,23)]<- sapply(data[,c(11,17,23)], function(x) suppressWarnings(as.numeric(gsub("Not Available","NA",x))))
  
  ## Check that state and outcome data are valid
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
  
  #Return hospital name with lowest mortality rate for specified indication
  
  if(outcome=="heart attack") colindex <- 11
  if(outcome=="heart failure") colindex <- 17
  if(outcome=="pneumonia") colindex <- 23
  
  data<-data[order(data[colindex],data[2]),]
  data<-subset(data, !is.na(data[colindex]))
  
  state <- unique(data$State)
  state <- state[order(state)]
    
  for(i in 1:length(state)) {
    index <- num
    if(num > length(data[data$State==state[i],2])) hospital[i]<-NA
    if(num == "best") index <- 1
    if(num == "worst") index <- length(data[data$State==state[i],2])
    hospital[i] <- data[data$State == state[i], 2][index]
  }
  
  data.frame(cbind(hospital,state), row.names = state)
  
  
}