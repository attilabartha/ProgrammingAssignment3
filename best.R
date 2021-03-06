#First part of Programming Assignment 3 for R Programming on Coursera

best <- function(state, outcome){
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        options(warn=-1)
        
        if(!(state %in% data$State)){
                stop("invalid state")
        }
        
        if(outcome == "heart attack"){
                findBest(11, state, data)
        }
        else if(outcome == "heart failure"){
                findBest(17, state, data)
        }
        else if(outcome == "pneumonia"){
                findBest(23, state, data)
        }
        else{
                stop("invalid outcome")
        }

}

findBest <- function(outcomeNumber, state, data) {
        
        data[, outcomeNumber] <- as.numeric(data[, outcomeNumber])
        
        data<-subset(data,data$State==state) # only need info for given state
        
        valMin<-min(data[[outcomeNumber]],na.rm=TRUE) # best in this case = minimum
        
        data<-subset(data,data[[outcomeNumber]]==valMin)
        
        data<-data[order(data[["Hospital.Name"]]),]
        
        return(data[1,"Hospital.Name"]) # return best hospital name
}