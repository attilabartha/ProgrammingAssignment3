rankhospital <- function(state, outcome, num = "best") {
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        source("best.R")
        options(warn = -1)
        
        if(!(state %in% data$State)) {
                stop("invalid state")
        }
        
        if(outcome == "heart attack") {
                if (num == "worst"){
                        return(findWorst(11, state, data))
                }
                
                else {
                        data[, 11] <- as.numeric(data[, 11])
                        dataFrame <- data.frame(table(data[["State"]]))
                        dataSubset <- subset(data, data[["State"]] == state)
                        lengthSubset <- length(dataSubset[["State"]])
                        
                        dataSubset <- dataSubset[order(dataSubset[[11]], dataSubset[["Hospital.Name"]]),]
                        dataSubset <- subset(dataSubset,subset=(!is.na(dataSubset[[11]])))
                }
        }
                
        else if (outcome == "heart failure"){
                if (num == "worst"){
                        return(findWorst(17, state, data))
                }
                
                else {
                        data[, 17] <- as.numeric(data[, 17])
                        dataFrame <- data.frame(table(data[["State"]]))
                        dataSubset <- subset(data, data[["State"]] == state)
                        lengthSubset <- length(dataSubset[["State"]])
                        
                        dataSubset <- dataSubset[order(dataSubset[[17]], dataSubset[["Hospital.Name"]]),]
                        dataSubset <- subset(dataSubset,subset=(!is.na(dataSubset[[17]])))
                }
        }
        
        else if(outcome == "pneumonia"){
                if (num == "worst"){
                        return(findWorst(23, state, data))
                }
                
                else {
                        data[, 23] <- as.numeric(data[, 23])
                        dataFrame <- data.frame(table(data[["State"]]))
                        dataSubset <- subset(data, data[["State"]] == state)
                        lengthSubset <- length(dataSubset[["State"]])
                        
                        dataSubset <- dataSubset[order(dataSubset[[17]], dataSubset[["Hospital.Name"]]),]
                        dataSubset <- subset(dataSubset,subset=(!is.na(dataSubset[[23]])))
                }
        }
        else {
                stop("invalid outcome")
        }

        
        if(is.numeric(num)&&(num>lengthSubset)){
                return(NA)
        }
        
        if(!is.numeric(num)&&(num=="best")){
                return(best(state, outcome))
        }
        
        if(is.numeric(num)){
                return(dataSubset[num, "Hospital.Name"])
        }
}

findWorst <- function(outcomeNumber, state, data) {
        
        data[, outcomeNumber] <- as.numeric(data[, outcomeNumber])
        
        data<-subset(data,data$State==state) # only need info for given state
        
        valMax<-max(data[[outcomeNumber]],na.rm=TRUE) # worst in this case = maximum
        
        data<-subset(data,data[[outcomeNumber]]==valMax)
        
        data<-data[order(data[["Hospital.Name"]]),]
        
        return(data[1,"Hospital.Name"]) # return worst hospital name
}