rankall <- function(outcome, num = "best"){
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[, c(2, 7, 11, 17, 23)]
        #options(warn = -1)
        
        if(!(outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")){
                stop("invalid outcome")
        }
        
        if(class(num) == "character"){
                if(!(num == "best" || num == "worst")){
                        stop("invalid number")
                }
        }
        
        if(outcome == "heart attack"){
                data = data[, c(1, 2, 3)]
        }
        else if (outcome == "heart failure"){
                data = data[, c(1, 2, 4)] 
        }
        else if (outcome == "pneumonia"){
                data = data[, c(1, 2, 5)]
        }
        
        names(data)[3] = "Deaths"
        data[, 3] = suppressWarnings(as.numeric(data[, 3]))
        
        splitted = split(data, data$State)
        result = lapply(splitted, function(x, num) {
                x = x[order(x$Deaths, x$Hospital.Name),]
                
                if(class(num) == "character"){
                        if(num == "best"){
                                return(x$Hospital.Name[1])
                        }
                        else if(num == "worst"){
                                return(x$Hospital.Name[nrow(x)])
                        }
                }
                else {
                        return(x$Hospital.Name[num])
                }
        }, num)
        
        return(data.frame(hospital=unlist(result), state=names(result)))
}