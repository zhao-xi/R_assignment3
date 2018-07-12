rankhospital <- function(state, outcome, num = "best"){
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv",
                                 colClasses = "character")
        
        ## Check that state and outcome are valid
        state_vect <- as.character(unique(outcome_data[["State"]]))
        if(!(state %in% state_vect)){
                message("Error in best("+state+", "+outcome+") : "
                        +"invalid state")
                stop()
        }
        outcome_vect <- c("heart attack", "heart failure", "pneumonia")
        if(!(outcome %in% outcome_vect)){
                message("Error in best("+state+", "+outcome+") : "
                        +"invalid outcome")
                stop()
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        data <- subset(outcome_data, outcome_data$State == state)
        if(outcome=="heart attack"){
                outcome <- 
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        }
        if(outcome=="heart failure"){
                outcome <-
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        }
        if(outcome=="pneumonia"){
                outcome <-
                        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        
        data[[outcome]] <- as.numeric(data[[outcome]])
        data <- subset(data,!is.na(data[[outcome]]))
        data <- data[order(data[["Hospital.Name"]]),]
        data <- data[order(data[[outcome]]),]
        
        if(num=="best") num <- 1
        if(num=="worst") num <- nrow(data)
        data[num,][["Hospital.Name"]]
}