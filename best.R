best <- function(state, outcome){
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
        
        ## Return hospital in that state with lowest 30-day death rate
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
        min_index <- which.min(as.numeric(data[[outcome]]))
        
        as.character(data[["Hospital.Name"]][min_index])
}