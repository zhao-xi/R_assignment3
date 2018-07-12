rankall <- function(outcome, num = "best"){
        ## The data frame that is going to be returned
        result <- data.frame(matrix(nrow = 0, ncol = 2))
        colnames(result) <- c("hospital","state")
        
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv",
                                 colClasses = "character")
        
        ## Check that outcome is valid
        outcome_vect <- c("heart attack", "heart failure", "pneumonia")
        state_vect <- as.character(unique(outcome_data[["State"]]))
        if(!(outcome %in% outcome_vect)){
                message("Error in best("+state+", "+outcome+") : "
                        +"invalid outcome")
                stop()
        }
        
        ## Return the data frame with the hospital names and the (abbreviated)
        ## state name
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
        
        data_list <- split(outcome_data, outcome_data$State)
        data_list <- lapply(data_list,function(x) x<-x[order(as.character(x[["Hospital.Name"]])),] )
        data_list <- lapply(data_list,function(y) y<-y[order(as.numeric(y[[outcome]])),])
     
        for(x in state_vect){
                if(num == "best") num <- 1
                if(num == "worst") num <- nrow(data_list[[x]])
                
                hospital <- as.character(data_list[[x]]$Hospital.Name[num])
                state <- as.character(data_list[[x]]$State[1])
                new_row <- data.frame(hospital, state)
                colnames(new_row) <- c("hospital","state")
                result <- rbind(result, new_row)
        }
        result <- result[order(as.character(result$state)),]
        result
}