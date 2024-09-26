
#load dplyr
library("dplyr")

## best function returns the name of a hospital with the lowest
## outcome based on the state and measure arguments
best <- function(state, outcome) {
    
    ## read in outcome of care measures
    outcomes <- read.csv ("outcome-of-care-measures.csv", colClasses="character")

    ## create an outcome dataframe
    otcmLs <- data.frame(
        condition = c("heart attack","heart failure","pneumonia"),
        clmnN = c(11,17,23)
    )
    
    ## Check that state and outcome are valid
    if (any(state %in% outcomes$State) == FALSE) {
        stop("invalid state")
    } 
    else if (any(outcome %in% otcmLs$condition) == FALSE) {
        stop("invalid outcome")
    } 
    
    ## Get the correct column number for the selected outcome
    column_index <- otcmLs$clmnN[otcmLs$condition == outcome]
    
    ## Return hospital name in that state with lowest 30-day death
    ## first 
    sttOtcms <- outcomes %>%
        filter(State == state) %>%
        mutate_at(vars(column_index), as.numeric) %>%   # Convert the selected outcome column to numeric
        filter(!is.na(.[[column_index]])) %>%           # Remove rows with missing outcome data
        arrange(.[[column_index]])                      # Sort by the outcome column
    
    ## different way of doing it
    ## Filter the data for the selected state
    ## sttOtcms <- outcomes[outcomes$State == state, ]
    
    ## Convert the selected outcome column to numeric to allow for comparison
    ## sttOtcms[, column_index] <- suppressWarnings(as.numeric(sttOtcms[, column_index]))
    
    ## Remove rows with missing data in the selected outcome column
    ## sttOtcms <- sttOtcms[!is.na(sttOtcms[, column_index]), ]
    
    ## Find the hospital with the lowest 30-day death rate (smallest value in the outcome column)
    ## best_hospital <- sttOtcms[which.min(sttOtcms[, column_index]), "Hospital.Name"]
    
    ## Rate    
    ## Return the hospital with the lowest 30-day death rate
    best_hospital <- sttOtcms$Hospital.Name[1]
    
    return(best_hospital)
}

best("CA","heart attack")
