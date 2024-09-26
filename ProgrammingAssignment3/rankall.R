rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomes <- read.csv ("outcome-of-care-measures.csv", colClasses="character")
    
    ## create an outcome dataframe
    otcmLs <- data.frame(
        condition = c("heart attack","heart failure","pneumonia"),
        clmnN = c(11,17,23)
    )
    
    ## create data frame for results
    sttRslts <- data.frame (
        hospital = character (),
        state = character ()
    )
    hospital_select <- data.frame (
        hospital = character (),
        state = character ()
    )
    
    ## Check that outcome is valid
    if (any(outcome %in% otcmLs$condition) == FALSE) {
        stop("invalid outcome")
    } 
    
    ## Get the correct column number for the selected outcome
    column_index <- otcmLs$clmnN[otcmLs$condition == outcome]
    
    ## Return hospital name in that state with the given rank
    ## 30-date death rate
    States <- unique(outcomes$State)
    
    for (i in States) {
        
        sttOtcms <- outcomes %>%
            filter(State == i) %>%
            mutate_at(vars(column_index), as.numeric) %>% # Convert the selected outcome column to numeric
            filter(!is.na(.[[column_index]])) %>% # Remove rows with missing outcome data
            arrange(.[[column_index]], Hospital.Name) # Sort by the outcome column
    
        sttOtcmsLen <- nrow(sttOtcms)
    
        if (num=="best") {
            rankReturn = 1
        }
        else if (num == "worst") {
            rankReturn = sttOtcmsLen
        }
        else {rankReturn = num}
    
        if (is.numeric(num) & num > sttOtcmsLen) {
            hospital_select <- c ("NA", i)
        }
        else{
            hospital_select <- sttOtcms[rankReturn, c("Hospital.Name","State")]
        }
        
        sttRslts <- rbind (sttRslts, hospital_select)
        sttRslts <- arrange (sttRslts, State)
    }
    
    return (sttRslts)
}

res<-tail (rankall("heart failure"), 10)
