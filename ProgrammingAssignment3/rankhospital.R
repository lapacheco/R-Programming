rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
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
    
    ## Return hospital name in that state with the given rank
    ## 30-date death rate
    sttOtcms <- outcomes %>%
        filter(State == state) %>%
        mutate_at(vars(column_index), as.numeric) %>%   # Convert the selected outcome column to numeric
        filter(!is.na(.[[column_index]])) %>%           # Remove rows with missing outcome data
        arrange(.[[column_index]], Hospital.Name)                      # Sort by the outcome column
    
    sttOtcmsLen <- nrow(sttOtcms)
    
    if (num=="best") {
        rankReturn = 1
    }
    else if (num == "worst") {
        rankReturn = sttOtcmsLen
    }
    else {rankReturn = num}
   
    if (is.numeric(num) & num > sttOtcmsLen) {
        hospital_select <- "NA"
    }
    else{
        hospital_select <- sttOtcms$Hospital.Name[rankReturn]   
    }
    
    return (hospital_select)
}

res<-rankhospital("TX","heart failure", 4)
