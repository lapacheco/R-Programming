
complete <- function (directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating the location of the csv file
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame in the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    library (stringr)
    
    completes <- data.frame(id = integer (), nobs = integer ())
    
    for (i in id) {
        pth <- paste(sep = "", getwd(),"/",directory,"/",str_pad(i,3,pad = "0"),".csv")
        mntr_file <- read.csv(pth)
        cmplts <- complete.cases(mntr_file)
        cmplts_len <- sum(cmplts)
        
        cmplts_add <- data.frame(id = i, nobs = cmplts_len)
        completes <- rbind (completes, cmplts_add)
    }
    
    return (completes)
    
}
