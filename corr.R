
corr <- function (directory, threshold = 0) {
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    ## 'threshold' is a numeric vector of length 1 indicating the number of completely observed observations (on all
    ## variables )required to compute the correlation between nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    library (stringr)
    
    ids <- 1:332
    pollutants <- c("sulfate","nitrate")
    
    allCrs <- c()
    
    for (i in ids) {
        
        pth <- paste(sep = "", getwd(),"/",directory,"/",str_pad(i,3,pad = "0"),".csv")
        mntr_file <- read.csv(pth)
        cmplts <- complete.cases(mntr_file)
        cmplts_len <- sum(cmplts)
        
        if (cmplts_len > threshold) {
            mntr_slft <- mntr_file["sulfate"]
            mntr_ntrt <- mntr_file["nitrate"]
            
            mntrFlCr <- cor (mntr_slft,mntr_ntrt, use = "complete.obs")
            allCrs <- append(allCrs,mntrFlCr)
        }
        
    }
    
    return (allCrs)
    
}
