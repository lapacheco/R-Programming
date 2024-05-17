pollutantmean <- function (directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    library (stringr)
    
    id_len = length(id)
    
    all_mntr = c()
    
    for (i in id) {
        
         pth <- paste(sep = "", getwd(),"/",directory,"/",str_pad(i,3,pad = "0"),".csv")
         mntr_file <- read.csv(pth)
         mntr_plltnt <- mntr_file[pollutant]
         mntr_plltnt_cln <- mntr_plltnt[!is.na(mntr_plltnt)]
    
         all_mntr <- append(all_mntr,mntr_plltnt_cln)
    }
    
     mntr_sum <- sum (all_mntr,na.rm=TRUE)
     mntr_cnt <- length(all_mntr)
    all_mntr_mean <- mntr_sum/mntr_cnt
    
    return (all_mntr_mean)
    
}

pollutantmean ("specdata","nitrate",23)
