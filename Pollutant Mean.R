#Part 1: Pollutant Mean
pollutantmean <- function(directory, pollutant, id = 1:332) {
#Providing location of file
  files_full <- list.files(path = "C:/Users/arjun/Downloads/specdata", full.names = TRUE) 
#Creating empty data frame
  dat <- data.frame()
#Running i in id and combining rows
  for (i in id) {
    dat <- rbind(dat, read.csv(files_full[i]))
  }
#Calculating Mean  
  mean(dat[, pollutant], na.rm = TRUE)
}

#Part 2: Reads a directory full of files and reports the number of completely observed cases in each data file

complete<- function(directory, id = 1:332){
#Providing location of file
  files_full <- list.files(path = "C:/Users/arjun/Downloads/specdata", full.names = TRUE)
#Creating empty data frame
  dat <- data.frame()
#Return Data Frame 
  for (i in id) {
    moni_i <- read.csv(files_full[i])
    nobs <- sum(complete.cases(moni_i))
    tmp <- data.frame(i, nobs)
    dat <- rbind(dat, tmp)
  }
  
  colnames(dat) <- c("id", "nobs")
  dat
}

#Part 3: Takes a directory of data files and a threshold for complete cases and calculates the correlation

corr<- function(directory, threshold = 0){
#Providing location of file
  files_full <- list.files(path = "C:/Users/arjun/Downloads/specdata", full.names = TRUE)
  
  dat <- vector(mode = "numeric", length = 0)
  
  for (i in 1:length(files_full)) {
    moni_i <- read.csv(files_full[i])
    csum <- sum((!is.na(moni_i$sulfate)) & (!is.na(moni_i$nitrate)))
    if (csum > threshold) {
      tmp <- moni_i[which(!is.na(moni_i$sulfate)), ]
      submoni_i <- tmp[which(!is.na(tmp$nitrate)), ]
      dat <- c(dat, cor(submoni_i$sulfate, submoni_i$nitrate))
    }
  }
  
  dat
}