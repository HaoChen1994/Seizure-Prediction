#not run
library(edf)
library(DensParcorr)
setwd("C:/Users/Desktop")
file <- list.files(pattern = ".edf")
cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)
for (k in 1:1) {
  EDF <- read.edf(file[[k]], read.annotations = F)
  
  # 1. The header of an EDF file 
  EDF$header.global
  
  # sampling rate: 500 times per second
  SR <- EDF$header.signal$EEG_Fp1_Ref$samplingrate
  
  # 2. the ordinary signals from an EDF file 
  signals <- EDF$signal
  # It is a list. we then transform it as a data.frame
  m <- 19
  row_num <- 3600*500
  dat <- data.frame(matrix(nrow = row_num, ncol = m))
  names(dat)[1:17] <- names(signals)[1:17]
  names(dat)[18] <- names(signals)[23]
  names(dat)[19] <- names(signals)[24]
  for(i in 1:19){
    dat[, i] <- signals[[i]][['data']][(1800*500+1):(5400*500)]
  }
  EEG_data_seizures_old <- as.matrix(dat,nrow = row_num, ncol = m)
  
  nr <- row_num/SR
  EEG_data_seizures <- matrix(nrow = nr, ncol=m)
  for (i in 1:nr) {
    EEG_data_seizures[i,] <- colMeans(EEG_data_seizures_old[(SR*(i-1)+1):(SR*i),])
  }
  X1.clime <- DensParcorr(EEG_data_seizures,dens.level =.5,select=TRUE)
  X1_omega <- X1.clime$selected.precision
}