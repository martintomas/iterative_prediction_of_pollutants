library(ncdf)

args <- commandArgs(trailingOnly = TRUE)

file.bap.addr <- args[1] #'/home/trn/Desktop/allJPonly_BAP.nc'
func.addr <- args[2] #"/home/trn/diploma-thesis/R/scripts/volatilization-function"
save.dir <-  args[3] #'/home/trn/Desktop/diploma-thesis/R/scripts/measurements'

#MEAN OUTPUTS FROM CMAQ
file.bap <- open.ncdf(file.bap.addr)
bap.cmaq <- get.var.ncdf(file.bap, 'BAP')
date.from <- '2006-01-01'
date.to <- '2006-12-31'
days.num <- as.numeric((as.Date(date.to)-as.Date(date.from)),units="days")+1
reduced.hours <- 24
col<-121
row<-121
source(paste(func.addr,"/create-ncdf.r",sep=""))

#bap.cmaq <- array(sample(seq(0.001,2,0.05),size=(row*col*days.num*reduced.hours),replace=TRUE),dim=c(row,col,days.num*reduced.hours)) #for testing
cmaq.days <- seq(from=0,to=(days.num*reduced.hours),by=reduced.hours)
result <- array(0, dim=c(row,col,days.num))
for(i in 2:length(cmaq.days)) {
  cmaq.temp  <- apply(bap.cmaq[,,(cmaq.days[i-1]+1):cmaq.days[i]], 2, function(data) {
    apply(data,1,mean)
  })
  result[,,i-1] <- cmaq.temp
}

variables <- matrix(NA, nrow=1,ncol=2)
variables[1,] <- c('BAP', "units (mean for days)")
file.new <- createNcdf(paste(save.dir,'/BAP-mean.nc',sep=""), col, row, 1, days.num, variables)
put.var.ncdf(file.new,'BAP',result)
close.ncdf(file.new)