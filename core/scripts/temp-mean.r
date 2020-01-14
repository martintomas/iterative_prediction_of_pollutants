library(ncdf)

args <- commandArgs(trailingOnly = TRUE)

met.start.addr <- args[1] #'/home/trn/Desktop/CMAQ/cmaq471_bap/data/mcip3' #it is dictionary
file.save.addr <- args[2] #paste(met.start.addr,'/temp-mean',sep="")
func.addr <- args[3] #"/home/trn/diploma-thesis/R/scripts/volatilization-function"
met.name <- args[4] #'METCRO2D_aa2006test'

met.variable <- 'TEMPG'
#row <- 121
#col <- 121
#lev <- 1
time <- 1

source(paste(func.addr,"/create-ncdf.r",sep=""))

traverseDir <- function(dir) {
  print(paste("Checking dir:",dir))  
  met.addr <- list.files(dir, include.dirs = FALSE, full.names=TRUE)
  for(i in met.addr) {
    if(file.info(i)$isdir) {
      traverseDir(i)
    } else {
      name <- strsplit(i,"/")
      name <- name[[1]][length(name[[1]])]
      if(met.name==name) {
	print(paste("Reading:",i))
	file <-  open.ncdf(i)
	temp <- get.var.ncdf(file, met.variable)
	temp.mean <- apply(temp, 2, function(data) {
	  apply(data,1,mean)
	})
	if(temp.result.first) {
	  temp.result.first <<- FALSE
	  temp.result <<- temp.mean
	} else {
	  for(i in 1:nrow(temp.mean)) {
	    for(j in 1:ncol(temp.mean)) {
	      temp.result[i,j] <<- (temp.result[i,j]+temp.mean[i,j])/2
	    }
	  }
	}
	close.ncdf(file)
      }
    }
  }
}

temp.result.first <- TRUE
traverseDir(met.start.addr)
#image(temp.result)

variables <- matrix(NA, nrow=1,ncol=2)
variables[1,] <- c('temperature', "average temp")

file.new <- createNcdf(file.addr=file.save.addr, time=time, variables=variables)
put.var.ncdf(file.new,'temperature',temp.result)
close.ncdf(file.new)
