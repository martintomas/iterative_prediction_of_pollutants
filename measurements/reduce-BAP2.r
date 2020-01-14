library(ncdf)

args <- commandArgs(trailingOnly = TRUE)

func.addr <- '/storage/brno2/home/martintomas/data-preparation' #where is create-ncdf files script
save.file <-  '/storage/brno2/home/martintomas/data-preparation/BAP-mean2.nc' #where to save file + its name

row <- 121
col <- 121
date.from <- as.Date('2006-01-01')
date.to <- as.Date('2006-01-07')
simulation <- '' #if you don't want to use numbered output, set up to '', otherwise, use number of output

source(paste(func.addr,"/create-ncdf.r",sep=""))

days.num <- as.numeric(date.to-date.from,units="days")+1 #number of days
days.seq <- seq(from=date.from,to=date.to,by='days') #vector of days, format %Y-%m-%d
days.seq2 <- format(days.seq, "%Y%j") #vector of days, format %Y%j

#will open separate ncdf files
#dir -- where are files stored
#aconc.name -- first parts of files name
#variables -- variables loaded from file (they are summed together)
traverseAconc <- function(dir, aconc.name, variables) {
  result <- array(0,dim=c(col,row,days.num))
  for(i in 1:length(days.seq2)) {
    if(simulation=='') {
      file.addr <- paste(dir,'/',aconc.name,'.',days.seq2[i],'.ncf',sep="") #name of file is created
    } else {
      file.addr <- paste(dir,'/',aconc.name,'.',days.seq2[i],'_',simulation,'.ncf',sep="") #name of file is created
    }
    print(paste("Reading:",file.addr))
    file <-  open.ncdf(file.addr)
    temp.result <- matrix(0,nrow=col,ncol=row)
    for(j in variables) { #each variable is read
      temp <- get.var.ncdf(file, j)
      temp.variable <- apply(temp, 2, function(data) { #variable is averaged over hours
	apply(data,1,mean)
      })
      temp.result <- temp.result+temp.variable #variables are summed together !!! (ABAPI, ABAPJ, ABAPK)
    }
    close.ncdf(file)
    result[,,i] <- temp.result #save temp result for one day to summary matrix
  }
  return(result)
}

#if variables are in separate files
# summed <- array(0,dim=c(row,col,days.num))
# file.bap.addr <- '/storage/home/janam/test/aver/JPonly_output/ABAPI'
# aconc.name <- 'ABAPI_CCTM_e1a_Linux2_x86_64ifort.benchmark.ACONC' #date and .ncf will be added later
# summed <- summed + traverseAconc(file.bap.addr,aconc.name,c('ABAPI'))
# 
# file.bap.addr <- '/storage/home/janam/test/aver/JPonly_output/ABAPJ'
# aconc.name <- 'ABAPJ_CCTM_e1a_Linux2_x86_64ifort.benchmark.ACONC' #date and .ncf will be added later
# summed <- summed + traverseAconc(file.bap.addr,aconc.name,c('ABAPJ'))
# 
# file.bap.addr <- '/storage/home/janam/test/aver/JPonly_output/ABAPK'
# aconc.name <- 'ABAPK_CCTM_e1a_Linux2_x86_64ifort.benchmark.ACONC' #date and .ncf will be added later
# summed <- summed + traverseAconc(file.bap.addr,aconc.name,c('ABAPK'))

#if all variables are in one ncdf file, you can use this:
file.bap.addr <- '/storage/home/janam/test/aver/output'
aconc.name <- 'abap_jikCCTM_e1a_Linux2_x86_64ifort.benchmark.ACONC' #date and .ncf will be added later
summed <- traverseAconc(file.bap.addr,aconc.name,c('ABAPI','ABAPJ','ABAPK'))

variables <- matrix(NA, nrow=1,ncol=2)
variables[1,] <- c('BAP', "units (mean for days)")
file.new <- createNcdf(save.file, col, row, 1, days.num, variables)
put.var.ncdf(file.new,'BAP',summed)
close.ncdf(file.new)