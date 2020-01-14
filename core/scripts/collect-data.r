library(RSQLite)
library(ncdf)

args <- commandArgs(trailingOnly = TRUE)

file.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning.db'
input.addr <- args[2] #'/home/trn/diploma-thesis/data/soil/init-soil' #+beginnig of file
output.addr <- args[3] #'/home/trn/diploma-thesis/data/CCTM_e1a_Linux2_x86_64ifort.benchmark.ACONC' #+beginnig of file
measurements.addr <- args[4] #'/home/trn/Desktop/diploma-thesis/R/scripts/measurements/artificial-measurements.rda'
func.addr <- args[5] #'/home/trn/diploma-thesis/R/scripts/measurements' #where is clear measurements files script
#number of simulations to read
sim.from <- as.numeric(args[6]) #0
sim.to <- as.numeric(args[7]) #99
#number of days to read
date.from <- as.Date(args[8]) #as.Date('2006-01-01') #first day
days.num <- as.numeric(args[9]) #7 #as.numeric(date.to-date.from,units="days")+1 #number of days
date.to <- as.Date(date.from+(days.num-1)) #as.Date('2006-01-07')
#variable to read
variables.output <- c('BAP')
variable.input <- 'STOR_pop2'
row <- NA
col <- NA
avoid <- c(1,3,5,10,20,30,40,50,60,70,80,90,95) #simulations, which are not stored (used for testing)
multiply.outputs <- 1000

source(paste(func.addr,"/clear-measurements.r",sep=""))

load(measurements.addr) #adds data frame called vystup!!!!

days.seq <- seq(from=date.from,to=date.to,by='days') #vector of days, format %Y-%m-%d
days.seq2 <- format(days.seq, "%Y%j") #rep("",length(days.seq)) #vector of days, format %Y%j

#run first, it sets up row and col variables
traverseAconc <- function(dir, simulation ,variables) {
  if(!is.na(row)) result <- array(0,dim=c(col,row,days.num)) #dimensions of CMAQ outputs are col, row, time
  for(i in 1:length(days.seq2)) {
    file.addr <- paste(dir,'.',days.seq2[i],'_',simulation,'.ncf',sep="") #name of file is created
    print(paste("Reading:",file.addr))
    file <-  open.ncdf(file.addr)
    if(is.na(row)) { #one is enough, load dimensions
      row <<- get.var.ncdf(file, "ROW")
      col <<- get.var.ncdf(file, "COL")
      row <<- row[length(row)]
      col <<- col[length(col)]
      result <- array(0,dim=c(col,row,days.num))
    }
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

traverseSoil <- function(dir, simulation ,variable) {
  file.addr <- paste(dir,'_',simulation,'.nc',sep="") #name of file is created
  print(paste("Reading:",file.addr))
  file <-  open.ncdf(file.addr)
  result <- get.var.ncdf(file,variable)[,,1] #!!! so far, we read only first layer !!!
  close.ncdf(file)
  return(result)
}

#open db
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = file.db)

#prepare queries
insert.output <- "insert into output (measurement,simulation,value) values(:aa,:bb,:cc);"
insert.inputInfo <- "insert into inputInfo (row,col) values(:aa,:bb);"
insert.input <- "insert into input (info,simulation,value) values(:aa,:bb,:cc);"

#load measurements
res <- dbSendQuery(con,"select max,min,id from measurement order by id;")
measurements <- fetch(res,n=-1)
dbClearResult(res)
#!!! output data have to made exactly same method as real measurements
#first go data mean for every measurement (we need original rda file with measurements), then averaging for same cells is done

#read & insert to db outputs, change min/max value (for normalization)
result.orig <- vystup
for(i in sim.from:sim.to) {  
  out <- tryCatch({
    output <- traverseAconc(output.addr,i,variables.output) #obtain one simulation output for whole period
    result <- rep(NA,nrow(vystup))
    for(j in 1:nrow(vystup)) { #for every measurement obtain specific data vector
      result[j] <- (mean(output[vystup$col[j],vystup$row[j],which(vystup$from[j]==days.seq):which(vystup$to[j]==days.seq)])*multiply.outputs)
    }
    result.orig$value <- result
    result <- cleanMeasurements(result.orig)$value
    measurements$max <- pmax(result,measurements$max)
    measurements$min <- pmin(result,measurements$min)
    print(paste("Saving output matrix number",i))
    test <- FALSE
    if(i %in% avoid) {
	print(paste('Simulation number ',i,' used for testing',sep="")) 
	test <- TRUE	 
    }
    dbBeginTransaction(con)
    res <- dbGetPreparedQuery(con,"insert into test (simulation,test) values(:aa,:bb)",data.frame(aa=i, bb=test))
    res <- dbGetPreparedQuery(con,insert.output,data.frame(aa=as.numeric(rownames(measurements)), bb=rep(i,nrow(measurements)), cc=result)) #insert output
    dbCommit(con)
  }, error = function(err) {
    print(paste('Problems with simulations outputs number: ',i,'.It is not saved!',sep=""))
  })
}
#update min max values
dbBeginTransaction(con)
dbGetPreparedQuery(con,'update measurement set max=:aa, min=:bb where id=:cc',data.frame(aa=measurements$max, bb=measurements$min, cc=measurements$id)) 
dbCommit(con)

#POLUTATING INPUTINFO DB if it is not!!
#load inputInfo
res <- dbSendQuery(con,"select count(*) from inputInfo;")
inputInfo.number <- fetch(res)
dbClearResult(res)
if(as.numeric(inputInfo.number)==0) {
  print("Polutating input info SQL table")
  cols <- rep(1:row,col)
  rows <- as.vector(sapply(1:col, function(x) rep(x,row)))
  dbBeginTransaction(con)
  res <- dbGetPreparedQuery(con,insert.inputInfo,data.frame(aa=rows, bb=cols))
  dbCommit(con)
}

#load min,max values
res <- dbSendQuery(con,"select max,min,id from inputInfo order by id;")
input.info <- fetch(res,n=-1)
dbClearResult(res)

dbGetQuery(con, "drop index iinfo") #remove index so insert is faster
#read & insert to db inputs
for(i in sim.from:sim.to) {
  out <- tryCatch({
    output <- as.vector(traverseSoil(input.addr,i,variable.input)) #obtain one simulation output for whole period
    output[which(output<0)] <- 0 #no negative values are allowed
    input.info$max <- pmax(output,input.info$max,na.rm=TRUE)
    input.info$min <- pmin(output,input.info$min,na.rm=TRUE)
    print(paste("Saving input matrix number",i))
    if(i %in% avoid) print(paste('Simulation number ',i,' used for testing',sep="")) #data in test table saved already in previous output traverse
    dbBeginTransaction(con)
    res <- dbGetPreparedQuery(con,insert.input,data.frame(aa=input.info$id, bb=rep(i,row*col), cc=output)) #insert output
    dbCommit(con)
  }, error = function(err) {
    print(paste('Problems with simulations inputs number: ',i,'.It is not saved!',sep=""))
  })
}
#update min max values
dbBeginTransaction(con)
dbGetPreparedQuery(con,'update inputInfo set max=:aa, min=:bb where id=:cc',data.frame(aa=input.info$max, bb=input.info$min, cc=input.info$id))
dbCommit(con)

dbGetQuery(con, "create index iinfo on input(info)") #create index again

dbDisconnect(con)