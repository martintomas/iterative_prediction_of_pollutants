library("RSQLite")
library(Rmpi)

args <- commandArgs(trailingOnly = TRUE)

file.save.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning/pom-data.db'
file.db <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db'
file.analysis.addr <- args[3] #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.csv'
type <- args[4] #'dist' #can be now dist or corr (distance or correlation)
cores <- as.numeric(args[5]) #3
method <- args[6] #'back'

if(method=='back') {
  prev.method <- args[7] #'region'
  prev.type <- args[8] #'dist'
  if(prev.method=='cell' & prev.type==type) { #previous method to construct pom data was cell and type is same => we can use data
    drv <- dbDriver("SQLite")
    con <- dbConnect(drv, dbname = file.save.db)
    res <- dbSendQuery(con,"select * from data;")
    prev.data <- as.matrix(fetch(res,n=-1))
    dbClearResult(res)    
    prev.data <- t(prev.data[,2:ncol(prev.data)]) #remove row_names and do transposition (so we can use it in reverse prediction)
    
    #create new pom-data db
    dbSendQuery(con, "drop table if exists data;") #remove old data, if there are
    data <- as.data.frame(cbind(1:nrow(prev.data),prev.data))
    names(prev.data) <- c("row_names",1:ncol(prev.data))

    dbSendQuery(con,paste('create table data (row_names integer,', paste('V',colnames(data[,2:ncol(data)]),' REAL',collapse=",", sep=""),');'))
    dbBeginTransaction(con)
    for(i in 1:nrow(data)) {
      dbSendQuery(con,statement=paste("INSERT INTO data VALUES (", paste(unlist(data[i,]),collapse=","),");"))
    }
    dbCommit(con)
    dbGetQuery(con, "create unique index idata on data(row_names)")
    dbDisconnect(con)
    quit('no') #end script
  }
}

mpi.spawn.Rslaves(nslaves=cores)

# In case R exits unexpectedly, have it automatically clean up # resources taken up by Rmpi (slaves, memory, etc...) 
.Last <- function() { 
  if (is.loaded("mpi_initialize")){ 
    if (mpi.comm.size(1) > 0){ 
      print("Please use mpi.close.Rslaves() to close slaves.") 
      mpi.close.Rslaves() 
    } 
    print("Please use mpi.quit() to quit R") 
    .Call("mpi_finalize") 
  } 
}

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = file.db)

res <- dbSendQuery(con,"select max(row),max(col) from inputInfo;")
info <- fetch(res,n=-1)
dbClearResult(res)

#set up dimensions
row <- info[1,1]
col <- info[1,2]

num <- row*col #num of lines
water <- read.csv(file.analysis.addr)$water

#prepare measurements
res <- dbSendQuery(con,"select row,col,id from measurement order by id;")
measurement <- fetch(res,n=-1)
dbClearResult(res)

distVector <- function(input,output) {
  return(pmax(abs(input$row-output$row),abs(input$col-output$col)))
}

if(type=='dist') {
  res <- dbSendQuery(con,"select row,col,id from inputInfo order by id;")
  input <- fetch(res,n=-1)
  dbClearResult(res)

  mpi.bcast.Robj2slave(distVector)
  mpi.bcast.Robj2slave(input)
} else if(type=='corr') {
  res <- dbSendQuery(con,"select count(simulation) from test where test.test=0;")
  num.sim <- fetch(res,n=-1)[1,1]
  dbClearResult(res)

  get.output <- "select output.value from output inner join test on output.simulation=test.simulation where output.measurement=:aa and test.test=0 order by output.simulation;"
  get.input <- "select input.value from input inner join test on input.simulation=test.simulation where input.info=:aa and test.test=0 order by input.simulation;"

  output <- matrix(NA,nrow=num.sim,ncol=length(measurement$id)) #rows are simulations, col are variables (measurements)
  for(i in 1:length(measurement$id)) {
    #simulations are marked from 0 to num.sim-1
    output[,i] <- as.vector(dbGetPreparedQuery(con,get.output,data.frame(aa=(i)))$value)
  }
  
  mpi.bcast.Robj2slave(output)
  mpi.bcast.Robj2slave(get.input)
  mpi.bcast.Robj2slave(file.db)
}

#dist is fast, corr is slow
resultNode <- function() {
  if(type=='corr') {
    library(RSQLite)
    drv <- dbDriver("SQLite")
    con <- dbConnect(drv, dbname = file.db)
  }
  result <- matrix(NA,nrow=length(group),ncol=length(measurement$id))
  for(i in 1:length(group)) {
    if(water[group[i]]==1) {
      result[i,] <- rep(1,length(measurement$id))
    } else {
      if(type=='dist') {
	result[i,] <- (distVector(input[group[i],],measurement))
      } else if(type=='corr') {
	input <- as.vector(dbGetPreparedQuery(con,get.input,data.frame(aa=group[i]))$value)
	result[i,] <- abs(cor(input,output)) #I don't care if positive or negative, just how big
      } else if(type=='all') {
	result[i,] <- rep(1,length(measurement$id)) #everything is equivalent
      }
    }
  }
  return(result)
}

slaves <- (mpi.comm.size()-1) #except of master
groups <- split(1:(num), ceiling(1:(num)/((num)/slaves)))


mpi.bcast.Robj2slave(type)
mpi.bcast.Robj2slave(measurement)
mpi.bcast.Robj2slave(resultNode)
mpi.bcast.Robj2slave(groups)
mpi.bcast.Robj2slave(water)
mpi.bcast.cmd(group <- groups[[mpi.comm.rank()]])
res <- mpi.remote.exec(resultNode())
output <- matrix(0, nrow=num, ncol=length(measurement$id))
for(i in 1:length(res)) { #put it all together
  output[groups[[i]],] <- res[[i]]  #sum all differences
}
dbDisconnect(con) #close db
mpi.close.Rslaves()

if(type=='corr') {
  output[which(is.na(output))] <- 0 #if corr is NA, there is no correlation
}

if(method=='back') {
  output <- t(output) #we are using reverse prediction, do transposition
}

#create new pom-data db
con <- dbConnect(drv, dbname = file.save.db)
dbSendQuery(con, "drop table if exists data;") #remove old data, if there are

data <- as.data.frame(cbind(1:nrow(output),output))
names(data) <- c("row_names",1:ncol(output))

#dbWriteTable(con, "data", data, row.names=FALSE) #rows are output
#dbSendQuery(con,dbBuildTableDefinition(con,'data',output,row_names=FALSE))

dbSendQuery(con,paste('create table data (row_names integer,', paste('V',colnames(data[,2:ncol(data)]),' REAL',collapse=",", sep=""),');'))
dbBeginTransaction(con)
for(i in 1:nrow(data)) {
  dbSendQuery(con,statement=paste("INSERT INTO data VALUES (", paste(unlist(data[i,]),collapse=","),");"))
}
dbCommit(con)

dbGetQuery(con, "create unique index idata on data(row_names)")

dbDisconnect(con)

#mpi.quit()