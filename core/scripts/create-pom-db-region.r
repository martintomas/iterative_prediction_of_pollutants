library("RSQLite")
library(Rmpi)

args <- commandArgs(trailingOnly = TRUE)

file.save.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning/pom-data-region.db'
file.db <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db'
type <- args[3] #'dist' #can be now dist or corr (distance or correlation)
file.region <- args[4] #'/home/trn/Desktop/diploma-thesis/software/multi-iteration/init/map-regions.csv'
cores <- as.numeric(args[5]) #3

mpi.spawn.Rslaves(nslaves=cores)

#load region map
regions <- as.matrix(read.csv(file.region))

water <- rep(0,max(regions))
water[1] <- 1 #region 1 is ignored -> water

#set up dimensions
row <- nrow(regions)
col <- ncol(regions)

num <- max(regions) #row*col #num of regions (avoid water)

#prepare measurements
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = file.db)
res <- dbSendQuery(con,"select row,col,id from measurement order by id;")
measurement <- fetch(res,n=-1)
dbClearResult(res)


distVector <- function(input,output) {
  res <- rep(0,nrow(output))
  for(i in 1:nrow(output)) {
    res[i] <- min(pmax(abs(input[,2]-measurement$row[i]),abs(input[,1]-measurement$col[i]))) #keep in mind that cmaq outputs are switched, first is col, second is row
  }
  return(res)
}

if(type=='dist') {
  mpi.bcast.Robj2slave(distVector)
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
      result[i,] <- rep(1,length(measurement$id)) #water is avoided
    } else {
      if(type=='dist') {
	result[i,] <- (distVector(which(regions==group[i],arr.ind=TRUE),measurement))
      } else if(type=='corr') {
	len.region <- which(regions==group[i])
	input <- apply(matrix(dbGetPreparedQuery(con,get.input,data.frame(aa=len.region))$value,ncol=length(len.region)),1,mean)
	result[i,] <- abs(cor(input,output)) #I don't care if positive or negative, just how big
      } else if(type=='all') {
	result[i,] <- rep(1,length(measurement$id)) #everything is equivalent
      }
    }
  }
  return(result)
}

slaves <- (mpi.comm.size()-1) #except of master
groups <- split(1:(num), ceiling(1:(num)/((num)/slaves))) #!!!!avoid water, region number 1

mpi.bcast.Robj2slave(regions)
mpi.bcast.Robj2slave(type)
mpi.bcast.Robj2slave(measurement)
mpi.bcast.Robj2slave(resultNode)
mpi.bcast.Robj2slave(groups)
mpi.bcast.Robj2slave(water)
mpi.bcast.cmd(group <- groups[[mpi.comm.rank()]])
res <- mpi.remote.exec(resultNode())
output <- matrix(0, nrow=(num), ncol=length(measurement$id)) #!!! avoid water, num-1
for(i in 1:length(res)) { #put it all together
  output[(groups[[i]]),] <- res[[i]]  #sum all differences #!!! avoid water, num-1
}
dbDisconnect(con) #close db
mpi.close.Rslaves()

if(type=='corr') {
  output[which(is.na(output))] <- 0 #if corr is NA, there is no correlation
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