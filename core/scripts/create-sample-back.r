library(RSQLite)
library(Rmpi)

args <- commandArgs(trailingOnly = TRUE)

file.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db'
file.pom.db <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning/pom-data.db'
save.dir <- args[3] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning'
map.save.dir <- paste(save.dir,'/map.csv',sep="")
sample.save.dir <- paste(save.dir,'/',args[4],sep="") #sample
file.analysis.addr <- args[5] #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.csv' #to obtain water file
func.addr <- args[6] #'/home/trn/diploma-thesis/software/multi-iteration/scripts'

type <- args[7] #'all'
min.data <- as.numeric(args[8]) #0
max.data <- as.numeric(args[9]) #90
cores <- as.numeric(args[10]) #3

data.water <- read.csv(file.analysis.addr)$water

source(paste(func.addr,"/samples-functions.r",sep=""))

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

#prepare db
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = file.db)

#load measurements
res <- dbSendQuery(con,'select id from measurement order by id')
measurements <- as.vector(fetch(res,n=-1)$id)
dbClearResult(res)
#load data for normalization
res <- dbSendQuery(con,'select min,max from measurement')
output.norm <- t(as.matrix(fetch(res,n=-1)))
dbClearResult(res)
res <- dbSendQuery(con,'select min,max from inputInfo')
input.norm <- t(as.matrix(fetch(res,n=-1))) #norm values for input
dbClearResult(res)
res <- dbSendQuery(con,'select simulation from test where test=0')
simulations <- as.vector(fetch(res,n=-1)$simulation) #simulations used to construct learning samples
dbClearResult(res)

dbDisconnect(con)

#prepare map, it is easy
write.csv(measurements,map.save.dir,row.names=FALSE)

samplesId <- function() {
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = file.pom.db)
  res <- list()
  for(i in 1:length(group)) {
    data.sample <- as.matrix(dbGetPreparedQuery(con,'select * from data where row_names=:aa',data.frame(aa=(group[i]))))
    data.sample <- as.vector(data.sample[,2:ncol(data.sample)])
    min.data.t <- min.data #temporal variables, so original values are not changed
    max.data.t <- max.data
    if(type=='all') {
      res[[i]] <- which(data.water[1:length(data.sample)]!=1)   
    } else {
      if(type=='dist') {
        data.sample.min <- min(data.sample[which(data.water[1:length(data.sample)]!=1)])
        if(data.sample.min>max.data.t) { #use at least one sample
          print('Changing max value for data selection')
	  max.data.t <- data.sample.min #we change max distance from stations
	} 
      } else if(type=='corr') {
        data.sample.min <- max(data.sample[which(data.water[1:length(data.sample)]!=1)])
        if(data.sample.min<min.data.t) {
          print('Changing max value for data selection')
	  min.data.t <- data.sample.min #we change max distance from stations
	}
      }
      res[[i]] <- which(data.sample>=min.data.t & data.sample<=max.data.t & data.water[1:length(data.sample)]!=1)   
    }
  }
  dbDisconnect(con)
  return(res)
}

slaves <- (mpi.comm.size()-1) #except of master
groups <- split(1:length(measurements), ceiling(1:length(measurements)/(length(measurements)/slaves)))

mpi.bcast.cmd(library(RSQLite))
mpi.bcast.Robj2slave(samplesId)
mpi.bcast.Robj2slave(file.pom.db)
mpi.bcast.Robj2slave(max.data)
mpi.bcast.Robj2slave(min.data)
mpi.bcast.Robj2slave(data.water)
mpi.bcast.Robj2slave(groups)
mpi.bcast.Robj2slave(type)
mpi.bcast.cmd(group <- groups[[mpi.comm.rank()]])
output <- mpi.remote.exec(samplesId()) #get id of used inputs
#output <- samplesId(groups[[4]])
#print(type)
data.id <- list()
for(i in 1:length(output)) {
  data.id[groups[[i]]] <- output[[i]]
}

input.get <- 'select input.value from input where input.simulation=:aa order by input.info;'
output.get <- 'select output.value from output inner join test on output.simulation=test.simulation where output.measurement=:aa and test.test=0 order by output.simulation;'

createSample <- function() { #samples) {
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = file.db)
  output <- matrix(dbGetPreparedQuery(con,output.get,data.frame(aa=(samples)))$value,ncol=length(samples))
  output <- normalize(output,output.norm[,samples])
  for(i in 1:length(simulations)) {
    print(paste('Adding simulation',simulations[i],'for learning'))
    input <- as.vector(dbGetPreparedQuery(con,input.get,data.frame(aa=(simulations[i])))$value)
    input <- normalize(input,input.norm)
    for(j in 1:length(samples)) {
      if(i==1) {
	#save info
	sink(paste(sample.save.dir,'-info_',samples[j],sep=""),type="output")
	cat(data.id[[samples[j]]])
	sink(NULL)
	#remove old file if exists
	out <- tryCatch({
	  file.remove(paste(sample.save.dir,'_',samples[j],'.data',sep="")) #remove old file
	}, error = function(err) {
	  #file doesn't exists
	})
	#save begging fo data file
	sink(paste(sample.save.dir,'_',samples[j],'.data',sep=""),append=TRUE,type="output")
	cat(paste(length(simulations),length(data.id[[samples[j]]]),'1'))
	cat('\n')
	sink(NULL)
      }
      sink(paste(sample.save.dir,'_',samples[j],'.data',sep=""),append=TRUE,type="output")
      cat(input[data.id[[samples[j]]]])
      cat('\n')
      cat(output[i,j])
      cat('\n')
      sink(NULL)
    }
  }
}

groups <- split(measurements, ceiling(1:length(measurements)/(length(measurements)/slaves)))
mpi.bcast.Robj2slave(groups)
mpi.bcast.Robj2slave(normalize)
mpi.bcast.Robj2slave(norm.func)
mpi.bcast.Robj2slave(file.db)
mpi.bcast.Robj2slave(input.get)
mpi.bcast.Robj2slave(output.get)
mpi.bcast.Robj2slave(input.norm)
mpi.bcast.Robj2slave(output.norm)
mpi.bcast.Robj2slave(simulations)
mpi.bcast.Robj2slave(data.id)
mpi.bcast.Robj2slave(createSample)
mpi.bcast.Robj2slave(sample.save.dir)
mpi.bcast.cmd(samples <- groups[[mpi.comm.rank()]])
mpi.remote.exec(createSample()) #create sample files

#createSample(measurements) #create samples
#print(data.id)
mpi.close.Rslaves()
mpi.quit()
