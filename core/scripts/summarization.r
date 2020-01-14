library(RSQLite)

args <- commandArgs(trailingOnly = TRUE)

file.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db'
file.save <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/post-process'
func.addr <- args[3] #'/home/trn/diploma-thesis/software/multi-iteration/scripts'
file.info <- paste(file.save,'/info.csv',sep="")
file.best <- paste(file.save,'/best.csv',sep="")
iteration <- as.numeric(args[4]) #1

source(paste(func.addr,"/post-learning-functions.r",sep=""))

#if(iteration==0) {
#  print('Creating info file')
#  sink(file.info,append=TRUE,type="output")
#  cat('\"Iteration 0:\" \"info file created\"')
#  sink(NULL)
#  quit('no')
#}

appendInfo <- function(file.info,text, iteration) {
  sink(file.info,append=TRUE,type="output")
  cat('\n')
  cat(paste('\"Iteration ',iteration,':\" \"',paste(text,collapse=" ",sep=""),'\"',sep=""))
  sink(NULL)
}
saveBest <- function(file.best,text,index) {
  sink(file.best,type="output")
  cat(text)
  cat('\n')
  cat(paste('File number:',index))
  sink(NULL)
}

#load measurements
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = file.db)
res <- dbSendQuery(con,'select value from measurement order by id')
measurements <- as.vector(fetch(res,n=-1)$value)
dbClearResult(res)

#load outputs
res <- dbSendQuery(con,'select output.value from output inner join test on test.simulation=output.simulation where test.test=0 order by output.measurement')
outputs <- matrix(fetch(res,n=-1)$value,nrow=length(measurements),byrow=TRUE)
dbClearResult(res)

res <- compare(outputs,measurements)
appendInfo(file.info,res,iteration)

saveBest(file.best,min(res),which.min(res))

dbDisconnect(con)

