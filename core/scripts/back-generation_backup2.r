#check whetever enough initial files were generated in previous iteration with learning algorithm
#if not, it use random functions to generate missing ones
#save all inputs to db, so back algorithm can use them

args <- commandArgs(trailingOnly = TRUE)

input.addr <- args[1] #'/home/trn/scratch/save'
input.soil <- paste(input.addr,'/init-soil',sep="")
data.addr <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.csv'
func.addr <- args[3] #"/home/trn/diploma-thesis/R/scripts/init-soil"
save.db <- args[4] #'/home/trn/Desktop/diploma-thesis/R/scripts/post-process/pom-data-back.db'
file.db <- args[5] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db'

from <- as.numeric(args[6]) #100
to <- as.numeric(args[7]) #110
cores <- as.numeric(args[8]) #3
testing <- as.numeric(args[9]) #1
useKriging <- as.numeric(args[10]) #0 #1 == yes, 0 == no
variable <- "STOR_pop2"
min.samples <- 14000
max.samples <- 14200
do.visualization <- TRUE #visualization doesn't work with limiter
limiter <- '' #100 #for testing purpose #limits amount of data saved in db, change to '' (empty string), to remove its effect


files.names <- paste(input.soil,'_',from:to,'.nc',sep="")

file.exist <- file.exists(files.names)

if(any(file.exist==FALSE)) {
  print("Some of the input files for next iteration are missing!!! Generating missing ones with random functions.")
} else {
  print("Simulation part have enough soil input files. Only saving values from previos simulations")
  #quit('no')
}

if(all((file.exist)==FALSE)) {
  print('All ncdf files are missing. Impossible to reconstruct some files!!')
  quit('no')
}

library(Rmpi)
library(RSQLite)
library(ncdf)

num.missing.files <- length(files.names[!file.exist])
if(num.missing.files>0) {
  if(num.missing.files<cores) {
    mpi.spawn.Rslaves(nslaves=num.missing.files)
  } else {
    mpi.spawn.Rslaves(nslaves=cores)
  }
}

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

slaves <- (mpi.comm.size()-1) #except of master

#prepare db
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = file.db)

#load dimensions
res <- dbSendQuery(con,'select max(row),max(col) from inputInfo')
dim <- as.vector(fetch(res,n=-1))
row <- dim[1,1]
col <- dim[1,2]
dbClearResult(res)

#load testing
if(testing==1) {
  res <- dbSendQuery(con,'select simulation from test where test=1 order by simulation')
  tests <- as.vector(fetch(res,n=-1)$simulation)
  input.test <- paste(input.addr,'/init-test-',tests,'.csv',sep="")
  dbClearResult(res)
}

dbDisconnect(con)


#we will use db, where new inputs and generated ones are stored
con <- dbConnect(drv, dbname = save.db)
dbSendQuery(con, "drop table if exists input;")
#!!!! simulation number -1 is for original measurements, rest is for tests
#dbSendQuery(con, "create table input (id integer not null, simulation integer not null, sample integer not null, value double not null,CONSTRAINT id_pk PRIMARY KEY (id asc));")
#dbGetQuery(con, "create index isample on input(sample)")
if(limiter!='') {
  dbSendQuery(con,paste('create table input (simulation integer not null, sample integer not null ,', paste('V',1:limiter,' REAL',collapse=",", sep=""),', CONSTRAINT id_pk PRIMARY KEY (simulation asc, sample asc));'))
} else {
  dbSendQuery(con,paste('create table input (simulation integer not null, sample integer not null ,', paste('V',1:(row*col),' REAL',collapse=",", sep=""),', CONSTRAINT id_pk PRIMARY KEY (simulation asc, sample asc));'))
}
#save.data <- 'insert into input (simulation,sample,value) values(:aa,:bb,:cc);'
#save original data
input <- matrix(0,nrow=length(files.names[file.exist]),ncol=(row*col))
for(i in 1:length(files.names[file.exist])) {
  file.init <-  open.ncdf(files.names[file.exist][i])
  soil.init <- as.vector(get.var.ncdf(file.init, variable)[,,1]) #only first layer
  input[i,] <- soil.init #save it for prediction
  print(paste('Saving sample:',files.names[file.exist][i]))
  dbBeginTransaction(con)  
  if(limiter!='') {
    res <- dbSendQuery(con,statement=paste("INSERT INTO input VALUES (-1,",i,',', paste(unlist(soil.init[1:limiter]),collapse=","),");"))
    #res <- dbGetPreparedQuery(con,save.data,data.frame(aa=rep(-1,length(soil.init[1:limiter])), bb=rep(i,length(soil.init[1:limiter])), cc=soil.init[1:limiter]))
  } else {
    res <- dbSendQuery(con,statement=paste("INSERT INTO input VALUES (-1,",i,',', paste(unlist(soil.init),collapse=","),");"))
    #res <- dbGetPreparedQuery(con,save.data,data.frame(aa=rep(-1,length(soil.init)), bb=rep(i,length(soil.init)), cc=soil.init))
  }
  dbCommit(con)
}

computeInputs <- function() {
  grd <- expand.grid(col=seq(from=1, to=row, by=1), row=seq(from=1, to=col, by=1))
  coordinates(grd) <- ~ row+col
  gridded(grd) <- TRUE
  
  if(length(missing)==0) {
    return(NULL)
  }
  
  res <- matrix(0,ncol=(row*col),nrow=length(missing))
  for(i in 1:length(missing)) {   
    data.sample <- sample(1:nrow(input),1) #choose one from generated values => at least one file returned from learning part have to exist   
    
    data$isoil <- input[data.sample,]
    num.na <- sample(min.samples:max.samples,1)
    if(useKriging==1) {
      n <- as.vector(randomKriging(data,grd,num.na,col,row))
    } else {
      n <- as.vector(randomIDW(data,grd,num.na,col,row,8,7))
    }
    out <- tryCatch({
      res[i,] <- n
    }, error = function(err) {
      #return(0)
      res[i,] <- as.vector(randomIDW(data,grd,num.na,col,row,8,7))
    })
  }
  return(res)
}

data <- read.csv(data.addr)
get.data <- 'select value from input where sample=:aa and simulation=:bb'

runBack <- function(simulation) {
  if(simulation==-1) {
    print('Generating input samples for predicted inputs')
  } else {
    print(paste('Generating input samples for tested inputs number:',simulation))
  }
  samples <- 1:length(files.names[file.exist]) #as.vector(dbGetPreparedQuery(con,'select distinct sample from input where simulation=:aa order by sample',data.frame(aa=simulation))$sample)
  missing <- c(1:(to-from+1))[-samples]
  groups <- split(missing, ceiling(1:length(missing)/(length(missing)/slaves)))
  
  mpi.bcast.Robj2slave(samples)
  mpi.bcast.Robj2slave(groups)
  mpi.bcast.Robj2slave(input)
  mpi.bcast.cmd(missing <- groups[[mpi.comm.rank()]])
  res <- mpi.remote.exec(computeInputs())
  saveOutput(groups,res,simulation,missing)
  visualizeBack(groups,res,simulation) #visualize inputs
}

visualizeBack <- function(groups,res,simulation) {
  if(do.visualization) {
    if(simulation==-1) {
      print('Visualizating input samples for predicted inputs')
      pdf(paste(input.addr,'/output-back.pdf',sep=""), bg = "white")
    } else {
      print(paste('Visualizating input samples for tested inputs number:',simulation))
      pdf(paste(input.addr,'/test-',simulation,'-back.pdf',sep=""), bg = "white")
    }
    
#     for(i in 1:(to-from)) {
#       image(matrix(dbGetPreparedQuery(con,get.data,data.frame(aa=i, bb=simulation))$value,nrow=row,ncol=col),main=paste("Sample number ",i,' used in reverse prediction',sep=""),col=terrain.colors(140))
#     }
    for(i in 1:length(groups)) {
      for(j in 1:length(groups[[i]])) {
	image(matrix(res[[i]][j,],nrow=row,ncol=col),main=paste("Sample number ",groups[[i]][j],' used in reverse prediction',sep=""),col=terrain.colors(140))
      }
    }
    dev.off()
  }
}

saveOutput <- function(groups,res,simulation,missing) {
  if(simulation==-1) {
    print('Saving generated data for output')
  } else {
    print(paste('Saving generated data for',simulation))
  } 
  for(i in 1:length(groups)) {
    #print(str(res[[i]]))
    for(j in 1:length(groups[[i]])) {
      dbBeginTransaction(con)
      if(limiter!='') {
	#resSQL <- dbGetPreparedQuery(con,save.data,data.frame(aa=rep(simulation,length(res[[i]][j,][1:limiter])), bb=rep(groups[[i]][j],length(res[[i]][j,][1:limiter])), cc=res[[i]][j,][1:limiter]))
	resSQL <- dbSendQuery(con,statement=paste("INSERT INTO input VALUES (",simulation,',',groups[[i]][j],',',paste(unlist(res[[i]][j,][1:limiter]),collapse=","),");"))
      } else {
	#resSQL <- dbGetPreparedQuery(con,save.data,data.frame(aa=rep(simulation,length(res[[i]][j,])), bb=rep(groups[[i]][j],length(res[[i]][j,])), cc=res[[i]][j,]))
	resSQL <- dbSendQuery(con,statement=paste("INSERT INTO input VALUES (",simulation,',',groups[[i]][j],',',paste(unlist(res[[i]][j,]),collapse=","),");"))
      }
      dbCommit(con)
    }
  }
}

if(num.missing.files>0) {
  mpi.bcast.Robj2slave(row)
  mpi.bcast.Robj2slave(col)
  mpi.bcast.Robj2slave(min.samples)
  mpi.bcast.Robj2slave(max.samples)
  mpi.bcast.Robj2slave(computeInputs)
  mpi.bcast.Robj2slave(func.addr)
  mpi.bcast.Robj2slave(data)
  mpi.bcast.Robj2slave(get.data)
  #mpi.bcast.Robj2slave(save.data)
  mpi.bcast.Robj2slave(save.db)
  mpi.bcast.Robj2slave(useKriging)
  mpi.bcast.cmd(source(paste(func.addr,"/random-init-soil.r",sep="")))
  mpi.bcast.cmd(library(sp))
  mpi.bcast.cmd(library(gstat))
  mpi.bcast.cmd(library(fields))
  back <- runBack(-1) # run back algorithm for original data
}


#run back algorithm for testing data
if(testing==1) {
  for(i in 1:length(input.test)) {
    if(file.exists(input.test[i])) {
      print(paste('Found test number:',tests[i]))
      data.test <- as.matrix(read.csv(input.test[i]))
      print(paste('Saving data for test',tests[i]))
      input <- t(data.test)
      for(j in 1:ncol(data.test)) {
 	dbBeginTransaction(con) 	
 	if(limiter!='') {
	  #res <- dbGetPreparedQuery(con,save.data,data.frame(aa=rep(tests[i],length(soil.init[1:limiter])), bb=rep(j,length(soil.init[1:limiter])), cc=data.test[,j][1:limiter]))
	  res <- dbSendQuery(con,statement=paste("INSERT INTO input VALUES (",tests[i],',',j,',', paste(unlist(soil.init[1:limiter]),collapse=","),");"))
	} else {
	  #res <- dbGetPreparedQuery(con,save.data,data.frame(aa=rep(tests[i],length(soil.init)), bb=rep(j,length(soil.init)), cc=data.test[,j]))
	  res <- dbSendQuery(con,statement=paste("INSERT INTO input VALUES (",tests[i],',',j,',', paste(unlist(soil.init),collapse=","),");"))
	}
 	dbCommit(con)
      }
      if(num.missing.files>0) {
	runBack(tests[i])
      }
    } else {
      print(paste('Test number',tests[i],'is missing'))
    }
  }
}

dbDisconnect(con)
if(num.missing.files>0) {
  mpi.close.Rslaves()
  mpi.quit()
}