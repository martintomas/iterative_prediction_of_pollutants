library(nnet)
library(RSNNS)
library("RSQLite")
library(Rmpi)

mpi.spawn.Rslaves() #spawn some slaves

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


args <- commandArgs(trailingOnly = TRUE)

input.addr <- args[1] #"/home/trn/Desktop/diploma-thesis/software/post-procesing/input-norm2-100-50.csv"
output.addr <-  args[2] #"/home/trn/Desktop/diploma-thesis/software/post-procesing/output-norm2-100-50.csv"
output.reduced.addr <-  args[3] #"/home/trn/diploma-thesis/software/post-procesing/output-norm2-100-50.csv" #-reduced-20.csv"
file.addr <-  args[4] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-mining' #where to save csv files (even pdf)
func.addr <- file.addr
name.file <-  args[5] #"-norm2-100-50-20-1"
correlations.addr <-  args[6] #"/home/trn/diploma-thesis/R/scripts/data-mining/corr-norm2-100-50.db" #computed from complete matrices
norm.back.addr <-  args[7] #'/home/trn/diploma-thesis/software/post-procesing/input-norm2-back.csv'
method <- args[8] #'nnet'
init.addr <- args[9] #"/home/trn/diploma-thesis/software/post-procesing/init-output.csv" #to set missing values


sd.step <- 1/15
if(method=='nnet') {
  random.inputs <- 5000
} else {
  random.inputs <- 70 #use much less of them (it takes a lot of time to predict)
}
corr.const <- 0.2 #min corr
min.cells <- 25 #used for regression (min number of cells used for compute y), when there is not good correlation
max.cells <- 40
corr.const.reduced <- 0.1
min.cells.reduced <- 30
max.cells.reduced <- 40
tune <- 1
seed <- sample(1:10000,1)
slaves <- (mpi.comm.size()-1) #except of master

source(paste(func.addr,"/compare-func",sep=""))
source(paste(func.addr,"/plot-func",sep="")) #require compare-functions

print("Loading data")
#we need samples in row, cell in columns (caret needs) => we need to do transposition
input <- (data.matrix(read.csv(input.addr)))
output <- (data.matrix(read.csv(output.addr)))
output.reduced <- (data.matrix(read.csv(output.reduced.addr)))
output.knowStations <- which(!is.na(output.reduced[1,]))
output.reduced <- output.reduced[,apply(output.reduced, 2, function(z)all(is.finite(z)))] #remove NA

norm.back <- (data.matrix(read.csv(norm.back.addr)))
norm.back <- norm.back[,1:ncol(input)] #read only cells I need

initOutput <- data.matrix(read.csv(init.addr)) #first column is input, second is output, rows are cells

list.corr <- function(min.cells.f, max.cells.f, corr.const.f, z.f) {
  index.f <- which(z.f >= corr.const.f )
  if(length(index.f)<min.cells.f) { # we are missing some cells
     num <- (length(z.f)-min.cells.f+1)
     index.f <- which(z.f >= (sort(z.f,partial=num)[num]))
  }
  if(length(index.f)>max.cells.f) {
    num <- (length(z.f)-max.cells.f+1)
    index.f <- which(z.f >= (sort(z.f,partial=num)[num]))
    index.f <- index.f[1:max.cells.f] #never return more than max
  }
  return(index.f)
}

readDB <- function() {
  #drv <- dbDriver("SQLite")
  library("RSQLite")
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = correlations.addr)
  res.input <- as.matrix(dbGetPreparedQuery(con, sql.input, data.frame(aa = length(groups),bb = (groups[1]-1))))
  res.output <- as.matrix(dbGetPreparedQuery(con, sql.output, data.frame(aa = length(groups),bb = (groups[1]-1))))
  dbDisconnect(con)  
  colnames(res.input)<-NULL #remove col names
  rownames(res.input)<-NULL #remove row names
  colnames(res.output)<-NULL #remove col names
  rownames(res.output)<-NULL #remove row names
  #!!! we absolute values, we need just how high is correlation
  res.input <- abs(res.input) 
  inter <- intersect(groups,output.knowStations)
  locations <- sapply(inter, function(z) which(groups == z))
  res.output <- abs(res.output[locations,])
  #lets compute first corelations indexes for output.reduced -> input matrix
  corr.index.out <- list()
  for(i in 1:nrow(res.input)) {
    corr.index.out[[i]] <- list.corr(min.cells.reduced, max.cells.reduced, corr.const.reduced, res.input[i,])
  }
  corr.index.in <- list()
  for(i in 1:nrow(res.output)) {
    corr.index.in[[i]] <- list.corr(min.cells, max.cells, corr.const, res.output[i,])
  }
  rm(res.input,res.output)
  return(list(corr.index.out,corr.index.in)) 
}

#!!IMPORTANT, split to node groups
sql.groups <- split(1:ncol(input), ceiling(1:ncol(input)/(ncol(input)/slaves)))
iter.stations <- split(1:length(output.knowStations), ceiling(1:length(output.knowStations)/(length(output.knowStations)/slaves)))

print("Reading correlations")
sql.string.input <- paste(sapply(output.knowStations, function(z) paste("X",z,sep="")),collapse=",") #we compute only known regions
sql.input <- paste("select",sql.string.input,"from input order by row_names limit :aa offset :bb")
sql.string.output <- paste(sapply(1:ncol(input), function(z) paste("X",z,sep="")),collapse=",") #we compute only known regions
sql.output <- paste("select",sql.string.output,"from output order by row_names limit :aa offset :bb")
mpi.bcast.Robj2slave(dbDriver)
mpi.bcast.Robj2slave(correlations.addr)
mpi.bcast.Robj2slave(dbGetQuery)
mpi.bcast.Robj2slave(readDB)
mpi.bcast.Robj2slave(dbGetPreparedQuery)
mpi.bcast.Robj2slave(sql.input)
mpi.bcast.Robj2slave(sql.output)
mpi.bcast.Robj2slave(sql.groups)
mpi.bcast.Robj2slave(list.corr)
mpi.bcast.Robj2slave(min.cells.reduced)
mpi.bcast.Robj2slave(max.cells.reduced)
mpi.bcast.Robj2slave(corr.const.reduced)
mpi.bcast.Robj2slave(min.cells)
mpi.bcast.Robj2slave(max.cells)
mpi.bcast.Robj2slave(corr.const)
mpi.bcast.Robj2slave(output.knowStations)
mpi.bcast.cmd(groups <- sql.groups[[mpi.comm.rank()]])
res <- mpi.remote.exec(readDB())
corr.index <- list(list(),list()) #for every corr.index
for(i in 1:length(res)) {
  corr.index[[1]][sql.groups[[i]]] <- res[[i]][[1]]
  corr.index[[2]][(length(corr.index[[2]])+1):(length(corr.index[[2]])+length(res[[i]][[2]]))] <- res[[i]][[2]]
}
rm(res)

#inTrain <- createDataPartition(y = 1:nrow(input), p = .9, list = FALSE)
#test <- c(2, 3, 15, 35, 40)
test <- c(1,2,6,7,8,10,15,30,45,70,120,150,166,180,188,190,201,220,270,280,300,310,330,380,400,401,402,403,404)
inTrain <- (1:nrow(input))[-test]

#columns are cells and every row is new random variable
inputs.rand <- function(input.f, amount, sd.step.f,seed.f) {
  rand <- sapply(input.f, function(z) {
    set.seed(seed.f)
    return(rnorm(amount, mean=z, sd=sd.step.f))
  })
  #add even first find value, which is used for computation
  rand <- rbind(rand, input.f)
  return(rand)
}

mpi.bcast.Robj2slave(output)
mpi.bcast.Robj2slave(output.reduced) #on all nodes
mpi.bcast.Robj2slave(input) #on all nodes
mpi.bcast.Robj2slave(nnet) #on all nodes (function)
mpi.bcast.Robj2slave(elman) #on all nodes (function)
mpi.bcast.Robj2slave(jordan) #on all nodes (function)
mpi.bcast.Robj2slave(mlp) #on all nodes (function)
mpi.bcast.Robj2slave(rbf) #on all nodes (function)
mpi.bcast.Robj2slave(predict) #on all nodes (function)
mpi.bcast.Robj2slave(corr.index) 
mpi.bcast.Robj2slave(inTrain) #on all nodes
mpi.bcast.Robj2slave(method) #on all nodes
mpi.bcast.Robj2slave(tune) #on all nodes
mpi.bcast.Robj2slave(initOutput) #on all nodes #to fit bad values
mpi.bcast.Robj2slave(inputs.rand) #on all nodes (function)

timeMethod <- Sys.time()

print("Computing regressions")
#need groups variable -> got from database load 
input.new <- function() { #compute new input on nodes
  i <- 1
  find <- matrix(NA, nrow=nrow(input),ncol=length(groups))
  for(z in groups) {
    find[,i] <- tryCatch({
      if(method=='nnet') {
	m <- nnet(output.reduced[inTrain,corr.index[[1]][[z]]], input[inTrain,z], size=3, abstol=0.01, entropy=T) #use only ones which correlate, output (reduced) -> input  
      } else if(method=='elman') {
	m <-elman(output.reduced[inTrain,corr.index[[1]][[z]]], input[inTrain,z])
      } else if(method=='jordan') {
        m <-jordan(output.reduced[inTrain,corr.index[[1]][[z]]], input[inTrain,z])
      } else if(method=='mlp') {
	m <-mlp(output.reduced[inTrain,corr.index[[1]][[z]]], input[inTrain,z])
      } else if(method=='rbf') {
        m <-rbf(output.reduced[inTrain,corr.index[[1]][[z]]], input[inTrain,z])
      } else {
        stop("Unknown method")
      }
      predict(m, newdata=output.reduced[,corr.index[[1]][[z]]]) #we predict even cross validation
    }, error = function(err) { #error occured, pick value from init soil file (global model)
      return(rep(initOutput[1,z],nrow(input)))
    })
    i <- i+1
  }
  return(list(find))
}
mpi.bcast.Robj2slave(input.new)
find.mpi <- mpi.remote.exec(input.new()) #find input matrix
find <- matrix(NA, nrow=nrow(find.mpi[[1]][[1]]), ncol=ncol(input)) #create one matrix
for(i in 1:length(find.mpi)) {
  find[,sql.groups[[i]]] <- find.mpi[[i]][[1]]
}
rm(find.mpi)

if(is.matrix(find)) {
  len.samples <- nrow(find)
  len.variables <- ncol(find)
} else {
  len.samples <- 1
  len.variables <- length(find)
  find <- t(as.matrix(find))
}

mpi.bcast.Robj2slave(find) #on all nodes
mpi.bcast.Robj2slave(len.samples) #on all nodes
mpi.bcast.Robj2slave(random.inputs) #on all nodes
mpi.bcast.Robj2slave(sd.step) #on all nodes
mpi.bcast.Robj2slave(seed) #on all nodes

output.new <- function(index) { #compute predicted output
  if(is.na(index)) return(NULL)  
  out <- tryCatch({
    if(method=='nnet') {
      n <- nnet(input[inTrain,corr.index[[2]][[index]]], output[inTrain,output.knowStations[index]], size=3, abstol=0.01, entropy=T) #input -> output 
    } else if(method=='elman') {
      n <-elman(input[inTrain,corr.index[[2]][[index]]], output[inTrain,output.knowStations[index]])
    } else if(method=='jordan') {
      n <-jordan(input[inTrain,corr.index[[2]][[index]]], output[inTrain,output.knowStations[index]])
    } else if(method=='mlp') { 
      n <-mlp(input[inTrain,corr.index[[2]][[index]]], output[inTrain,output.knowStations[index]])
    } else if(method=='rbf') {
      n <-rbf(input[inTrain,corr.index[[2]][[index]]], output[inTrain,output.knowStations[index]])
    } else {
      stop("Unknown method")
    }
    
    distanc <- matrix(NA, nrow=(random.inputs+1), ncol=len.samples) #matrix of distances, for every sample and random vector
    for(i in 1:len.samples) { #for all samples
      #generate needed random values
      input.new <- inputs.rand(find[i,corr.index[[2]][[index]]],random.inputs,sd.step,seed) #matrix of random values, columns are cells, rows are samples   
      colnames(input.new) <- paste('V',corr.index[[2]][[index]],sep="")
      pred <- predict(n, newdata=input.new) #prediced values
      #compute even distances   
      distanc[,i] <- sapply(pred, function(j) (j-output.reduced[i,index])^2)
    }
    distanc #to return distanc
  }, error = function(err) { #error occured, return output from simulations with init soil concentrations
    #return(matrix(0, nrow=(random.inputs+1), ncol=len.samples))
    return(matrix(initOutput[2,output.knowStations[index]], nrow=(random.inputs+1), ncol=len.samples))
  })
  return(out)
}

#needs stations variable, on every node we compute group of train functions for predicted output 
iter.nodes <- function() {
  distances <- matrix(0, nrow=(random.inputs+1), ncol=len.samples)
  for(index in stations) {
     res <- output.new(index)
     distances <- distances+res
  }
  return(list(distances))
}

mpi.bcast.Robj2slave(output.new) #on all nodes (function)
mpi.bcast.Robj2slave(iter.nodes) #on all nodes (function)
mpi.bcast.Robj2slave(iter.stations) #on all nodes
mpi.bcast.cmd(stations <- iter.stations[[mpi.comm.rank()]]) 
res <- mpi.remote.exec(iter.nodes())
distances <- matrix(0, nrow=(random.inputs+1), ncol=len.samples)
for(i in 1:length(res)) { #put it all together
  distances <- distances+res[[i]][[1]] #sum all differences
}
rm(res)

res.input <- find
for(i in 1:len.samples) {
  out.min <- which.min(distances[,i])
  if(out.min<=random.inputs) {
    res.input[i,] <- inputs.rand(find[i,],out.min,sd.step,seed)[out.min,]
  } #else { #is last example, don't need to generate new vector
    #res.input[i,] <- find[i,] #don't need, res.input is copy of find
  #}
}

#denormalization (before we save), compute only for trained
real <- input[-inTrain,]
computed <- find[-inTrain,]
computed.check <- res.input[-inTrain,]
if(!is.matrix(real)) {
  real <- t(as.matrix(real))
  computed <- t(as.matrix(computed))
  res.input <- t(as.matrix(res.input))
  computed.check <- t(as.matrix(computed.check))
}
for(i in 1:ncol(real)) {
  real[,i] <- (input[-inTrain,i]*(input.norm.back[2,i]-input.norm.back[1,i]))+input.norm.back[1,i]
  computed[,i] <- (find[-inTrain,i]*(input.norm.back[2,i]-input.norm.back[1,i]))+input.norm.back[1,i]
  computed.check[,i] <- (res.input[-inTrain,i]*(input.norm.back[2,i]-input.norm.back[1,i]))+input.norm.back[1,i]
}
  
print("Saving results")
write.csv(real, paste(file.addr,"/real-",method,name.file,".csv",sep=""), row.names=FALSE)
write.csv(computed, paste(file.addr,"/computed-",method,name.file,".csv",sep=""), row.names=FALSE)
write.csv(computed.check, paste(file.addr,"/computed-check-",method,name.file,".csv",sep=""), row.names=FALSE)

pdf(paste(file.addr,"/",method,name.file,".pdf",sep=""), bg = "white")
plot.data(input,res.input,inTrain)
plot.data(input,find,inTrain)
dev.off()

mistakes.checked <- comparetment3(input,res.input)
mistakes <- comparetment3(input,find)
real.time <- as.numeric(difftime(Sys.time(),timeMethod, units="secs"))
info <- matrix("",nrow=10,ncol=2) #CHANGE nrow when you need add more info
info[1,] <- c("Sum of mistakes, checked  version:",sum(mistakes.checked)/nrow(input))
info[2,] <- c("Sum of mistakes (nontrained), checked  version:",sum(mistakes.checked[-inTrain])/nrow(input[-inTrain,]))
info[3,] <- c("Sum of mistakes:",sum(mistakes)/nrow(input))
info[4,] <- c("Sum of mistakes (nontrained):",sum(mistakes[-inTrain])/nrow(input[-inTrain,]))
info[5,] <- c("Used samples for testing:",paste(c(1:nrow(input))[-inTrain], collapse=" "))
info[6,] <- c("How long it has taken (sec):",real.time)
info[7,] <- c("Mistakes of samples (nontrained), checked version:",paste(mistakes.checked[-inTrain], collapse=" "))
info[8,] <- c("Mistakes of samples (nontrained):",paste(mistakes[-inTrain], collapse=" "))
info[9,] <- c("Mistakes of samples, checked version:",paste(mistakes.checked, collapse=" "))
info[10,] <- c("Mistakes of samples:",paste(mistakes, collapse=" "))
write.csv(info, paste(file.addr,"/info-geo-",method,name.file,".csv",sep=""), row.names=FALSE)

mpi.close.Rslaves()
mpi.quit()