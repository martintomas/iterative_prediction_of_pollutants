library(RSQLite)

args <- commandArgs(trailingOnly = TRUE)

file.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db'
file.pom.db <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning/pom-data.db'
load.dir <- args[3] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning'
save.dir <- paste(load.dir,'/',args[4],sep="") #'/home/trn/Desktop/diploma-thesis/R/scripts/learning/sample'
load.steps <- paste(load.dir,'/steps.csv',sep="")
load.steps.db <- paste(load.dir,'/steps.db',sep="")

#coordinates in map.csv (arguments from main python script), now whole cell is generated at once
row <- as.numeric(args[5]) #10 
col <- args[6] #'10' #string containing values from map (how many and which data are generated)
col <- unlist(strsplit(col,','))

type <- args[7] #'dist'
method.time <- args[8] #'noadd' #can be two methods, add or noadd
method.select <- args[9] #'nonuniform' #can be two methods, uniform or nonuniform

norm.func <- function(value, norm) {
  return((value-norm[1])/(norm[2]-norm[1]))
}

normalize <- function(data,data.norm) {
  if(is.matrix(data)) {
    len <- ncol(data)
    for(i in 1:len) {
      data[,i] <- norm.func(data[,i],data.norm[,i])
    }
  } else {
    len <- length(data)
    for(i in 1:len) {
      data[i] <- norm.func(data[i],data.norm[,i])
    }
  }
  return(data)
}

drv <- dbDriver("SQLite")

#load data-pom
con <- dbConnect(drv, dbname = file.pom.db)
data <- as.vector(unlist(dbGetPreparedQuery(con,'select * from data where row_names=:aa',data.frame(aa=(row)))))
data <- data[2:length(data)]
dbDisconnect(con)

if(type=='all') {
  data.copy <- rep(1,length(data)) #everything is in one group
} else {
  if(method.select=='uniform') {
    seq.data <- as.vector(unlist(read.csv(load.steps)))
  } else {  
    con <- dbConnect(drv, dbname = load.steps.db)
    seq.data <- as.vector(unlist(dbGetPreparedQuery(con,'select * from seqData where row_names=:aa',data.frame(aa=(row)))))
    seq.data <- seq.data[2:length(seq.data)]
    dbDisconnect(con)
  }

  #get indexes of output data used for learning
  data[data<seq.data[1]] <- NA
  data[data>seq.data[length(seq.data)]] <- NA
  
  data.copy <- rep(Inf,length(data))
  for(i in 2:length(seq.data)) {
    data.copy[data>=seq.data[i-1] & data<=seq.data[i] & data.copy==(Inf)] <- (i-1) #which variables are in which sequenci
  }
}
data <- data.copy[data.copy!=(Inf)]
data.num <- which(data.copy!=(Inf))
rm(data.copy)

con <- dbConnect(drv, dbname = file.db)
output <- matrix(dbGetPreparedQuery(con,'select output.value from output inner join test on output.simulation=test.simulation where output.measurement=:aa and test.test=0 order by output.simulation;',data.frame(aa=data.num))$value,ncol=length(data.num))
input <- as.vector(dbGetPreparedQuery(con,'select input.value from input inner join test on input.simulation=test.simulation where input.info=:aa and test.test=0 order by input.simulation;',data.frame(aa=(row)))$value)

output.norm <- t(as.matrix(dbGetPreparedQuery(con,'select min,max from measurement where id=:aa',data.frame(aa=data.num))))
input.norm <- t(as.matrix(dbGetPreparedQuery(con,'select min,max from inputInfo where id=:aa',data.frame(aa=row))))

#normalization
output <- normalize(output,output.norm)
input <- as.vector(normalize(as.matrix(input),input.norm))

#create learning samples
for(i in 1:length(col)) {
  if(col[i]!='R' & col[i]!='I' & col[i]!='NA') { #repeated, ignored or NA value
    #print(paste('Creating learning sample with index:',col[i]))
    if(method.time=='add') {
      id <- which(data<=i)
    } else {
      id <- which(data==i)
    }
    
    #save some info
    save.file <- file(paste(save.dir,'-info_',col[i],'',sep=""), open = "wt") #save to file
    sink(save.file, type = "output")
    #cat(length(id))
    #cat('\n')
    cat(id)
    sink(NULL)
    
    #save outputs
    save.file <- file(paste(save.dir,'_',col[i],'.data',sep=""), open = "wt") #save to file
    sink(save.file, type = "output")
    cat(paste(length(input),length(id),1))
    for(i in 1:length(input)) {
      cat('\n')
      cat(output[i,id])
      cat('\n')
      cat(input[i])
    }
    #sink(type = "output") 
    sink(NULL)
  }
}

dbDisconnect(con)
