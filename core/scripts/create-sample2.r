library(RSQLite)

args <- commandArgs(trailingOnly = TRUE)

file.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db'
file.pom.db <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning/pom-data.db'
file.map <- args[3] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning/map.csv'
load.dir <- args[4] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning'
save.dir <- paste(load.dir,'/',args[5],sep="") #'/home/trn/Desktop/diploma-thesis/R/scripts/learning/sample'
load.steps <- paste(load.dir,'/steps.csv',sep="")
load.steps.db <- paste(load.dir,'/steps.db',sep="")
func.addr <- args[6] #'/home/trn/diploma-thesis/software/multi-iteration/scripts'

#coordinates in map.csv (arguments from main python script), now whole cell is generated at once
row <- args[7] #'10,20,30,40,50'
row <- as.numeric(unlist(strsplit(row,',')))

type <- args[8] #'dist'
method.time <- args[9] #'noadd' #can be two methods, add or noadd
method.select <- args[10] #'nonuniform' #can be two methods, uniform or nonuniform

method <- args[11] #'region' #can be cell or region
if(method=='region') {
  library(ncdf)
  #load original data
  file.init.addr <- args[12] #'/home/trn/Desktop/CMAQ/cmaq471_bap/init/AI219_pop_stor_1983_87.nc'
  file.init <-  open.ncdf(file.init.addr)
  soil.init <- get.var.ncdf(file.init, "STOR_pop2")[,,1] #use only first layer
  #load regions
  file.region <- args[13] #'/home/trn/Desktop/diploma-thesis/software/multi-iteration/init/map-regions.csv'
  regions <- as.matrix(read.csv(file.region))
}

source(paste(func.addr,"/samples-functions.r",sep=""))

drv <- dbDriver("SQLite")

#load cols
col <- as.matrix(read.csv(file.map))[row,]
if(!is.matrix(col)) {
  col <- as.matrix(col)
}

#load data-pom
con <- dbConnect(drv, dbname = file.pom.db)
data <- as.matrix(dbGetPreparedQuery(con,'select * from data where row_names=:aa',data.frame(aa=(row))))
data <- (data[,2:ncol(data)])
data <- checkMatrix(data)
dbDisconnect(con)

if(type=='all') {
  data.copy <- matrix(1,nrow=nrow(data),ncol=ncol(data)) #everything is in one group
} else {
  if(method.select=='uniform') {
    seq.data <- as.vector(unlist(read.csv(load.steps)))
    seq.data.copy <- seq.data
  } else {  
    con <- dbConnect(drv, dbname = load.steps.db)
    seq.data <- as.matrix(dbGetPreparedQuery(con,'select * from seqData where row_names=:aa',data.frame(aa=(row))))
    seq.data <- seq.data[,2:ncol(seq.data)]
    seq.data <- checkMatrix(seq.data)
    dbDisconnect(con)
  }

  #get indexes of output data used for learning
  data[data<min(seq.data)] <- NA
  data[data>max(seq.data)] <- NA
    
  data.copy <- matrix(Inf,nrow=nrow(data),ncol=ncol(data))
  for(j in 1:nrow(data)) {
    if(is.matrix(seq.data)) seq.data.copy <- seq.data[j,]
    for(i in 2:length(seq.data)) {
      data.copy[j,data[j,]>=seq.data.copy[i-1] & data[j,]<=seq.data.copy[i] & data.copy[j,]==(Inf)] <- (i-1) #which variables are in which sequenci
    }
  }
}

#throw away cols, where only Inf are
# bad.cols <- rep(FALSE, ncol(data.copy))
# for(i in 1:ncol(data.copy)) {
#   if(sum(data.copy[,i]==Inf) == nrow(data.copy)) {
#     bad.cols[i] <- TRUE
#   }
# }

data <- data.copy #[,!bad.cols]
data <- checkMatrix(data)
#data.num <- which(bad.cols==FALSE)
rm(data.copy)

con <- dbConnect(drv, dbname = file.db)
#load data for normalization
#output.norm <- t(as.matrix(dbGetPreparedQuery(con,'select min,max from measurement where id=:aa',data.frame(aa=data.num))))
#load values
#output <- matrix(dbGetPreparedQuery(con,'select output.value from output inner join test on output.simulation=test.simulation where output.measurement=:aa and test.test=0 order by output.simulation;',data.frame(aa=data.num))$value,ncol=length(data.num))

#load data for normalization
con <- dbConnect(drv, dbname = file.db)
res <- dbSendQuery(con,'select min,max from measurement') 
output.norm <- t(as.matrix(fetch(res,n=-1))) #data for denormalization
dbClearResult(res)

#load values
res <- dbSendQuery(con,'select output.value from output inner join test on output.simulation=test.simulation where test.test=0 order by output.simulation;') 
output <- matrix(fetch(res,n=-1)$value,ncol=ncol(output.norm),byrow=TRUE) #data for denormalization
dbClearResult(res)

input.get <- 'select input.value from input inner join test on input.simulation=test.simulation where input.info=:aa and test.test=0 order by input.simulation;'
input.norm.get <- 'select min,max from inputInfo where id=:aa'
if(method=='cell') {
  input <- matrix(dbGetPreparedQuery(con,input.get,data.frame(aa=(row)))$value,ncol=length(row))
  input.norm <- t(as.matrix(dbGetPreparedQuery(con,input.norm.get,data.frame(aa=row))))
} else {   
  input <- matrix(0,ncol=length(row),nrow=nrow(output))
  input.norm <- matrix(0,ncol=length(row),nrow=2)
  for(i in 1:length(row)) {
    len.region <- which(regions==row[i])
    input[,i] <- classificationValues(matrix(dbGetPreparedQuery(con,input.get,data.frame(aa=len.region))$value,ncol=length(len.region)),soil.init[len.region])
    input.norm[,i] <- classificationValues(t(as.matrix(dbGetPreparedQuery(con,input.norm.get,data.frame(aa=len.region)))),soil.init[len.region])
  }
}

#normalization
output <- normalize(output,output.norm)
input <- normalize(input,input.norm)

#create learning samples
for(i in 1:nrow(col)) {
  for(j in 1:ncol(col)) {
    if(col[i,j]!='R' & col[i,j]!='I' & !is.na(col[i,j])) { #repeated, ignored or NA value
      #print(paste('Creating learning sample with index:',col[i]))
      if(method.time=='add') {
	id <- which(data[i,]<=j)
      } else {
	id <- which(data[i,]==j)
      }
      #save some info
      save.file <- file(paste(save.dir,'-info_',col[i,j],'',sep=""), open = "wt") #save to file
      sink(save.file, type = "output")
      #cat(length(id))
      #cat('\n')
      cat(id)
      sink(NULL)
      close(save.file)
      
      #save outputs
      save.file <- file(paste(save.dir,'_',col[i,j],'.data',sep=""), open = "wt") #save to file
      sink(save.file, type = "output")
      cat(paste(nrow(input),length(id),1))
      for(k in 1:nrow(input)) {
	cat('\n')
	cat(output[k,id])
	cat('\n')
	cat(input[k,i])
      }
      #sink(type = "output") 
      sink(NULL)
      close(save.file)
    }
  }
}

dbDisconnect(con)
