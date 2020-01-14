library(RSQLite)

args <- commandArgs(trailingOnly = TRUE)

file.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning/pom-data.db'
file.analysis.addr <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.csv' #to obtain water file
save.dir <- args[3] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning'
save.map <- paste(save.dir,'/map.csv',sep="")
save.steps <- paste(save.dir,'/steps.csv',sep="")
save.steps.db <- paste(save.dir,'/steps.db',sep="")
how.many <- as.numeric(args[4]) #5
test.seq <- 50
min.data <- as.numeric(args[5]) #0
max.data <- as.numeric(args[6]) #90
max.na <- as.numeric(args[7]) #80 # how many % can be maximaly NA values (or repeate values)
type <- args[8] #'dist'
method.time <- args[9] #'noadd' #can be two methods, add or noadd
method.select <- args[10] #'nonuniform' #can be two methods, uniform or nonuniform
method <- args[11] #'region' #can be cell or region

if(method=='cell') {
  data.water <- read.csv(file.analysis.addr)$water
}

#open db
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = file.db)
res <- dbSendQuery(con,"select max(row_names) from data;")
num <- fetch(res,n=-1)[1,1] #number of lines
dbClearResult(res)

#load type dependent variables
if(type=='dist') {
  #nothing now
} else if(type=='corr') {
  #nothing now
} else if(type=='all') {
  #simple input/ouput, can be done immediately
  print('Saving simple map!!')
  map.all <- paste(1:num,sep="")
  if(method=='cell') {
    map.all[which(data.water[1:num]==1)] <- 'I'
  } else {
    map.all[1] <- 'I' #first row is ignored, it is water
  }
  write.csv(map.all,save.map,row.names=FALSE)
  write.csv('all',save.steps,row.names=FALSE)
  quit('no')
} else {
  stop('Unknown method!!!!')
}

if(test.seq>num) {
  test.seq <- num
}

#load all data
res <- dbSendQuery(con,"select * from data order by row_names;") 
data <- fetch(res,n=-1) #remove row_names
data <- data[,2:ncol(data)]
dbClearResult(res)

dbDisconnect(con)

data[data<min.data] <- NA
data[data>max.data] <- NA

#create sequenci (how to split data)
if(method.select=='uniform') {
  #index <- sample(1:num,test.seq) #to create the best seq, use only part of the data
  x <- apply(data,1,function(i) quantile(i, probs = seq(0,1,length.out=how.many+1),na.rm=TRUE)) #first method
  seq.data <- as.vector(apply(x,1,function(i) mean(i,na.rm=TRUE)))
  #seq.data <- quantile(as.vector(data), probs = seq(0,1,length.out=how.many+1),na.rm=TRUE) #second method
  seq.data <- na.omit(seq.data) #remove NA values if exist
  seq.data <- unique(seq.data) #remove redundand values => every example is unique
  seq.data[1] <- min.data
  seq.data[length(seq.data)] <- max.data
} else {
  seq.data <- t(apply(data,1,function(i) quantile(i, probs = seq(0,1,length.out=how.many+1),na.rm=TRUE))) #every cell has own distribution
  seq.data[,1] <- min.data
  seq.data[,ncol(seq.data)] <- max.data
  seq.data[which(is.na(seq.data))] <- min.data
}
if(!is.matrix(seq.data)) seq.data <- t(as.matrix(seq.data))

# data[is.na(data)] <- -Inf #used to get rid of NA
# data.seq <- as.matrix(data)
# if(method.select=='uniform') { #all data will have same split sequenci
#   for(i in 2:length(seq.data)) {
#     data.seq[data>=seq.data[i-1] & data<=seq.data[i]] <- (i-1) #which variables are in which sequenci
#   }
# } else {
#   for(j in 1:nrow(data)) {
#     for(i in 2:length(seq.data[j,])) {
#       data.seq[j,data[j,]>=seq.data[j,i-1] & data[j,]<=seq.data[j,i]] <- (i-1) #which variables are in which sequenci
#     }
#   }
# }
# data.seq[data.seq==-Inf] <- NA #return NA values

#generate prediction map
#max.i <- 1 #max(map,na.rm=TRUE)
#data.copy <- (Inf,nrow=nrow(data),ncol=ncol(data))
map <- matrix(paste(1:(nrow(data)*(ncol(seq.data)-1)),sep=""),nrow=nrow(data),ncol=(ncol(seq.data)-1))
if(method.select=='uniform') seq.data.copy <- seq.data[1,] #use only first row
if(method=='cell') {
  map[which(data.water[1:nrow(data)]==1),] <- 'I'
} else if(method=='region') {
  map[1,] <- 'I' #first region is water
}
for(i in 1:nrow(map)) {
  if(map[i,1]!='I') { #ignored cell are avoided
    data.copy <- rep(Inf,ncol(data))
    if(method.select!='uniform') seq.data.copy <- seq.data[i,]
    for(j in 1:(length(seq.data.copy)-1)) {  
      data.cell <- which(data[i,]>=seq.data.copy[j] & data[i,]<=seq.data.copy[j+1] & data.copy==(Inf))
      if(length(data.cell)>0) {
	data.copy[data.cell] <- j
      } else {  
	if((j==1) | (method.time=='noadd')) map[i,j] <- 'NA'
	else map[i,j] <- 'R' #map[i,j-1] #R, used for repeate
      }
    }
  }
}

#remove cols, where is more NA or repeated values then set up in max.na
bad.remove <- rep(0,ncol(map))
for(i in 1:ncol(map)) {
  bad.remove[i] <- sum(map[,i]=='R' | map[,i]=='NA' | is.na(map[,i]) | map[,i]=='I')
}

if(all(bad.remove==nrow(map))) { #if all rows in map are bad (100% of NA or repeated values)
  stop('THIS METHOD IS NOT SUITABLE TO FIND GOOD LEARNING SPLIT, USE DIFFERENT APPROACH OR CHANGE SET UP OF THIS METHOD!!!!')
}

na.limit <- ((nrow(map)/100)*max.na)
if(all(bad.remove > na.limit)) {
  print('Limit of NA or repeated values were increased, so at least one map is used.')
  na.limit <- min(bad.remove)
}
map <- map[,!(bad.remove > na.limit)]
if(!is.matrix(map)) map <- (as.matrix(map))
write.csv(map,save.map,row.names=FALSE)

print(paste('Map was generated for',ncol(map),'samples'))

#remove bad intervals from sequenci
bad.remove.seq <- rep(FALSE,ncol(seq.data))
for(i in 1:length(bad.remove)) {
  if(bad.remove[i]>na.limit) {
    if(!bad.remove.seq[i] & i!=1) bad.remove.seq[i] <- TRUE
    else bad.remove.seq[i+1] <- TRUE
  }
}
seq.data <- seq.data[,!bad.remove.seq]

#save sequenci
if(method.select=='uniform') {
  write.csv(seq.data,save.steps,row.names=FALSE) #save sequenci #csv is enough
} else {
  con2 <- dbConnect(drv, dbname = save.steps.db)
  dbSendQuery(con2, "drop table if exists seqData;") #remove old data, if there are
  
  seq.data.copy <- as.data.frame(cbind(1:nrow(seq.data),seq.data))
  names(seq.data.copy) <- c("row_names",1:ncol(seq.data))

  #dbWriteTable(con2, "seqData", seq.data.copy, row.names=FALSE) #rows are output
  #dbSendQuery(con2,dbBuildTableDefinition(con,'seqDdata',seq.data))
  dbSendQuery(con2,paste('create table seqData (row_names integer,', paste('V',colnames(seq.data.copy[,2:ncol(seq.data.copy)]),' REAL',collapse=",", sep=""),');'))
  dbBeginTransaction(con2)
  for(i in 1:nrow(seq.data.copy)) {
    dbSendQuery(con2,statement=paste("INSERT INTO seqData VALUES (", paste(unlist(seq.data.copy[i,]),collapse=","),");"))
  }
  dbCommit(con2)
  dbGetQuery(con2, "create unique index idata on seqData(row_names)")
  
  dbDisconnect(con2) 
}
print("Map sequenci was saved.")