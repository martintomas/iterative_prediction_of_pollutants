library(RSQLite)

args <- commandArgs(trailingOnly = TRUE)

regions.addr <- args[1] #'/home/trn/scratch/regions.csv'
pom.db <- args[2] #'/home/trn/diploma-thesis/R/scripts/learning/pom-data.db'
func.addr <- args[3] #'/home/trn/diploma-thesis/software/multi-iteration/scripts'
save.addr <- args[4] #'/home/trn/diploma-thesis/R/scripts/learning'
save.map <- paste(save.addr,'/map.csv',sep="")
min.data <- as.numeric(args[5]) #0
max.data <- as.numeric(args[6]) #70
type <- args[7] #'all'
limiter <- '' #100 #'' #for testing => set up to '' (empty string) if you don't want to use

source(paste(func.addr,"/samples-functions.r",sep=""))

if(limiter=='') {
  regions <- normalizeRegions(as.matrix(read.csv(regions.addr))) #normalize, just to be sure, that some regions are not skipped
  write.csv(regions,regions.addr,row.names=FALSE)
} else {
  regions <- normalizeRegions(as.matrix(read.csv(regions.addr))[1:limiter,])
  #save limited regions
  write.csv(regions,regions.addr,row.names=FALSE)
}

#create maps of regions
map.regions <- matrix('I',ncol=ncol(regions),nrow=max(regions))
for(i in 1:ncol(regions)) {
  reg <- unique(regions[,i])
  map.regions[reg,i] <- reg+((i-1)*max(regions))
}

#first region is water => it is ignored
map.regions[1,] <- 'I'

if(type!='all') {

  #prepare db
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = pom.db)

  #load all data
  res <- dbSendQuery(con,"select * from data order by row_names;") 
  data <- fetch(res,n=-1) #remove row_names
  data <- as.matrix(data[,2:ncol(data)])
  dbClearResult(res)

  #check if region have some data, it can take while
  for(i in 1:nrow(map.regions)) {
    for(j in 1:ncol(map.regions)) {
      if(map.regions[i,j]!='I') { #ignored cells are avoided
	#k <- as.numeric(map.regions[i,j]) #change type to number
	#data.dist <- data[which(regions[,j]==k),]
	data.dist <- data[which(regions[,j]==i),]
	if(!is.matrix(data.dist)) {
	  data.dist <- t(as.matrix(data.dist))
	}
	values.dist <- apply(data.dist,2,min) #tells me which measuremnts to use
	if(length(which(values.dist>=min.data & values.dist<=max.data))==0) { #if there are no data for this sample
	  map.regions[i,j] <- 'NA'
	}
      }
    }
  }
}

write.csv(map.regions,save.map,row.names=FALSE)
