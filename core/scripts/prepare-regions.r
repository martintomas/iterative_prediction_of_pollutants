library(RSQLite)
library(ncdf)

args <- commandArgs(trailingOnly = TRUE)

file.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db'
save.dir <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning'
map.save.dir <- paste(save.dir,'/map.csv',sep="")
regions.save.csv.dir <- paste(save.dir,'/regions.csv',sep="")
regions.save.dir <- paste(save.dir,'/split-regions.rda',sep="")
file.analysis.addr <- args[3] #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.csv' #to obtain water file
func.addr <- args[4] #'/home/trn/diploma-thesis/software/multi-iteration/scripts'
file.init.addr <- args[5] #'/home/trn/Desktop/CMAQ/cmaq471_bap/init/AI219_pop_stor_1983_87.nc'

num.samples <- as.numeric(args[6]) #2
min.regions <- as.numeric(args[7]) #10
max.regions <- as.numeric(args[8]) #100
iteration <- as.numeric(args[9]) #1
limiter <- '' #200 #just for testing, if not used, set up to ''(empty string)

source(paste(func.addr,"/samples-functions.r",sep=""))

variable <- "STOR_pop2"
file.init <-  open.ncdf(file.init.addr)
soil.init <- get.var.ncdf(file.init, variable)
soil <- as.vector(soil.init[,,1])
row <- get.var.ncdf(file.init, "row")
col <- get.var.ncdf(file.init, "col")
lev <- get.var.ncdf(file.init, "lev")
lev <- lev[length(lev)]
row <- row[length(row)]
col <- col[length(col)]
time <- 1

regions <- trunc(seq(min.regions,max.regions,length.out=num.samples))

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = file.db)

res <- dbSendQuery(con,'select min,max from inputInfo')
input.norm <- t(as.matrix(fetch(res,n=-1))) #norm values for input
dbClearResult(res)

#input.diff <- apply(cbind((soil-input.norm[2,]),(soil-input.norm[1,])),1,mean) #
input.diff <- (input.norm[2,]-input.norm[1,])-soil #what are differences between min and max value
#hist(input.diff)
#limits <- range(input.diff)
limits <- c(min(input.norm[1,]-soil),max(input.norm[2,]-soil))
#regions.splits <- sapply(regions,function(x) seq(limits[1],limits[2],length.out=x+1))
regions.splits <- sapply(regions,function(x) c(as.vector(quantile(unique(c(input.norm[1,]-soil,input.norm[2,]-soil)),prob=seq(0,1,length.out=x+1)))))

input.diff <- (input.diff - min(input.diff))*((limits[2]-limits[1])/(max(input.diff)-min(input.diff)))+limits[1]

#show nice visualisation of regions
res <- matrix(Inf,nrow=length(input.diff),ncol=length(regions.splits))
pdf(paste(save.dir,'/predicted-regions-test-iteration-',iteration,'.pdf',sep=""), bg = "white")
for(i in 1:length(regions.splits)) {
  res[,i] <- createRegion(input.diff,regions.splits[[i]])
  image(matrix(res[,i],nrow=row,ncol=col),main=paste('Region map for',regions[i],'regions'),col=terrain.colors(140))  
}
dev.off()

#save map
data.water <- read.csv(file.analysis.addr)$water
if(limiter=='') {
  map <- matrix(1:(ncol(res)*row*col),nrow=(row*col),ncol=ncol(res))
} else {
  map <- matrix(1:(limiter),nrow=(limiter),ncol=ncol(res))
}
map[which(data.water[1:nrow(map)]==1),] <- 'I' #ignore water
print('Saving learning map')
write.csv(map,map.save.dir,row.names=FALSE)

print('Saving split regions info')
save(regions.splits,file=regions.save.dir)

#save analytic predicted regions
print('Saving analytics predicted regions')
res[which(data.water==1),] <- 0
res <- res+1
write.csv(res,regions.save.csv.dir,row.names=FALSE)

dbDisconnect(con)
