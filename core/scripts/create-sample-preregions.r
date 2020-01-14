library(RSQLite)
library(ncdf)

args <- commandArgs(trailingOnly = TRUE)

file.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db'
save.dir <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning'
map.dir <- paste(save.dir,'/map.csv',sep="")
regions.dir <- paste(save.dir,'/split-regions.rda',sep="")
sample.save.dir <- paste(save.dir,'/sample',sep="")
func.addr <- args[3] #'/home/trn/diploma-thesis/software/multi-iteration/scripts'
file.init.addr <- args[4] #'/home/trn/Desktop/CMAQ/cmaq471_bap/init/AI219_pop_stor_1983_87.nc'

#coordinates in map.csv (arguments from main python script), now whole cell is generated at once
row <- args[5] #'1,3'
row <- as.numeric(unlist(strsplit(row,',')))

source(paste(func.addr,"/samples-functions.r",sep=""))

variable <- "STOR_pop2"
file.init <-  open.ncdf(file.init.addr)
soil.init <- get.var.ncdf(file.init, variable)
soil <- as.vector(soil.init[,,1])

#load cols
col <- as.matrix(read.csv(map.dir))[row,]
col <- checkMatrix(col)

#regions <- trunc(seq(min.regions,max.regions,length.out=ncol(col)))

load(regions.dir) #we obtain regions.splits variable

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = file.db)

res <- dbSendQuery(con,'select min,max from measurement')
output.norm <- t(as.matrix(fetch(res,n=-1))) #norm values for input
dbClearResult(res)

res <- dbSendQuery(con,'select output.value from output inner join test on output.simulation=test.simulation where test.test=0 order by output.simulation;')
output <- matrix(fetch(res,n=-1)$value,ncol=ncol(output.norm),byrow=TRUE)
dbClearResult(res)

output <- normalize(output,output.norm)

input.get <- 'select input.value from input inner join test on input.simulation=test.simulation where input.info=:aa and test.test=0 order by input.simulation;'
input.norm.get <- 'select min,max from inputInfo where id=:aa'
input <- matrix(dbGetPreparedQuery(con,input.get,data.frame(aa=(row)))$value,ncol=length(row))
#input.norm <- t(as.matrix(dbGetPreparedQuery(con,input.norm.get,data.frame(aa=row))))

#create learning samples
for(i in 1:nrow(col)) {
  #regions.splits <- sapply(regions,function(x) seq(input.norm[1,i],input.norm[2,i],length.out=x))
  for(j in 1:ncol(col)) {
    if(col[i,j]!='R' & col[i,j]!='I' & !is.na(col[i,j])) { #repeated, ignored or NA value   
      #compute classification values
      indexes <- createRegion(input[,i]-soil[row[i]],regions.splits[[j]])  #actual value - minimal value (we measure how much it grows)
      x <- matrix(0,nrow=length(indexes),ncol=length(regions.splits[[j]])-1)
      sapply(1:length(indexes),function(k) x[k,indexes[k]] <<- 1)
      #save outputs
      save.file <- file(paste(sample.save.dir,'_',col[i,j],'.data',sep=""), open = "wt") #save to file
      sink(save.file, type = "output")
      cat(paste(nrow(input),ncol(output),ncol(x)))
      for(k in 1:nrow(input)) {
	cat('\n')
	cat(output[k,])
	cat('\n')
	cat(x[k,])
      }
      #sink(type = "output") 
      sink(NULL)
      close(save.file)
    }
  }
}

dbDisconnect(con)
