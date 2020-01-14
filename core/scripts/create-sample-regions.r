library(RSQLite)
library(ncdf)

args <- commandArgs(trailingOnly = TRUE)

file.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db'
file.pom.db <- args[2] #'/home/trn/scratch/pom-data.db'
file.map <- args[3] #'/home/trn/scratch/map.csv'
save.dir <- args[4] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning/sample'
func.addr <- args[5] #'/home/trn/diploma-thesis/software/multi-iteration/scripts'
file.init.addr <- args[6] #'/home/trn/Desktop/CMAQ/cmaq471_bap/init/AI219_pop_stor_1983_87.nc'
file.regions <- args[7] #'/home/trn/scratch/regions.csv'
variables <- 'STOR_pop2'
min.data <- as.numeric(args[8]) #0
max.data <- as.numeric(args[9]) #70
type <- args[10] #'all'

#coordinates in map.csv (arguments from main python script), now whole cell is generated at once
row <- args[11] #'3,4,5'
row <- as.numeric(unlist(strsplit(row,',')))

#load original data
file.init <-  open.ncdf(file.init.addr)
soil.init <- as.vector(get.var.ncdf(file.init, variables)[,,1]) #use only first layer
#load regionss
regions <- as.matrix(read.csv(file.regions))

source(paste(func.addr,"/samples-functions.r",sep=""))

drv <- dbDriver("SQLite")

#load cols
col <- as.matrix(read.csv(file.map))[row,]
if(!is.matrix(col)) {
  col <- as.matrix(col)
}

#load data for normalization
con <- dbConnect(drv, dbname = file.db)
res <- dbSendQuery(con,'select min,max from measurement') 
output.norm <- t(as.matrix(fetch(res,n=-1))) #data for denormalization
dbClearResult(res)

#load values
res <- dbSendQuery(con,'select output.value from output inner join test on output.simulation=test.simulation where test.test=0 order by output.simulation;') 
output <- matrix(fetch(res,n=-1)$value,ncol=ncol(output.norm),byrow=TRUE) #data for denormalization
dbClearResult(res)

#normalize output
output <- normalize(output,output.norm)

end <- dbDisconnect(con)

input.get <- 'select input.value from input inner join test on input.simulation=test.simulation where input.info=:aa and test.test=0 order by input.simulation;'
input.norm.get <- 'select min,max from inputInfo where id=:aa'

for(i in 1:nrow(col)) {
  for(j in 1:ncol(col)) {
    if(col[i,j]!='R' & col[i,j]!='I' & !is.na(col[i,j])) { #ignore these tags
    
      if(type!='all') {
      
	#load data-pom
	regions.row <- which(regions[,j]==row[i]) #which rows are in appropriate region
	con <- dbConnect(drv, dbname = file.pom.db)
	data <- as.matrix(dbGetPreparedQuery(con,'select * from data where row_names=:aa',data.frame(aa=(regions.row)))) #load data
	data <- (data[,2:ncol(data)])
	data <- checkMatrix(data)
	end <- dbDisconnect(con)
	
	data.min <- apply(data,2,min)
	id <- which(data.min>=min.data & data.min<=max.data)
	
      } else {
	id <- c(1:ncol(output)) #use all samples
      }
      
      con <- dbConnect(drv, dbname = file.db)
      input <- as.matrix(classificationValues(matrix(dbGetPreparedQuery(con,input.get,data.frame(aa=regions.row))$value,ncol=length(regions.row)),soil.init[regions.row]))
      input.norm <- (as.matrix(classificationValues(t(as.matrix(dbGetPreparedQuery(con,input.norm.get,data.frame(aa=regions.row)))),soil.init[regions.row])))
      end <- dbDisconnect(con)
      
      input <- normalize(input,input.norm)
      
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
	cat(input[k,1]) #have only one col
      }
      #sink(type = "output") 
      sink(NULL)
      close(save.file)
    }
  }
}
