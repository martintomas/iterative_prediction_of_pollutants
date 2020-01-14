library(ncdf)
library(RSQLite)
library(gstat)
library(sp)

args <- commandArgs(trailingOnly = TRUE)

file.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db' #used for denormalization of inputs
outputs.addr <- args[2] #'/home/trn/scratch/save' #predicted inputs in right places
func.addr <- args[3] #'/home/trn/diploma-thesis/software/multi-iteration/scripts'
save.addr <- args[4] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning'
save.ncdf <- paste(save.addr,'/init-soil',sep="")
save.tests <- paste(save.addr,'/init-test',sep="")
save.output <- paste(save.addr,'/output.csv',sep="")
file.analysis.addr <- args[5] #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.csv'
file.init.addr <- args[6] #'/home/trn/Desktop/CMAQ/cmaq471_bap/init/AI219_pop_stor_1983_87.nc'
file.info <- args[7] #'/home/trn/scratch/save/iteration-0-info-zaloha.txt'
iteration <- as.numeric(args[8]) #0 #number of iteration
testing <- as.numeric(args[9]) #1 #1 == true, 0 == false
first.id.file <- as.numeric(args[10]) #100
variable <- "STOR_pop2"
idw.distance <- 10
method <- args[11] #'region' #can be cell or region

if(method=='region' | method=='regions') {
  file.region <- args[12] #'/home/trn/Desktop/diploma-thesis/software/multi-iteration/init/map-regions.csv'
  regions <- as.matrix(read.csv(file.region))
}

source(paste(func.addr,"/create-ncdf.r",sep=""))
source(paste(func.addr,"/post-learning-functions.r",sep=""))

file.init <-  open.ncdf(file.init.addr)
soil.init <- get.var.ncdf(file.init, variable)
row <- get.var.ncdf(file.init, "row")
col <- get.var.ncdf(file.init, "col")
lev <- get.var.ncdf(file.init, "lev")
lev <- lev[length(lev)]
row <- row[length(row)]
col <- col[length(col)]
time <- 1

#load denormalization data
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = file.db)
res <- dbSendQuery(con,'select min,max from inputInfo order by id')
input.norm <- t(as.matrix(fetch(res,n=-1)))
dbClearResult(res)

post.len <- 0 #0 is output, other numbers are tested samples (its simulation number)

if(testing==1) {
  res <- dbSendQuery(con,'select simulation from test where test=1 order by simulation')
  post.test <- as.vector(fetch(res,n=-1)$simulation)
  dbClearResult(res)
  post.len <- c(post.len,post.test) #add simulations for testing
}

#load data
data <- read.csv(file.analysis.addr)
max.value <- max(data$isoil) #used as max border
water <- data$water

if(method=='region' | method=='regions'){
  water.regio <- rep(0,max(regions))
  water.regio[1] <- 1 #region 1 is water
  soil <- as.vector(soil.init[,,1])
  if(method=='region') {
    input.norm <- regionNorm(input.norm,soil,regions)
  } else if(method=='regions') {
    input.norm.res <- list()
    for(k in 1:ncol(regions)) {
      input.norm.res[[k]] <- regionNorm(input.norm,soil,regions[,k])
    }
    max.regions <- apply(regions,2,max)
  } 
}

for(i in 1:length(post.len)) {
  out <- tryCatch({
    if(i==1) {
      output <- readFile(outputs.addr,'output',iteration)
    } else {
      output <- readFile(outputs.addr,'test',iteration,post.len[i])
    }
    #output <- polutateFile(output) #just for testing, turn off after testing!!!
    output <- removeR(output)
    
    if(method=='cell') {
      output <- removeI(output,water[1:nrow(output)])  
      output <- denormalizeMap(output,input.norm[,1:nrow(output)])
    } else if(method=='region') {
      if(i==1) {
	regions[regions>nrow(output)] <- 0 #remove regions which are not in map
      }
      output <- removeI(output,water.regio[1:nrow(output)]) 
      output <- denormalizeMap(output,input.norm[,1:nrow(output)])
      output <- deregionMap(output,soil,regions)
    } else if(method=='regions') {
      output <- removeI(output,water.regio[1:nrow(output)]) 
      output.copy <- matrix(NA,nrow=nrow(regions),ncol=ncol(regions))
      for(j in 1:length(input.norm.res)) {
	output.temp <- denormalizeMap(t(as.matrix(output[1:max.regions[j],j])),input.norm.res[[j]])
	output.copy[,j] <- deregionMap(t(output.temp),soil[1:nrow(regions)],regions[,j])
      }	  
      output <- output.copy
    }
         
    output <- removeI(output,water[1:nrow(output)]) # set up water to 0 once more (just to be sure, that denormalization doesn't changed results)    
    
    output <- matrix(as.numeric(output),nrow=nrow(output),ncol=ncol(output)) #change types to numeric
    output <- applyBorders(output,0,max.value)

    output <- fillMissingValues(data,output,idw.distance)
    #output[which(is.na(output))] <- 0
    
    output <- resizeOutput(output,row,col)
    #image(matrix(output[,4],nrow=row,ncol=col))
    if(i==1) {
      saveMap(output,save.ncdf,first.id.file,soil.init,variable) #save as ncdf
      write.csv(output,save.output,row.names=FALSE)
    } else {
      #load original data
      input.test <- as.vector(dbGetPreparedQuery(con,'select value from input where simulation=:aa order by info',data.frame(aa=(post.len[i])))$value)
      #compare tests and original values
      infoTest(file.info,post.len[i],input.test,output)     
      #plot results
      plotTest(output,input.test,row,col,save.addr,post.len[i],iteration)
      write.csv(output,paste(save.tests,'-',post.len[i],'.csv',sep=""),row.names=FALSE)
    }
  }, error = function(err) {
    print(err)
    print('Learning sample is missing or corrupted')
    if(i==1) {
      print('Serious mistake during post process learning, Learning is not working properly!!!')
    } else {
      print(paste('Testing sample',post.len[i],'is missing!'))
    }
  })
}

dbDisconnect(con)