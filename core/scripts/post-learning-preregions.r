library(ncdf)
library(RSQLite)

args <- commandArgs(trailingOnly = TRUE)

file.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db' #used for denormalization of outputs
outputs.addr <- args[2] #'/home/trn/scratch/save' #predicted outputs in right places
func.addr <- args[3] #'/home/trn/diploma-thesis/software/multi-iteration/scripts'
init.add <- args[4] #'/home/trn/scratch'
regions.save.dir <- paste(init.add,'/regions.csv',sep="")
regions.dir <- paste(init.add,'/split-regions.rda',sep="")
save.addr <- args[5] #'/home/trn/scratch/save'
save.ncdf <- paste(save.addr,'/init-soil',sep="")
save.tests <- paste(save.addr,'/test',sep="")
#save.output <- paste(save.addr,'/output-regions.csv',sep="")
file.analysis.addr <- args[6] #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.csv'
file.info <- args[7] #'/home/trn/scratch/save/iteration-1-info-regions.txt'
iteration <- as.numeric(args[8]) #1 #number of iteration
testing <- as.numeric(args[9]) #1 #1 == true, 0 == false
method <- args[10] #'preregions-reg' #can be preregions-clas or preregions-reg
shorten <- as.numeric(args[11]) #1
variable <- "STOR_pop2"
num.samples <- as.numeric(args[12]) #5
min.regions <- as.numeric(args[13]) #10
max.regions <- as.numeric(args[14]) #100

file.init.addr <- args[15] #'/home/trn/Desktop/CMAQ/cmaq471_bap/init/AI219_pop_stor_1983_87.nc'
first.id.file <- as.numeric(args[16]) #100
  
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


drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = file.db)

#load denormalization data
res <- dbSendQuery(con,'select min,max from inputInfo order by id')
input.norm <- t(as.matrix(fetch(res,n=-1)))
dbClearResult(res)

if(method == 'preregions-clas') {
  load(regions.dir) #we obtain regions.splits variable
} 
regions <- trunc(seq(min.regions,max.regions,length.out=num.samples))

source(paste(func.addr,"/create-ncdf.r",sep=""))
source(paste(func.addr,"/post-learning-functions.r",sep=""))
source(paste(func.addr,"/samples-functions.r",sep=""))

post.len <- -1 #-1 is output, other numbers are tested samples (its simulation number)

if(testing==1) {
  res <- dbSendQuery(con,'select simulation from test where test=1 order by simulation')
  post.test <- as.vector(fetch(res,n=-1)$simulation)
  dbClearResult(res)
  post.len <- c(post.len,post.test) #add simulations for testing
}

data <- read.csv(file.analysis.addr)
water <- data$water
soil[which(water[1:length(soil)]==1)] <- 0 #ignore water

groupWater <- function(output,water) {
  output[which(water==1),] <- 0 #now, water is in one group
  return(output)
}

#removes unused regions
normalizeRegions <- function(output) {
  for(i in 1:ncol(output)) {
    output[,i] <- factor(output[,i],labels=c(1:length(table(output[,i]))))
  }
  return(output)
}

fillValues <- function(output,regions.split,soil) {
  for(i in 1:ncol(output)) {
    for(j in 2:length(regions.split[[i]])) {
      output[output[,i]==j,i] <- soil[output[,i]==j] + mean(c(regions.split[[i]][j-1],regions.split[[i]][j]))
    }
  }
  output[output==1] <- 0 #water is always in 1 group, set up to zero
  return(output)
}

createRegionOutput <- function(output,regions,input.norm,soil) {
  #input.diff <- input.norm[2,]-output
  #limits <- range(input.diff)
  #regions.splits <- sapply(regions,function(x) seq(limits[1],limits[2],length.out=x+1))
  limits <- c(min(output-soil),max(output-soil)) #use output so we are sure, that we don't cross limits
  regions.splits <- sapply(regions,function(x) c(as.vector(quantile(unique(output-soil),prob=seq(0,1,length.out=x+1)))))
  #regions.splits <- sapply(regions,function(x) seq(limits[1],limits[2],length.out=x+1))
  #regions.splits <- sapply(regions,function(x) seq(min(soil-input.norm[2,]),max(soil-input.norm[1,]),length.out=x+1))
  res <- matrix(0,nrow=nrow(output),ncol=length(regions.splits))
  for(i in 1:length(regions.splits)) {
    res[,i] <- createRegion(output-soil,regions.splits[[i]])
  }
  res[which(water==1),] <- 'I' #now, water is in one group]
  return(res)
}

spreadValues <- function(output,regions,soil) {
  res <- matrix(0,nrow=nrow(output),ncol=ncol(regions))
  for(i in 1:ncol(regions)) {
    for(j in 1:max(regions[,i])) {
      index <- which(regions[,i]==j)
      res[index,i] <- mean(output[index]-soil[index]) + soil[index]
    }
  }
  return(res)
}

for(i in 1:length(post.len)) {
  out <- tryCatch({
    if(i==1) {
      if(method == 'preregions-reg') {
	output <- as.matrix(read.csv(paste(outputs.addr,'/output.csv',sep=""))) #read outputs from post-learning.r
	output.region <- createRegionOutput(output,regions,input.norm,soil)
      } else {
	output.region <- readFile(outputs.addr,'output-preregions',iteration)
      }
    } else {
      if(method == 'preregions-reg') {
	 output <- as.matrix(read.csv(paste(outputs.addr,'/init-test-',post.len[i],'.csv',sep=""))) #read outputs from post-learning.r #they are already denormalized, etc
	 output.region <- createRegionOutput(output,regions,input.norm,soil)
      } else {
	 output.region <- readFile(outputs.addr,'test',iteration,post.len[i],'-preregions')
      }    
    }
    if(!is.matrix(output.region)) {
      output.region <- t(as.matrix(output.region))
    }
    
    #output.region <- polutateFileRegion(output.region,regions) #just for testing, turn off after testing!!!
    output.region <- groupWater(output.region,water) #now we have regions
    output.region <- matrix(as.numeric(output.region),nrow=nrow(output.region),ncol=ncol(output.region)) #change types to numeric
    output.region <- output.region+1 #now water is always 1
          
    if(i==1) {
      write.csv(normalizeRegions(output.region),regions.save.dir,row.names=FALSE)
      if(shorten==1) {
	if(method == 'preregions-clas') {
	  output <- fillValues(output.region,regions.splits,soil)
	} else {
	  output <- spreadValues(output,output.region,soil)
	  output[which(water[1:nrow(output)]==1),] <- 0
	}
	plotResult(output.regions=output.region,output=output,row,col,save.addr,post.len[i],iteration,end='-regions')
	saveMap(output,save.ncdf,first.id.file,soil.init,variable) #save as ncdf
      } else {
	plotResult(output.region,output=NULL,row,col,save.addr,post.len[i],iteration,end='-regions')
      }
    } else {
      if(method == 'preregions-clas') {
	output <- fillValues(output.region,regions.splits,soil)
      } else {
	output <- spreadValues(output,output.region,soil)
	output[which(water[1:nrow(output)]==1),] <- 0
      }
      #load original data
      input.test <- as.vector(dbGetPreparedQuery(con,'select value from input where simulation=:aa order by info',data.frame(aa=(post.len[i])))$value)
      #compare tests and original values
      infoTest(file.info,post.len[i],input.test,output)     
      #plot results
      plotTest(output,input.test,row,col,save.addr,post.len[i],iteration,end='-regions')
      write.csv(normalizeRegions(output.region),paste(save.tests,'-',post.len[i],'-regions.csv',sep=""),row.names=FALSE)
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