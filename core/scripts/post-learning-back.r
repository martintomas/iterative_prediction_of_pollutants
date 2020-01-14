library(ncdf)
library(RSQLite)

args <- commandArgs(trailingOnly = TRUE)

file.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db' #used for denormalization of outputs
outputs.addr <- args[2] #'/home/trn/scratch/save' #predicted outputs in right places
func.addr <- args[3] #'/home/trn/diploma-thesis/software/multi-iteration/scripts'
save.addr <- args[4] #'/home/trn/scratch/save'
save.ncdf <- paste(save.addr,'/init-soil',sep="")
save.tests <- paste(save.addr,'/test',sep="")
file.analysis.addr <- args[5] #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.csv'
file.init.addr <- args[6] #'/home/trn/Desktop/CMAQ/cmaq471_bap/init/AI219_pop_stor_1983_87.nc'
file.info <- args[7] #'/home/trn/scratch/save/iteration-0-info-zaloha.txt'
iteration <- as.numeric(args[8]) #1 #number of iteration
testing <- as.numeric(args[9]) #1 #1 == true, 0 == false
first.id.file <- as.numeric(args[10]) #100
last.id.file <- as.numeric(args[11]) #104
variable <- "STOR_pop2"
method <- args[12] #'back' #can be cell or region

num.files <- last.id.file-first.id.file+1

if(method=='back') {
  file.pom.db <- args[13] #'/home/trn/scratch/pom-data-back.db'
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
res <- dbSendQuery(con,'select min,max from measurement order by id')
output.norm <- t(as.matrix(fetch(res,n=-1)))
dbClearResult(res)

#load measurements
res <- dbSendQuery(con,'select value from measurement order by id')
measurement <- as.vector(fetch(res,n=-1)$value)
dbClearResult(res)

post.len <- -1 #-1 is output, other numbers are tested samples (its simulation number)

if(testing==1) {
  res <- dbSendQuery(con,'select simulation from test where test=1 order by simulation')
  post.test <- as.vector(fetch(res,n=-1)$simulation)
  dbClearResult(res)
  post.len <- c(post.len,post.test) #add simulations for testing
}

dbDisconnect(con)

#load data
data <- read.csv(file.analysis.addr)
max.value <- max(data$isoil) #used as max border
water <- data$water

for(i in 1:length(post.len)) {
  out <- tryCatch({
    if(i==1) {
      output <- readFile(outputs.addr,'output-back',iteration)
    } else {
      output <- readFile(outputs.addr,'test',iteration,post.len[i],'-back')
    }
    #output <- polutateFile(output,TRUE) #just for testing, turn off after testing!!!
    output <- denormalizeMap(output,output.norm[,1:nrow(output)])
    
    samples <- order(compare(output,measurement))[1:num.files] #order based on distance
    
    con <- dbConnect(drv, dbname = file.pom.db)
    output <- matrix(dbGetPreparedQuery(con,'SELECT value from input where simulation=:aa and sample=:bb order by id',data.frame(aa=post.len[i],bb=samples))$value,ncol=length(samples))
    #output <- matrix(unlist(dbGetPreparedQuery(con,'SELECT * from input where simulation=:aa and sample=:bb',data.frame(aa=post.len[i],bb=samples))),ncol=length(samples),byrow=TRUE)
    #output <- output[3:nrow(output),] #remove info about simulation and sample
    dbDisconnect(con)
    
    output <- resizeOutput(output,row,col)
    #image(matrix(output[,4],nrow=row,ncol=col))
    if(i==1) {
      saveMap(output,save.ncdf,first.id.file,soil.init,variable) #save as ncdf
    } else {
      #load original data
      con <- dbConnect(drv, dbname = file.db)
      input.test <- as.vector(dbGetPreparedQuery(con,'select value from input where simulation=:aa order by info',data.frame(aa=(post.len[i])))$value)
      dbDisconnect(con)
      #compare tests and original values
      infoTest(file.info,post.len[i],input.test,output)     
      #plot results
      plotTest(output,input.test,row,col,save.addr,post.len[i],iteration,'-back')
      write.csv(output,paste(save.tests,'-',post.len[i],'-back.csv',sep=""),row.names=FALSE)
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