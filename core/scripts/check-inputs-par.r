#check whetever enough initial files were generated in previous iteration with learning algorithm
#if not, it use random functions to generate missing ones

args <- commandArgs(trailingOnly = TRUE)

input.addr <- args[1] #'/home/trn/Desktop/diploma-thesis/data/soil/init-soil'
data.addr <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.csv'
func.addr <- args[3] #"/home/trn/diploma-thesis/R/scripts/init-soil"

from <- args[4] #80
to <- args[5] #101
iteration <- args[6] #1
cores <- args[7] #3
variable <- "STOR_pop2"
min.samples <- 14000
max.samples <- 14200
row <- NA
col <- NA
lev <- NA
time <- 1

files.names <- paste(input.addr,'_',from:to,'.nc',sep="")

file.exist <- file.exists(files.names)

if(any(file.exist==FALSE)) {
  print("Some of the input files for next iteration are missing!!! Generating missing ones with random functions.")
} else {
  print("Simulation part have enough soil input files.")
  quit('no')
}

library(Rmpi)

num.missing.files <- length(files.names[!file.exist])
if(num.missing.files<cores) {
  mpi.spawn.Rslaves(nslaves=num.missing.files)
} else {
  mpi.spawn.Rslaves(nslaves=cores)
}

# In case R exits unexpectedly, have it automatically clean up # resources taken up by Rmpi (slaves, memory, etc...) 
.Last <- function() { 
  if (is.loaded("mpi_initialize")){ 
    if (mpi.comm.size(1) > 0){ 
      print("Please use mpi.close.Rslaves() to close slaves.") 
      mpi.close.Rslaves() 
    } 
    print("Please use mpi.quit() to quit R") 
    .Call("mpi_finalize") 
  } 
}

slaves <- (mpi.comm.size()-1) #except of master
groups <- split(files.names[!file.exist], ceiling(1:num.missing.files/(num.missing.files/slaves)))

#source(paste(func.addr,"/create-ncdf.r",sep=""))
#source(paste(func.addr,"/random-init-soil.r",sep=""))

saveNcdf <- function(file,data) {
  variables <- matrix(NA, nrow=1,ncol=2)
  variables[1,] <- c(variable, "units")
  file.new <- createNcdf(file, col, row, lev, time, variables)
  put.var.ncdf(file.new,variable,data)
  close.ncdf(file.new)
}

computeInputs <- function() {
  for(i in files.missing) {
    file.init.addr <- sample(files.exist,1) #choose one from generated values => at least one file returned from learning part have to exist
    file.init <-  open.ncdf(file.init.addr)
    soil.init <- get.var.ncdf(file.init, variable)
    if(is.na(row)) {
      row <<- get.var.ncdf(file.init, "ROW")
      col <<- get.var.ncdf(file.init, "COL")
      lev <<- get.var.ncdf(file.init, "LAY")
      lev <<- lev[length(lev)]
      row <<- row[length(row)]
      col <<- col[length(col)]
      grd <- expand.grid(col=seq(from=1, to=row, by=1), row=seq(from=1, to=col, by=1))
      coordinates(grd) <- ~ row+col
      gridded(grd) <- TRUE
    }
    
    #image(soil.init[,,1],main=paste("Original data from: ",file.init.addr,sep=""),col=terrain.colors(140))
    
    num.na <- sample(min.samples:max.samples,1)
    data$isoil <- as.vector(soil.init[,,1])
    n <- randomKriging(data,grd,num.na,col,row)
    
    #image(n,main=paste("Generated file: ",i,", Kriging was used",sep=""),col=terrain.colors(140))
    
    soil.init[,,1] <- n #other layers are unchanged
    saveNcdf(i,soil.init)
  }
}

data <- read.csv(data.addr)

mpi.bcast.Robj2slave(saveNcdf)
mpi.bcast.Robj2slave(createNcdf)
mpi.bcast.Robj2slave(randomKriging)
mpi.bcast.Robj2slave(row)
mpi.bcast.Robj2slave(col)
mpi.bcast.Robj2slave(time)
mpi.bcast.Robj2slave(lev)
mpi.bcast.Robj2slave(groups)
mpi.bcast.Robj2slave(files.names)
mpi.bcast.Robj2slave(file.exist)
mpi.bcast.Robj2slave(min.samples)
mpi.bcast.Robj2slave(max.samples)
mpi.bcast.Robj2slave(computeInputs)
mpi.bcast.Robj2slave(func.addr)
mpi.bcast.Robj2slave(variable)
mpi.bcast.Robj2slave(data)
mpi.bcast.cmd(files.missing <- groups[[mpi.comm.rank()]])
mpi.bcast.cmd(files.exist <- files.names[file.exist])
mpi.bcast.cmd(source(paste(func.addr,"/create-ncdf.r",sep="")))
mpi.bcast.cmd(source(paste(func.addr,"/random-init-soil.r",sep="")))
mpi.bcast.cmd(library(sp))
mpi.bcast.cmd(library(gstat))
mpi.bcast.cmd(library(fields))
mpi.bcast.cmd(library(ncdf))
outputs <- mpi.remote.exec(computeInputs()) #find input matrix

#visualize outputs
pdf(paste(input.addr,'-par-missing-',iteration,'.pdf',sep=""), bg = "white")
for(i in files.names[!file.exist]) {
  file.init <-  open.ncdf(i)
  soil.init <- get.var.ncdf(file.init, variable)
  image(soil.init[,,1],main=paste("Computed value with kriging, file:\n",i,sep=""),col=terrain.colors(140))
}
dev.off()

mpi.close.Rslaves()
mpi.quit()