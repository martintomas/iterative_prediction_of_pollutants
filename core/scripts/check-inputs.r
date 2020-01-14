#check whetever enough initial files were generated in previous iteration with learning algorithm
#if not, it use random functions to generate missing ones

args <- commandArgs(trailingOnly = TRUE)

input.addr <- args[1] #'/home/trn/Desktop/diploma-thesis/data/soil/init-soil'
data.addr <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.csv'
func.addr <- args[3] #"/home/trn/diploma-thesis/R/scripts/init-soil"

from <- as.numeric(args[4]) #80
to <- as.numeric(args[5]) #101
iteration <- as.numeric(args[6]) #1
variable <- "STOR_pop2"
min.samples <- 14200
max.samples <- 14400
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

if(all((file.exist)==FALSE)) {
  print('All ncdf files are missing. Impossible to reconstruct some files!!')
  quit('no')
}

library(ncdf)
library(sp)
library(gstat)
library(fields)

num.missing.files <- length(files.names[!file.exist])

source(paste(func.addr,"/create-ncdf.r",sep=""))
source(paste(func.addr,"/random-init-soil.r",sep=""))

saveNcdf <- function(file,data) {
  variables <- matrix(NA, nrow=1,ncol=2)
  variables[1,] <- c(variable, "units")
  file.new <- createNcdf(file, col, row, lev, time, variables)
  put.var.ncdf(file.new,variable,data)
  close.ncdf(file.new)
}

computeInputs <- function(files.missing,files.exist) {
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
    
    image(soil.init[,,1],main=paste("Original data from:\n",file.init.addr,sep=""),col=terrain.colors(140))
    
    num.na <- sample(min.samples:max.samples,1)
    data$isoil <- as.vector(soil.init[,,1])
    n <- randomKriging(data,grd,num.na,col,row)
    
    image(n,main=paste("Generated file:\n",i,",\nKriging was used",sep=""),col=terrain.colors(140))
    
    soil.init[,,1] <- n #other layers are unchanged
    saveNcdf(i,soil.init)
  }
}

data <- read.csv(data.addr)

pdf(paste(input.addr,'-missing-',iteration,'.pdf',sep=""), bg = "white")

outputs <- computeInputs(files.names[!file.exist],files.names[file.exist]) #find input matrix

dev.off()
