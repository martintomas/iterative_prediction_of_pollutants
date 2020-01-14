library(ncdf)
library(sp)
library(gstat)
library(caret)
library(fields)

args <- commandArgs(trailingOnly = TRUE)

file.init.addr <- args[1]
file.init.soil.addr <- paste(file.init.addr,'/',args[2],sep="") #'/home/trn/Desktop/CMAQ/cmaq471_bap/init/AI219_pop_stor_1983_87.nc'
file.analysis.addr <-  paste(file.init.addr,'/',args[3],sep="") #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.csv'
save.dir <-  args[4] #'/home/trn/diploma-thesis/R/scripts/init-soil'
func.addr <- args[5] #"/home/trn/diploma-thesis/R/scripts/volatilization-function"
#save.dir.pdf <-  '/home/trn/diploma-thesis/R/scripts/init-soil/new-inputs.pdf'
samples <- as.numeric(args[6])
from <- as.numeric(args[7])

#how random is generation
variable <- "STOR_pop2"
max.dist <- 6
min.dist <- 3
min.samples <- 13500
max.samples <- 14500
theta <- 0.9 #for smoothing #0.65 for idw(could be more), 0.85 for lm,

file.init <-  open.ncdf(file.init.addr)
soil.init <- get.var.ncdf(file.init, variable)
row <- get.var.ncdf(file.init, "row")
col <- get.var.ncdf(file.init, "col")
lev <- get.var.ncdf(file.init, "lev")
lev <- lev[length(lev)]
row <- row[length(row)]
col <- col[length(col)]
time <- 1

data <- read.csv(file.analysis.addr)

source(paste(func.addr,"/create-ncdf.r",sep=""))
source(paste(func.addr,"/random-init-soil.r",sep=""))

saveNcdf <- function(number,data) {
  file.new <- createNcdf(paste(save.dir,'_',number,'.nc',sep=""), col, row, lev, time, variables)
  put.var.ncdf(file.new,variable,data)
  close.ncdf(file.new)
}

variables <- matrix(NA, nrow=1,ncol=2)
variables[1,] <- c(variable, "units")

pdf(paste(save.dir,'.pdf',sep=""), bg = "white")
image(soil.init[,,1],main="Original concentrations",col=terrain.colors(140))

#first file is original data
soil.init[which(soil.init<0)] <- 0 #no negative values are allowed
saveNcdf(from,soil.init)

#prepare constants
soil.new <- soil.init
soil.groups <- split(1:(samples-1), ceiling(1:(samples-1)/((samples-1)/4)))
#prepare grid
grd <- expand.grid(col=seq(from=1, to=row, by=1), row=seq(from=1, to=col, by=1))
coordinates(grd) <- ~ row+col
gridded(grd) <- TRUE

out <- tryCatch({
for(i in soil.groups[[1]]) { #idw
  
  num.na <- sample(min.samples:max.samples,1)
  n <- randomIDW(data,grd,num.na,col,row,max.dist,min.dist)
  
  image(n,main=paste("IDW: picture",i),col=terrain.colors(140))
  
  soil.new[,,1] <- n #other layers are unchanged
  saveNcdf((i+from),soil.new)
}
}, error = function(err) {
  print("IDW 1 is missing")
})

out <- tryCatch({
for(i in soil.groups[[2]]) {
  
  num.na <- sample(min.samples:max.samples,1)
  n <- randomLM(data,num.na,col,row)
  
  image(n,main=paste("LM: picture",i),col=terrain.colors(140))
  
  soil.new[,,1] <- n #other layers are unchanged
  saveNcdf((i+from),soil.new)
}
}, error = function(err) {
  print("LM is missing")
})

out <- tryCatch({
for(i in soil.groups[[3]]) {
  
  num.na <- sample(min.samples:max.samples,1)
  n <- randomBRNN(data,num.na,col,row)
  
  image(n,main=paste("BRNN: picture",i),col=terrain.colors(140))
  
  soil.new[,,1] <- n #other layers are unchanged
  saveNcdf((i+from),soil.new)
}
}, error = function(err) {
  print("BRNN is missing")
})

out <- tryCatch({
data.corr <- data #for idw
coordinates(data.corr) <- ~ row+col #for idw
for(i in soil.groups[[4]]) {
  
  num.na <- sample(min.samples:max.samples,1)
  n <- randomIDW2(data,data.corr,num.na,col,row,max.dist,min.dist)
  
  image(n,main=paste("IDW2: picture",i),col=terrain.colors(140))
  
  soil.new[,,1] <- n #other layers are unchanged
  saveNcdf((i+from),soil.new)
}
}, error = function(err) {
  print("IDW 2 is missing")
})

dev.off()

