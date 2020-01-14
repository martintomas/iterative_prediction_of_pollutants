library(ncdf)
library(sp)
library(raster)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)

test.graphics <- TRUE
test.toFile <- TRUE

#Rprof()
file.old.addr <- "/home/trn/Dropbox/ForMartin/multicompartment_CMAQ/AI219_pop_stor_1983_87.nc"
file.new.addr <- "/home/trn/Desktop/CMAQ/cmaq471_bap/init/AI219_pop_stor_1983_87.nc"

variables <- list("STOR_pop2","STOR_pop5","STOR_pop8")
file.old <- open.ncdf(file.old.addr)
file.new.lon.max <- 121
file.new.lat.max <- 121
file.new.levels <- c(1,2,3) #length have to be 2 and more (functions works with 3D matrix)
file.new.levels.row <- c(1:length(file.new.levels)) #used for work

#based on information found here: https://bitbucket.org/tlroche/aqmeii-na_n2o/wiki/AQMEII-NA_spatial_domain#!griddesc
file.new.proj <- "+proj=lcc +lat_1=48 +lat_2=52 +lat_0=50 +lon_0=10" #values from griddesc
file.old.proj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
file.old.extent <- new("Extent",xmin=-180.40625,xmax=178.5938,ymin=-89.25846,ymax=89.25846)
#file.old.extent <- new("Extent",xmin=-180,xmax=180,ymin=-90,ymax=90)
file.new.extent <- new("Extent",xmin=-1746000,xmax=2610000,ymin=-2142000,ymax=2214000) #computed from cmaq griddesc

template <- raster(ext=file.new.extent, nrow=file.new.lat.max,ncol=file.new.lon.max, crs=file.new.proj) #used sizes of CMAQ outputs #to

inputModification <- function(data) {
  data2 <- matrix(0,nrow=length(file.old.lat),ncol=length(file.old.lon))
  half <- length(file.old.lon)/2
  data2[,1:half] <- data[,(half+1):length(file.old.lon)]
  data2[,(half+1):length(file.old.lon)]<- data[,1:(half)]
  return(data2)
}

#returns vector with number of columns
resampleLon <- function(x,newSize,start=0) {
  if(controlVectorSize(x,"longitude")==1)
  #last <- x[length(x)]
  #stepOld <- x[2]-x[1]
  #stepNew <- (last+stepOld)/newSize
  #return(seq(from=x[1],to=(last+stepOld)-stepNew,by=stepNew)) #dont make last step
  x <- c(1:file.new.lon.max)
  return(x)
}

#returns vector with number of rows
resampleLat <- function(x,newSize) { #??? don't know how to compute latitude
  stopifnot(controlVectorSize(x,"latitude")==1)
  x <- c(1:file.new.lat.max) 
  return(x)
}

controlVectorSize <- function(x, name) {
  if(length(x)<2) {
    print(paste("vector:",name,"have to have at least two values"))
    return(0)
  }
  return(1)
}

resampleData <- function(data) {
  print("resampling")
  
  x <- raster(ext=file.old.extent, crs=file.old.proj ,ncol=length(file.old.lon),nrow=length(file.old.lat)) #from
  values(x) <- inputModification(t(data)) # use transpositon => longtitude col, latitude rows
  xy <- projectRaster(x,template,method="bilinear",crs=file.new.proj)  
  xy@crs <- CRS(file.new.proj)
  
  #visualisation - FOR TESTING
  if(test.graphics) {
    createGraphs(x,xy)
  }
  
  return(xy)
}

getMatrix <- function(resampledData) {
  soil <- (matrix(nrow=file.new.lat.max,ncol=file.new.lon.max,data=resampledData@data@values,byrow=TRUE))
  return(t(soil[nrow(soil):1,])) #flip it
}

createNcdf <- function(names) {
  #setup dimensions
  dim3 <- dim.def.ncdf("time","time",19871231.75)
  dim2 <- dim.def.ncdf("lev","levels",file.new.levels.row)
  dim1 <- dim.def.ncdf("row", "degrees_north", file.new.lat)
  dim0 <- dim.def.ncdf("col", "degrees_east", file.new.lon)
  mv <- 1.e30 # missing value to use !!! TAKEN FROM DOCUMENTATION OF ncdf
  vard <- list()
  for(i in 1:length(names)) {
    vard[[i]] <- var.def.ncdf(names[[i]], "units", list(dim0,dim1,dim2,dim3),mv)
  }
  file.new <<- create.ncdf(file.new.addr, vard) #want to use upstairs
}

createGraphs <- function(input,output) {
  plot(input,main="original data")
  #map("world2Hires", add=TRUE)
  map("world", add=TRUE)
  
  plot(output,main="output data")
  plot(test.map, add=TRUE)
}

prepareOutputMap <- function() {
  grid <- raster()
  # define extent and crs
  extent(grid)<-new("Extent",xmin=-70,xmax=70,ymin=0,ymax=90)
  projection(grid) <- file.old.proj
  grid[] <- runif(1:ncell(grid))
  
  world <- spTransform(wrld_simpl, CRS(file.old.proj))
  worldcrop<-crop(world, grid) #remove don't need values
  return(spTransform(worldcrop, CRS(file.new.proj))) #return transformed map
}


#sizes of matrix
file.old.lon  <- get.var.ncdf(file.old,"lon")
file.old.lat  <- get.var.ncdf(file.old,"lat")

#resample lon & lat
file.new.lon <- resampleLon(file.old.lon,file.old.lon.max)
file.new.lat <- resampleLat(file.old.lat,file.old.lat.max)

#create netcdf fileInput
createNcdf(variables)

if(test.graphics) {
  data(wrld_simpl)
  test.map <- prepareOutputMap()
}

lapply(variables, function(nameVar) {
  stopifnot(controlVectorSize(file.new.levels,"levels")==1)
  
  if(test.toFile) {
    pdf(paste(nameVar, ".pdf", sep = ""), width = 8, height = 8 )
  }
  if(test.graphics) {
    nf <- layout( matrix( file.new.levels.row, length(file.new.levels.row), 1, byrow = TRUE), respect=FALSE )
  }
  
  #data
  file.old.data <- get.var.ncdf(file.old,nameVar)
  file.old.data <- file.old.data[,,file.new.levels] # levels have to be length(levels)>=2 !!! otherwise it will break computation
  
  #create raster object & resample
  resampledData <- apply(file.old.data,3,resampleData) #the most time consuming part
  file.new.data <- sapply(resampledData,getMatrix)
  file.new.data <-array(file.new.data,dim=c(file.new.lat.max,file.new.lon.max,file.new.levels.row[length(file.new.levels.row)]))
  file.new.data[which(file.new.data<0)] <- 0 
  
  #add to netcdf
  put.var.ncdf(file.new,nameVar,file.new.data)
  
  if(test.toFile) {
    dev.off()
  }
  
})

close.ncdf(file.old)
close.ncdf(file.new)
#summaryRprof()
