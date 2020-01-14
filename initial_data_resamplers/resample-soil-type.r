library(sp)
library(raster)
library(rgdal)
library(ncdf)

file.old.addr <- "/home/trn/Desktop/diploma-thesis/R/scripts/resample-organic/EUmap/All_grids_laea_plus/wrbfu/w001001.adf"
file.new.addr <- "/home/trn/Desktop/diploma-thesis/R/scripts/resample-organic/resampled-soil-type-map.nc" 

file.new.lat <- 121
file.new.lon <- 121
file.new.NA <- 1
file.new.varname <- "soil"

file.new.proj <- "+proj=lcc +lat_1=48 +lat_2=52 +lat_0=50 +lon_0=10" #values from griddesc
file.old.proj <- "+proj=laea +x_0=4321000 +y_0=3210000 +lat_0=52 +lon_0=10"
#file.old.extent <- new("Extent",xmin=-4321000,xmax=3179000,ymin=-3210000,ymax=2290000)
file.new.extent <- new("Extent",xmin=-1746000,xmax=2610000,ymin=-2142000,ymax=2214000) #computed from cmaq griddesc

template <- raster(ext=file.new.extent, nrow=file.new.lat,ncol=file.new.lon, crs=file.new.proj) #used sizes of CMAQ outputs #to

createNcdf <- function() {
  #setup dimensions
  dim1 <- dim.def.ncdf("row", "degrees_north", 1:file.new.lat)
  dim0 <- dim.def.ncdf("col", "degrees_east", 1:file.new.lon)
  mv <- 1.e30 # missing value to use !!! TAKEN FROM DOCUMENTATION OF ncdf
  vard <- list(var.def.ncdf(file.new.varname, "dominant soil type WRB", list(dim0,dim1),mv))
  file.new <<- create.ncdf(file.new.addr, vard) #want to use upstairs
}


x <- raster(file.old.addr, crs=file.old.proj)
xy <- projectRaster(x,template,method="bilinear",crs=file.new.proj)

soil <- matrix(xy@data@values,nrow=file.new.lat,ncol=file.new.lon,byrow=TRUE)
soil <- t(soil[nrow(soil):1,]) #flip it
#Sfoc <- Sfoc/100 #divide all values by 100 (it is in %)
soil[is.na(soil)] <- file.new.NA #change NA values to constant
#range(soil,na.rm=TRUE)
#image(soil)
soil
createNcdf()
put.var.ncdf(file.new,file.new.varname,soil)
close.ncdf(file.new)





