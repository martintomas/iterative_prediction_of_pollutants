library(sp)
library(raster)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ncdf)

test.plot <- FALSE

file.old.addr <- "/home/trn/Documents/OCmap/octop_insp_directory/octop_insp/w001001.adf"
file.old2.addr <- '/home/trn/Desktop/diploma-thesis/R/scripts/resample-organic/EUmap/All_grids_laea_plus/oc_top/w001001.adf'
file.water.addr <- '/home/trn/Documents/CMAQ/cmaq471_bap/data/mcip3/001/GRIDCRO2D_aa2006test'
file.new.addr <- "/home/trn/Desktop/diploma-thesis/R/scripts/resample-organic/resampled-organic-carbon-map.nc" 

file.new.lat <- 121
file.new.lon <- 121
file.new.NA <- 0.0125
file.new.varname <- "Sfoc"

file.new.proj <- "+proj=lcc +lat_1=48 +lat_2=52 +lat_0=50 +lon_0=10" #values from griddesc
file.old.proj <- "+proj=laea +x_0=4321000 +y_0=3210000 +lat_0=52 +lon_0=10"
#file.old.extent <- new("Extent",xmin=-4321000,xmax=3179000,ymin=-3210000,ymax=2290000)
file.new.extent <- new("Extent",xmin=-1746000,xmax=2610000,ymin=-2142000,ymax=2214000) #computed from cmaq griddesc

template <- raster(ext=file.new.extent, nrow=file.new.lat,ncol=file.new.lon, crs=file.new.proj) #used sizes of CMAQ outputs #to

prepareOutputMap <- function() {
  data(wrld_simpl)
  grid <- raster() 
  # define extent and crs
  extent(grid)<-new("Extent",xmin=-70,xmax=70,ymin=0,ymax=90)
  projection(grid) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  grid[] <- runif(1:ncell(grid))
  
  world <- spTransform(wrld_simpl, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  worldcrop<-crop(world, grid) #remove don't need values
  return(spTransform(worldcrop, CRS(file.new.proj))) #return transformed map
}

createNcdf <- function() {
  #setup dimensions
  dim1 <- dim.def.ncdf("row", "degrees_north", 1:file.new.lat)
  dim0 <- dim.def.ncdf("col", "degrees_east", 1:file.new.lon)
  mv <- 1.e30 # missing value to use !!! TAKEN FROM DOCUMENTATION OF ncdf
  vard <- list(var.def.ncdf(file.new.varname, "fractions of organic carbon", list(dim0,dim1),mv))
  file.new <<- create.ncdf(file.new.addr, vard) #want to use upstairs
}


x <- raster(file.old.addr, crs=file.old.proj)
xy <- projectRaster(x,template,method="bilinear",crs=file.new.proj)
x2 <- raster(file.old2.addr, crs=file.old.proj)
xy2 <- projectRaster(x2,template,method="bilinear",crs=file.new.proj)
file.water <-  open.ncdf(file.water.addr)
water <- get.var.ncdf(file.water,'LUFRAC_16')

Sfoc.big <- matrix(xy2@data@values,nrow=file.new.lat,ncol=file.new.lon,byrow=TRUE)
Sfoc.big <- t(Sfoc.big[nrow(Sfoc.big):1,]) #flip it
Sfoc.big <- Sfoc.big/100 #divide all values by 100 (it is in %)
Sfoc.big[which(water==0)] <- 0
Sfoc.big[is.na(Sfoc.big)] <- file.new.NA #change NA values to constant
#image(Sfoc.big)

Sfoc <- matrix(xy@data@values,nrow=file.new.lat,ncol=file.new.lon,byrow=TRUE)
Sfoc <- t(Sfoc[nrow(Sfoc):1,]) #flip it
Sfoc <- Sfoc/100 #divide all values by 100 (it is in %)
Sfoc[which(water==1)] <- 0
Sfoc[is.na(Sfoc)] <- file.new.NA #change NA values to constant
image(Sfoc,col=terrain.colors(200))


#plot data
if(test.plot) {
  map <- prepareOutputMap()
  plot(xy)
  plot(map, add=TRUE)
}


#image(Sfoc)
createNcdf()
put.var.ncdf(file.new,file.new.varname,Sfoc)
close.ncdf(file.new)

#test <-  open.ncdf(file.new)
#Sfoc <- get.var.ncdf(test, file.new.varname)
#values(template) <- Sfoc #matrix(xy@data@values,nrow=file.new.lat,ncol=file.new.lon, byrow=TRUE)
#plot(template)





