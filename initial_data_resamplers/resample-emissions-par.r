library(sp)
library(rgdal)
library(raster)
library(ncdf)
#library(M3)
library(Rmpi)
#library(aqfig)

file.old.addr <- "/storage/jihlava1-cerit/home/christos/LCC12/SMOKE-EU_AAAPB10_cb05_LCC12" #.2006001"
file.new.addr <- "/storage/brno2/home/martintomas/resampled-emission/ren_POA36" #_001" 
save.addr <- '/storage/brno2/home/martintomas/resampled-emission'
date.from <- as.Date('2006-01-01')
date.to <- as.Date('2006-12-31')
row <- 121
col <- 121

days.seq <- seq(from=date.from,to=date.to,by='days') #vector of days, format %Y-%m-%d
old.days <- format(days.seq, "%Y%j") #vector of days, format %Y%j
new.days <- format(days.seq, "%j") #vector of days, format %Y%j

files.old <- paste(file.old.addr,'.',old.days,sep="")
files.new <- paste(file.new.addr,'_',new.days,sep="")

mpi.spawn.Rslaves(nslaves=3) #start slaves
slaves <- (mpi.comm.size()-1) #except of master

groups.old <- split(files.old, ceiling(1:length(files.old)/(length(files.old)/slaves)))
groups.new <- split(files.new, ceiling(1:length(files.new)/(length(files.new)/slaves)))

#world.old.bds <- get.map.lines.M3.proj(file=file.old.addr, database="world")$coords
#world.new.bds <- get.map.lines.M3.proj(file=file.new.addr, database="world")$coords


resampleGroup <- function() {
  for(m in 1:length(group.old)) {
    file.old <-  open.ncdf(group.old[m])
    file.new <- open.ncdf(group.new[m], write=TRUE)
    resampleOneFile(file.old,file.new)
  }
  return('OK')
}

resampleOneFile <- function(file.old,file.new) {
  #prepare variables
  lay <- max(get.var.ncdf(file.old,"LAY"))
  time <- max(get.var.ncdf(file.old,"TSTEP"))
  row.old <- max(get.var.ncdf(file.old,"ROW"))
  col.old <- max(get.var.ncdf(file.old,"COL"))
  list.of.variables <- names(file.old$var)
  ignored.variables <- c('TFLAG')

  file.old.proj <- "+proj=lcc +lat_1=48 +lat_2=52 +lat_0=50 +lon_0=10"
  file.new.proj <- "+proj=lcc +lat_1=48 +lat_2=52 +lat_0=50 +lon_0=10" #values from griddesc
  file.old.extent <- new("Extent",xmin=-1714000,xmax=2570000,ymin=-2125000,ymax=2159000)
  file.new.extent <- new("Extent",xmin=-1746000,xmax=2610000,ymin=-2142000,ymax=2214000) #computed from cmaq griddesc

  template <- raster(ext=file.new.extent, nrow=col,ncol=row, crs=file.new.proj) #used sizes of CMAQ outputs #to
  x <- raster(ext=file.old.extent, crs=file.old.proj ,ncol=row.old,nrow=col.old) #from
  col.rng <- terrain.colors(100)
  
  for(i in list.of.variables) {
    print(paste('Changing variable',i))
    if(i %in% ignored.variables) {
      print(paste('Variable',i,'is ignored'))
    } else {
      if(i=='POA') {
	var.out <- 'POC'
      } else {
	var.out <- toupper(i)
      }
      data.old <- get.var.ncdf(file.old,i) 
      #file.old.data <- get.M3.var(file.old.addr,i)
      #file.new.data <- get.M3.var(file.new.addr,var.out)
      data.new <- array(0,dim=c(col,row,lay,time))
      #info <- matrix(0,ncol=5,nrow=(lay*time))
      #pdf(paste(save.addr,'/resampled-emission-',i,'.pdf',sep=''), bg = "white",width=10)
      for(j in 1:lay) {
	print(paste('Computing layer',j,'variable',i))
	for(l in 1:time) {
	  print(paste('Computing time',l,'layer',j,'variable',i))
	  #print(dim(data.old))
	  #z.rng <- range(as.vector(data.old[,,j,l]))
	  #image(file.old.data$x.cell.ctr,file.old.data$y.cell.ctr,data.old[,,j,l],main=paste('Old data, time',l,'layer',j),col=col.rng)
	  #lines(world.old.bds)
	  #vertical.image.legend(zlim=z.rng, col=col.rng)
	  #x <- raster(data.old[,,j,l], crs=file.old.proj,ext=file.old.extent)
	  values(x) <- data.old[,,j,l]
	  xy <- projectRaster(x,template,method="bilinear",crs=file.new.proj,ext=file.new.extent)
	  data.new[,,j,l] <- matrix(xy@data@values,nrow=col,ncol=row,byrow=TRUE)
	  data.new[which(is.na(data.new)) | which(data.new < 0)] <- 1e-15
	  #z.rng <- range(as.vector(data.new[,,j,l]))
	  #image(file.new.data$x.cell.ctr,file.new.data$y.cell.ctr,data.new[,,j,l],main=paste('New data, time',l,'layer',j),col=col.rng)
	  #lines(world.new.bds)     	
	  #vertical.image.legend(zlim=z.rng, col=col.rng)
	  #info preparetion
	  #info[((j-1)*lay)+l,1] <- paste('Info for: time',l,'layer',j,'variable',i)
	  #info[((j-1)*lay)+l,2] <- paste('Old mean',mean(data.old[,,j,l]))
	  #info[((j-1)*lay)+l,3] <- paste('New mean',mean(data.new[,,j,l],na.rm=TRUE))
	  #info[((j-1)*lay)+l,4] <- paste('Old sum',sum(data.old[,,j,l]))
	  #info[((j-1)*lay)+l,5] <- paste('New sum',sum(data.new[,,j,l],na.rm=TRUE))
	  #print(info)
	}
      }
      #write.csv(info,paste(save.addr,'/resampled-emission-info-',i,'.csv',sep=''),row.names=FALSE)
      put.var.ncdf(file.new,var.out,data.new)
      #dev.off()
    }
  }
}

mpi.bcast.Robj2slave(row)
mpi.bcast.Robj2slave(col)
mpi.bcast.Robj2slave(groups.old)
mpi.bcast.Robj2slave(groups.new)
mpi.bcast.Robj2slave(resampleGroup)
mpi.bcast.Robj2slave(resampleOneFile)
mpi.bcast.Robj2slave(save.addr)
mpi.bcast.cmd(group.old <- groups.old[[mpi.comm.rank()]])
mpi.bcast.cmd(group.new <- groups.new[[mpi.comm.rank()]])
mpi.bcast.cmd(library(sp))
mpi.bcast.cmd(library(rgdal))
mpi.bcast.cmd(library(raster))
mpi.bcast.cmd(library(ncdf))
res <- mpi.remote.exec(resampleGroup())
print(res)

mpi.close.Rslaves()
mpi.quit()

