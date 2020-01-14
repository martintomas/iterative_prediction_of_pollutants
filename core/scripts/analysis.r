library(ncdf)

args <- commandArgs(trailingOnly = TRUE)

file.init.addr <- args[1]
file.grid.addr <- paste(file.init.addr,'/',args[2],sep="") #'/home/trn/Desktop/CMAQ/cmaq471_bap/data/mcip3/001/GRIDCRO2D_aa2006test'
file.stations.addr <- paste(file.init.addr,'/',args[3],sep="") #'/home/trn/Desktop/diploma-thesis/R/scripts/measurements/modified-measurements.rda'
file.temp.addr <- paste(file.init.addr,'/',args[4],sep="") #'/home/trn/Desktop/CMAQ/cmaq471_bap/data/mcip3/temp-mean'
file.save.addr <- paste(file.init.addr,'/',args[5],sep="") #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.nc'
func.addr <- args[6] #"/home/trn/diploma-thesis/R/scripts/volatilization-function"

size.max.urban <- 20 #max block dist
size.max.station <- 40 #max block dist

file <-  open.ncdf(file.grid.addr)
row <- get.var.ncdf(file, "ROW")
col <- get.var.ncdf(file, "COL")
row <- row[length(row)]
col <- col[length(col)]
lev <- 1
time <- 1

source(paste(func.addr,"/create-ncdf.r",sep=""))
variables <- matrix(NA, nrow=6,ncol=2)
variables[1,] <- c('urban', "distance to size of cities")
variables[2,] <- c('station', "distance to station")
variables[3,] <- c('temperature', "average temp")
variables[4,] <- c('dluse', "dominant land use")
variables[5,] <- c('elevation', "terrain elevation")
variables[6,] <- c('water', "twater mask")
file.save <- createNcdf(file.save.addr,col,row,lev,time,variables)

distance <- function(x1,y1,x2,y2) {
  return(sqrt(((x2-x1)^2)+((y2-y1)^2))) #euclidean distance in 2D
}

#URBAN
print('Urban analysis')
#dimensions are col, row
data.urban <- get.var.ncdf(file,'LUFRAC_01') #'DLUSE')#'LUFRAC_01')#[40:60,40:60] #LUFRAC_01 is better, but dluse is faster
#data.urban[which(data.urban!=1)] <- 0 #remove when you use LUFRAC_01
#image(data.urban)
urban <- which(data.urban!=0) #==1) #!=0)
urban.coord <- sapply(urban, function(x) {
  urban.col <- x%%col
  urban.row <- ceiling(x/row)
  if(urban.col==0) urban.col <- col
  return(c(urban.row,urban.col))
})
urban.coord <- data.frame(row=urban.coord[1,],col=urban.coord[2,])

#distance.max <- sqrt(row*col*2)+1 #max distance
dist.urban <- data.urban
for(i in 1:row) {
  for(j in 1:col) {
    #distances.sum <- 0
    #urban.values.sum <- data.urban[i,j]
    for(l in 1:nrow(urban.coord)) {     
      if( i<(urban.coord$row[l]+size.max.urban) & i>(urban.coord$row[l]-size.max.urban) & j<(urban.coord$col[l]+size.max.urban) & j>(urban.coord$col[l]-size.max.urban) & 
       (i!=urban.coord$row[l] & j!=urban.coord$col[l])) {
	dist <- (distance(i,j,urban.coord$row[l],urban.coord$col[l]))
	#distances.sum <- dist+distances.sum
	dist.urban[j,i] <- ((1/dist)*data.urban[urban.coord$col[l],urban.coord$row[l]])+dist.urban[j,i] #first is col, second is row
      }
    }
    #if(distances.sum!=0) {
    #dist.urban[i,j] <- urban.values.sum #/distances.sum
    #} #else {
    #  dist.urban[i,j] <- NA
    #}
  }
}
#range(dist.urban)
#hist(dist.urban)
#dist.urban[which(is.na(dist.urban))] <- min(dist.urban,na.rm =TRUE)-distance.step
put.var.ncdf(file.save,'urban',dist.urban)

#STATIONS
print('Station analysis')
load(file.stations.addr)
vystup[,c('coordinates')] <- paste(vystup$row,vystup$col)
vystup$coordinates <- factor(vystup$coordinates, labels=1:length(table(vystup$coordinates)))
vystup <- vystup[order(vystup$coordinates),]
dist.stations <- matrix(0, nrow=col,ncol=row)
for(l in levels(vystup$coordinates)) {
  data.stations <- vystup[which(vystup$coordinates==l),]
  dist.stations[data.stations$col[1],data.stations$row[1]] <- log(nrow(data.stations)) #!!keep it flipped
}
for(i in 1:row) {
  for(j in 1:col) {
    #distances.sum <- 0
    #count.stations <- 0
    for(l in levels(vystup$coordinates)) {
      data.stations <- vystup[which(vystup$coordinates==l),]
      if( i<(data.stations$row[1]+size.max.station) & i>(data.stations$row[1]-size.max.station) & j<(data.stations$col[1]+size.max.station) & j>(data.stations$col[1]-size.max.station) & 
       (i!=data.stations$row[1] & j!=data.stations$col[1])) {
	dist.stations[j,i] <- dist.stations[j,i]+(dist.stations[data.stations$col[1],data.stations$row[1]]*(1/distance(i,j,data.stations$row[1],data.stations$col[1])))
	#count.stations <- count.stations+1
      }
    }
    #if(count.stations!=0) {
    #  dist.stations[j,i] <- dist.stations[j,i]/count.stations
    #} #else {
    #  dist.stations[j,i] <- 0	
    #}
  }
}
#range(dist.stations)
put.var.ncdf(file.save,'station',dist.stations)
#image(dist.stations)

print('Preparing rest of the data')
#SOIL TYPE
data.soil <- get.var.ncdf(file, 'DLUSE')
put.var.ncdf(file.save,'dluse',data.soil)

#ELEVATION
data.elevation <- get.var.ncdf(file, 'HT')
put.var.ncdf(file.save,'elevation',data.elevation)
#image(data.elevation)

#TEMPERATURE
file.temp <-  open.ncdf(file.temp.addr)
temp <- get.var.ncdf(file.temp, 'temperature')#[40:60,40:60]
put.var.ncdf(file.save,'temperature',temp)

#WATER
data.water <- get.var.ncdf(file, 'LUFRAC_16')
put.var.ncdf(file.save,'water',data.water)

close.ncdf(file.save)
