library(ncdf)

args <- commandArgs(trailingOnly = TRUE)

file.init.addr <- args[1]
file.analys.addr <- paste(file.init.addr,'/',args[2],sep="") #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.nc'
file.bap.addr <- paste(file.init.addr,'/',args[3],sep="") #'/home/trn/Documents/CMAQ/cmaq471_bap/init/AI219_pop_stor_1983_87.nc'
file.sfoc.addr <- paste(file.init.addr,'/',args[4],sep="") #'/home/trn/Documents/CMAQ/cmaq471_bap/init/resampled-organic-carbon-map.nc'
file.soil.addr <- paste(file.init.addr,'/',args[5],sep="") #'/home/trn/Documents/CMAQ/cmaq471_bap/init/resampled-soil-type-map.nc'
save.addr <- file.init.addr #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil'

file.analys <-  open.ncdf(file.analys.addr)
data <- data.frame(urban=as.vector(get.var.ncdf(file.analys, "urban")),
		   station=as.vector(get.var.ncdf(file.analys, "station")),
		   temperature=as.vector(get.var.ncdf(file.analys, "temperature")),
		   elevation=as.vector(get.var.ncdf(file.analys, "elevation")))
row <- get.var.ncdf(file.analys, "ROW")
col <- get.var.ncdf(file.analys, "COL")
row <- row[length(row)]
col <- col[length(col)]
dluse <- as.vector(get.var.ncdf(file.analys, "dluse"))#24
water <- as.vector(get.var.ncdf(file.analys, "water"))

file.bap <- open.ncdf(file.bap.addr)
data[,c("isoil")] <- as.vector(get.var.ncdf(file.bap, "STOR_pop2")[,,1])

file.sfoc <- open.ncdf(file.sfoc.addr)
data[,c("oc")] <- as.vector(get.var.ncdf(file.sfoc, "Sfoc"))

file.soil <- open.ncdf(file.soil.addr)
data[,c("tsoil")] <- as.vector(get.var.ncdf(file.soil, "soil"))

rows <- as.vector(sapply(1:col, function(x) rep(x,row)))
cols <- rep(1:row,col)
data[,c("row","col")] <- cbind(rows,cols)

tmp <- data
tmp[,c("dluse","water")] <- cbind(dluse,water)
write.csv(tmp,paste(save.addr,'/analysis.csv',sep=""),row.names=FALSE)
#simple analysis
#plot(data$x,data$isoil) #clear from map, bigger x, bigger concentrations
#plot(data$y,data$isoil) #clear from map, bigger x, bigger concentrations
#plot(data$isoil,data$urban) #not so nice U have expected, but can be seen that urban influence concetrations
#plot(data$isoil,data$oc) #not nice
#plot(data$isoil,data$tsoil) #not nice
#plot(data$isoil,data$dluse) #not really good
#plot(data$isoil,data$temperature) #smaller temperature, bigger concentrations
#plot(data$isoil,data$elevation) #lower places, bigger concetrations


reevaluate <- function(vector,x) {
  temp <- vector
  for(i in 1:(length(x)-1)) {
    temp[which(vector>=x[i] & vector<=x[i+1])] <- i
  }
  return(temp)
}

#we needs to revaluate urban, station, temperature, elevation, isoil, tsoil, oc, x, y
data.distribution <- list(urban=(seq(0, 1, 0.2)),
			  station=(seq(0, 1, 0.1)),
			  temperature=(seq(0, 1, 0.2)),
			  elevation=(seq(0, 1, 0.2)),
			  isoil=(seq(0, 1, 0.05)),
			  oc=(seq(0, 1, 0.1)),
			  tsoil=(seq(0, 1, 0.1)),
			  row=(seq(0, 1, 0.2)),
			  col=(seq(0, 1, 0.2)))

if(length(data.distribution)!=ncol(data)) {
  stop("Distributions of data are wrong")
}

pdf(paste(save.addr,'/revaluated-maps.pdf',sep=""), bg = "white")
for(i in 1:length(data.distribution)) {
  x <- quantile(data[,i], probs = data.distribution[[i]])
  data[,i] <- reevaluate(data[,i],x)
  image(matrix(data[,i],nrow=row,ncol=col),main=paste("Type of map:",colnames(data)[i]))
}


result <- rep(NA,row*col)
for(i in 1:nrow(data)) {
  result[i] <- paste(dluse[i],paste(data[i,],collapse=" "))
}

result[which(water==1)] <- 0 #100% of water => no land
x <- factor(result,labels=c(1:length(table(result))))
x <- as.numeric(as.vector(x))
image(matrix(x,nrow=row,ncol=col),col=terrain.colors(240),main="Map of regions")

dev.off()

write.csv(matrix(x,nrow=row,ncol=col),paste(save.addr,'/map-regions.csv',sep=""),row.names=FALSE)

