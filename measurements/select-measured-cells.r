library(M3)

file.addr <-  '/home/trn/Desktop/diploma-thesis/R/scripts/measurements/BAP-mean.nc' #'/storage/brno2/home/martintomas/BAP-data/jana-sum/all-emissions-BAP.nc' #where is file with data saved
save.addr <- '/home/trn/diploma-thesis/R/scripts/measurements/modified-measurements'
func.addr <- '/home/trn/diploma-thesis/R/scripts/measurements' #where is create-ncdf & clear measurements files script
file.variable <- 'BAP'
date.from <- '2006-01-01'
date.to <- '2006-12-31'

load('/home/trn/Desktop/diploma-thesis/measurements/BAP.rda')
source(paste(func.addr,"/clear-measurements.r",sep=""))

# observations <- data.frame(longitude=c(16.6333333,13.4,18.06491,-0.128005,21.0122287,-3.7037901,12.49424859), #for testing, Brno, Berlin, Stockholm, London, Warsava, Madrid, Rome lat/lon used
# 			   latitude=c(49.2,52.5166667,59.32893,51.5081289,52.2296756,40.4167754,41.8905198)) 
# 
file.data <- get.M3.var(file.addr,file.variable,ldatetime=as.Date(date.from),udatetime=as.Date(date.to)) #load data, dimensions are -> col,row,lay,time

vystup[,c("x", "y")] <-  project.lonlat.to.M3(vystup$longitude,vystup$latitude,file=file.addr)$coords #compute lcc coordinates
#file.selected <- matrix(NA, nrow=nrow(observations),ncol=length(file.data$datetime)) #prepare matrix, rows are measured cells, cols are hours
vystup[,c("row","col")] <- NA #save also matrix coordinates
name <- rep(NA,nrow(vystup))
for(i in 1:nrow(vystup)) {
  file.subset <- var.subset(file.data, llx=vystup$x[i], urx=vystup$x[i],lly=vystup$y[i], ury=vystup$y[i],hz.strict=FALSE)
  #file.selected[i,] <- as.vector(file.subset$data)
  vystup$row[i] <- file.subset$rows
  vystup$col[i] <- file.subset$cols
  x <- as.vector(vystup$nameCS.1[i])
  xx <- iconv(x, "CP1250", "UTF-8") #change encodings of names !!! platform dependent (this for LINUX)
  name[i] <- xx
}
vystup$nameCS.1 <- factor(name)
#file.selected #!!! here is result

#if you want to save observations frame
#write.csv(observations,'/home/trn/Desktop/diploma-thesis/R/scripts/measurements/observations.csv',row.names=FALSE)

vystup$from[which(is.na(as.Date(vystup$from)))] <- NA #remove values with NA day
vystup$from[which(as.Date(vystup$from)>as.Date(date.to))] <- NA #remove values which start after our date.to
vystup <- na.omit(vystup) #omit NA values
vystup$to[which(as.Date(vystup$to)>as.Date(date.to))] <- date.to #shorten date to at measurements which cross our max date (date.to)
save(vystup,file=paste(save.addr,'.rda',sep=""))
write.csv(vystup,paste(save.addr,'.csv',sep=""),row.names=FALSE)

#vystup <- cleanMeasurements(vystup)
#save(vystup,file=paste(save.addr,'-cleared.rda',sep=""))

#testing
# world.bds <- get.map.lines.M3.proj(file=file.addr, database="world")$coords
# for(i in 1:nrow(vystup)) {
#   file.data$data[vystup$col[i],vystup$row[i],1,1] <- 1000
# }
# image(file.data$x.cell.ctr, file.data$y.cell.ctr, file.data$data[,,1,1])
# lines(world.bds)