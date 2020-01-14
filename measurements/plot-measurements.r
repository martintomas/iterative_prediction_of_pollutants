library(M3)

file.bap.addr <- '/home/trn/Desktop/diploma-thesis/R/scripts/measurements/BAP-mean.nc'
save.dir <-  '/home/trn/Desktop/diploma-thesis/R/scripts/measurements/plot-measurements.pdf'

date.from <- '2006-01-01'
date.to <- '2006-12-31'
multiply.by <- 1000 #how much should be measurements multiplied
days.num <- as.numeric((as.Date(date.to)-as.Date(date.from)),units="days")+1

measurements.addr <- load('/home/trn/Desktop/diploma-thesis/measurements/BAP.rda') #where are measurements saved
file.data <- get.M3.var(file.bap.addr,'BAP',ldatetime=as.Date(date.from),udatetime=as.Date(date.to)) #load data, dimensions are -> col,row,lay,time
file.data.copy <- file.data #save copy just to visualize locations
file.data.copy$data <- matrix(0,nrow=nrow(file.data$data),ncol=ncol(file.data$data)) #reset data, we need only coordinates
vystup[,c("x", "y")] <-  project.lonlat.to.M3(vystup$longitude,vystup$latitude,file=file.bap.addr)$coords #compute lcc coordinates
world.bds <- get.map.lines.M3.proj(file=file.bap.addr, database="world")$coords #get map (visualize map + location of measurement)
vystup$from[which(is.na(as.Date(vystup$from)))] <- NA #remove values with NA day
vystup$from[which(as.Date(vystup$from)>as.Date(date.to))] <- NA #remove values which start after our date.to
vystup.na <- na.omit(vystup) #omit NA values
vystup$to[which(as.Date(vystup$to)>as.Date(date.to))] <- date.to #shorten date to at measurements which cross our max date (date.to)

vystup.na[,c("row","col")] <- NA #save also matrix coordinates
name <- rep(NA,nrow(vystup.na))
for(i in 1:nrow(vystup.na)) { #compute coordinates at matrix
  file.subset <- var.subset(file.data, llx=vystup.na$x[i], urx=vystup.na$x[i],lly=vystup.na$y[i], ury=vystup.na$y[i],hz.strict=FALSE)
  vystup.na$row[i] <- file.subset$rows
  vystup.na$col[i] <- file.subset$cols
  x <- as.vector(vystup.na$nameCS.1[i])
  xx <- iconv(x, "CP1250", "UTF-8") #change encodings of names !!! platform dependent (this for LINUX)
  name[i] <- xx
}
vystup.na$nameCS.1 <- factor(name) #change enconding of names
vystup.na[,c('coordinates')] <- paste(vystup.na$row,vystup.na$col) #create one summary coordinates (used for ordering)
vystup.order <- vystup.na[order(vystup.na$coordinates,vystup.na$nameCS.1),] #order based on coordinages and names of locations, to keep measurements in same cell together, they are in one plot
vystup.order$coordinates <- factor(vystup.order$coordinates, labels=1:length(table(vystup.order$coordinates))) #factor coordinates
pdf.options(encoding = "CP1250") #encoding of pdf (for czech names)
pdf(save.dir, bg = "white",width=10 ,height=8)
for(i in levels(vystup.order$coordinates)) {
  data <- vystup.order[i==vystup.order$coordinates,] #select data for one cell (measurements)
  ccol <- data$col[1]
  rrow <- data$row[1]
  file.subset <- file.data$data[ccol,rrow,1,]*multiply.by #select data for one cell (cmaq outputs) & multiply cmaq outputs
  file.data.copy$data[ccol,rrow] <- 1 #show location on map (matrix with marked location)
  par(xpd=FALSE)
  image(file.data$x.cell.ctr, file.data$y.cell.ctr, file.data.copy$data, main=paste("Measured location:",rrow,'x',ccol),col=c('white','red')) #plot matrix with marked location
  file.data.copy$data[ccol,rrow] <- 0 #reset location (so we can use it next loop)
  lines(world.bds) #plot map, from mapdata library
  minp <- min(min(data$value,file.subset)) #min value (from cmaq outputs and measurements)
  maxp <- max(max(data$value,file.subset)) #max value (from cmaq outputs and measurements)
  date <- seq(from=as.Date(date.from),to=as.Date(date.to),by="month") #x axis (months)
  plot(date,seq(from=minp,to=maxp, length.out=length(date)), ylab="Values",xlab="Months", main=paste("Position:",rrow,'x',ccol),type="n",xaxt = "n") #prepare plot for cmaq output
  axis(1, at = date, labels = format(date, "%Y-%m"))
  lines(seq(from=as.Date(date.from),to=as.Date(date.to),by=1),file.subset, col='black') #plot cmaq outputs
  name.tab <- table(data$nameCS.1) #obtain names of all locations in one cell
  name <- names(name.tab[name.tab>0])
  col <- rainbow(length(name)) #every location have different color
  data[,c('color')] <- factor(data$nameCS.1,labels=col) #create factor
  for(j in 1:nrow(data)) {
    if(as.Date(data$from[j])==as.Date(data$to[j])) {
      points(as.Date(data$from[j]),data$value[j],col=(as.vector(data$color[j])))     #plot active measurement 
    } else {
      lines(c(as.Date(data$from[j]),as.Date(data$to[j])),rep(data$value[j],2),col=(as.vector(data$color[j]))) #plot passive measurement
      if(j>1) {
	if((as.Date(data$from[j])==as.Date(data$to[j-1])) & data$color[j]==data$color[j-1] & (as.Date(data$from[j-1])!=as.Date(data$to[j-1]))) {
	  lines(c(as.Date(data$from[j]),as.Date(data$to[j-1])),c(data$value[j],data$value[j-1]),col=(as.vector(data$color[j]))) #connect two passive measurements together if they are after each other and share same location
	}
      }
    }
  }
  #add legend
  par(xpd=TRUE)
  legend("topright", inset=c(0.05,0), legend=c('CMAQ',name) , cex=0.8, col=c('black',col),lty=1) #add legend
}

dev.off()

