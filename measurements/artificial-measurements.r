library(ncdf)

output.addr <- '/home/trn/Desktop/diploma-thesis/data'
save.addr <- '/home/trn/diploma-thesis/R/scripts/measurements/artificial-measurements3' #name of file without end (.csv or .rda)
func.addr <- '/home/trn/diploma-thesis/R/scripts/measurements' #where is create-ncdf & clear measurements files script
water.addr <- '/home/trn/Desktop/diploma-thesis/software/multi-iteration/init/GRIDCRO2D_aa2006test'

number <- 400 #how many measurements generate
num.locations <- 380 #how many locations use
num.locations.redundant <- 20 #different locations in same cells
max.time <- 1 #maximum time for passive measurements
row <- 121
col <- 121
multiply.by <- 1000 #real measurements are 1000 bigger then cmaq outputs

#LOAD DATA
simulation <- 1 #if not used data from iterative simulations, use ''
date.from <- as.Date('2006-01-01')
date.to <- as.Date('2006-01-01')

source(paste(func.addr,"/create-ncdf.r",sep=""))
source(paste(func.addr,"/clear-measurements.r",sep=""))

days.num <- as.numeric(date.to-date.from,units="days")+1 #number of days
days.seq <- seq(from=date.from,to=date.to,by='days') #vector of days, format %Y-%m-%d
days.seq2 <- rep("",length(days.seq)) #vector of days, format %Y%j
for(i in 1:length(days.seq)) {
  days.seq2[i] <- format(days.seq[i], "%Y%j")
}

traverseAconc <- function(dir, aconc.name, variables) {
  result <- array(0,dim=c(row,col,days.num))
  for(i in 1:length(days.seq2)) {
    if(simulation=='') {
      file.addr <- paste(dir,'/',aconc.name,'.',days.seq2[i],'.ncf',sep="") #name of file is created
    } else {
      file.addr <- paste(dir,'/',aconc.name,'.',days.seq2[i],'_',simulation,'.ncf',sep="") #name of file is created
    }
    print(paste("Reading:",file.addr))
    file <-  open.ncdf(file.addr)
    temp.result <- matrix(0,nrow=row,ncol=col)
    for(j in variables) { #each variable is read
      temp <- get.var.ncdf(file, j)
      temp.variable <- apply(temp, 2, function(data) { #variable is averaged over hours
	apply(data,1,mean)
      })
      temp.result <- temp.result+temp.variable #variables are summed together !!! (ABAPI, ABAPJ, ABAPK)
    }
    close.ncdf(file)
    result[,,i] <- temp.result #save temp result for one day to summary matrix
  }
  return(result)
}

result <- traverseAconc(output.addr,'CCTM_e1a_Linux2_x86_64ifort.benchmark.ACONC',c('BAP'))

#CREATE LIST OF LOCATIONS
generateLocation <- function(col,row,number,water) { #once every location
  loc.all <- 1:(row*col)
  loc <- sample(loc.all[-water],number)
  coord <- sapply(loc, function(x) { #have to be flipped because water (CMAQ output) has col, row dimensions
    loc.col <- x%%col
    loc.row <- ceiling(x/row)
    if(loc.col==0) loc.col <- col
    return(c(loc.row,loc.col))
  })
  return(data.frame(row=coord[1,],col=coord[2,],nameCS.1=paste('Location ',1:number,sep=""))) #keep it flipped
}
generateRedundantLocation <- function(locations,num) {
  loc <- sample(1:nrow(locations),num,replace=TRUE)  
  return(data.frame(row=locations$row[loc],col=locations$col[loc],nameCS.1=paste('Location ',(nrow(locations)+1):(nrow(locations)+num),sep="")))
}
generateRestOfTheLocations <- function(locations,num) {
  loc <- sample(1:nrow(locations),num,replace=TRUE)
  return(locations[loc,])
}
water.file <- open.ncdf(water.addr)
water <- get.var.ncdf(water.file, 'LWMASK')
water <- which(water==0) #measurements aren't in water, !!! dimensions of water are col,row
locations <- generateLocation(col,row,num.locations,water) #once every location
#add redundant locations
locations <- rbind(locations,generateRedundantLocation(locations,num.locations.redundant))
#add rest of the locations
locations <- rbind(locations,generateRestOfTheLocations(locations,(number-nrow(locations))))
rownames(locations) <- 1:nrow(locations)

#CREATE LIST OF DATES
generateTimes <- function(number,max.time,times) {
  from <- sample(1:(length(times)),number,replace=TRUE)
  to <- sapply(from,function(x) sample(x:(x+max.time),1))
  to[to>length(times)] <- length(times)
  return(data.frame(from=times[from],to=times[to]))
}
times <- generateTimes(number,max.time,days.seq)

vystup <- cbind(locations,times)

#COMPUTE VALUES IN LOCATIONS OVER DEFINED TIME
vystup[,c('value')] <- NA
for(i in 1:nrow(vystup)) {
  vystup$value[i] <- (mean(result[vystup$col[i],vystup$row[i],which(vystup$from[i]==days.seq):which(vystup$to[i]==days.seq)]))*multiply.by #dimensions of result are col,row,time
}

save(vystup,file=paste(save.addr,'.rda',sep=""))
write.csv(vystup,paste(save.addr,'.csv',sep=""),row.names=FALSE)

#save cleared measurements
#vystup <- cleanMeasurements(vystup)
#save(vystup,file=paste(save.addr,'.rda',sep=""))
#write.csv(vystup,paste(save.addr,'.csv',sep=""),row.names=FALSE)