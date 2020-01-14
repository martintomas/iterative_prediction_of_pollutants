#function for post-learning


#reads file from defined directory
readFile <- function(addr,name,iteration,id='',end='') {
  if(id!='') id <- paste('-',id,sep="")
  return(as.matrix(read.csv(paste(addr,'/iteration-',iteration,'-',name,id,end,'.csv',sep=""),header=FALSE)))
}

#polutate file (if learning is not finished) -> used for testing
polutateFile <- function(output,na=FALSE) {
  if(!na) {
    indexes <- which(output!="R" & output!="I" & !is.na(output))
  } else {
    indexes <- which(is.na(output))
  }
  output[indexes] <- sample(seq(0.00001,1,0.002),length(indexes),replace=TRUE)
  return(output)
}

polutateFileRegion <- function(output,num.regions) {
  for(i in 1:ncol(output)) {
    indexes <- which(output[,i]!="R" & output[,i]!="I" & !is.na(output[,i]))
    output[indexes,i] <- sample(1:num.regions[i],length(indexes),replace=TRUE)
  }
  return(output)
}



resizeOutput <- function(output,row,col) {
  if(!is.matrix(output)) {
    if(length(output)<(row*col)) {
      print('Resizing precited output!!! Can lead to wrong prediction.')
      return(sample(output,row*col,replace=TRUE))
    }
  } else {
    if(nrow(output)<(row*col)) {
      print('Resizing predicted output!!! Can lead to wrong prediction.')
      res <- matrix(0,nrow=(row*col),ncol=ncol(output))
      for(i in 1:ncol(output)) {
	res[,i] <- sample(output[,i],row*col,replace=TRUE)
      }
      return(res)
    }
  }
  return(output)
}

#remove repeated values
removeR <- function(output) {
  if(!is.matrix(output)) return(output) #don't contain any R values
  for(i in 1:nrow(output)) {
    indexes <- which(output[i,]=="R")
    if(length(indexes)>0) { #there are some R values
      for(j in indexes) {
	index.new <- which(output[i,1:(j-1)]!="R")
	if(length(index.new)>0) output[i,j] <- output[i,max(index.new)]
	else output[i,j] <- NA
      }
    }
  }
  return(output)
}

removeI <- function(output,water) { 
  if(is.matrix(output)) {
    output[which(water==1),] <- 0
  } else {
    output[which(water==1)] <- 0
  }
  output[which(output=="I")] <- NA #other ignored cells fill with NA
  return(output)
}

denorm.func <- function(value, norm) {
  return((value*(norm[2]-norm[1]))+norm[1])
}

denormalize <- function(data,data.norm) {
  if(is.matrix(data)) {
    len <- ncol(data)
    for(i in 1:len) {
      data[,i] <- denorm.func(data[,i],data.norm[,i])
    }
  } else {
    len <- length(data)
    for(i in 1:len) {
      data[i] <- denorm.func(data[i],data.norm[,i])
    }
  }
  return(data)
}

denormalizeMap <- function(output,norm.data) {
  if(!is.matrix(output)) {
    return(denormalize(as.numeric(output),norm.data))
  }
  for(i in 1:ncol(output)) {
    output[,i] <- denormalize(as.numeric(output[,i]),norm.data)
  }
  return(output)
}

#creates classification problem (how much some values increased/decreased in some region)
regionNorm <- function(x,y,regions) {
  res <- matrix(NA,nrow=nrow(x),ncol=max(regions))
  for(i in 1:max(regions)) {
    cells <- which(regions==i)
    if(length(cells)>0) {
      #output <- matrix(0,nrow=nrow(x),ncol=length(cells))
      output <- t(t(x[,cells])-y[cells]) #x[1,cells]-x[2,cells] #
      #output[2,] <- x[2,cells]-x[1,cells]
      if(is.matrix(output)) {
	res[,i] <- apply(output,1,mean)
      } else {
	res[,i] <- output
      }
    }
  }
  return(res)
}

#changes values based on initial concetrations in regions
deregionMap <- function(output,soil,regions) {
  res <- matrix(NA,ncol=ncol(output),nrow=length(soil))
  for(i in 1:max(regions)) {
    cells <- which(regions==i)
    if(length(cells)>0) {
      soil.max <- max(soil[cells])
      soil.mean <- mean(soil[cells])
      for(j in cells) {
	res[j,] <- (soil.mean+as.numeric(output[i,]))*(soil[j]/(soil.mean))  #soil[j]+as.numeric(output[i,])
      }
    }
  }
  return(res)
}

saveMap <- function(output,addr, number, soil.init, variable) {
  variables <- matrix(NA, nrow=1,ncol=2)
  variables[1,] <- c(variable, "units")
  if(is.matrix(output)) {
    for(i in 1:ncol(output)) {
      file.new <- createNcdf(paste(addr,'_',(number+(i-1)),'.nc',sep=""), dim(soil.init)[1], dim(soil.init)[2], dim(soil.init)[3], 1, variables)
      soil.init[,,1] <- output[,i]
      put.var.ncdf(file.new,variable,soil.init)
      close.ncdf(file.new)
    }
  } else {
    file.new <- createNcdf(paste(addr,'_',number,'.nc',sep=""), dim(soil.init)[1], dim(soil.init)[2], dim(soil.init)[3], 1, variables)
    soil.init[,,1] <- output
    put.var.ncdf(file.new,variable,soil.init)
    close.ncdf(file.new)
  }
}

applyBorders <- function(output,minV,maxV) {
  output[output<minV] <- 0
  output[output>maxV] <- maxV
  return(output)
}

#!!! no smoothing is used
fillNAFunction <- function(data,grd,idw.distance) {
  #image(matrix(output[,1],nrow=row,ncol=col))
  data <- na.omit(data)
  coordinates(data) <- ~ row+col
  idw.out <- idw(isoil ~ 1,data,newdata=grd,idp=idw.distance) 
  #image(matrix(idw.out$var1.pred,nrow=row,ncol=col))
  return(idw.out$var1.pred)
}

fillMissingValues <- function(data,output,idw.distance) {
  grd <- expand.grid(col=seq(from=1, to=row, by=1), row=seq(from=1, to=col, by=1))
  coordinates(grd) <- ~ row+col
  gridded(grd) <- TRUE
  
  #fit NA values
  if(is.matrix(output)) {
    for(i in 1:ncol(output)) {
      data$isoil <- output[,i]
      output[,i] <- fillNAFunction(data,grd,idw.distance)
    }
  } else {
    data$isoil <- output
    output <- fillNAFunction(data,grd,idw.distance)
  }
  
  return(output)  
}

plotTest <- function(output,input.test,row,col,addr,simulation,iteration,end='') {
  name.file <- paste(addr,'/iteration-',iteration,'-simulation-',simulation,end,'.pdf',sep="")
  size <- 140  
  pl.col <- seq(from=min(min(output),min(input.test)), to=max(max(output),max(input.test)), length.out=size+1) #to keep breaks in both plots same
  k <- min(output[output>0])
  pdf(name.file, bg = "white",width=10)
  image(matrix(input.test,nrow=row,ncol=col),main=paste("Original data for simulation number",simulation),col=terrain.colors(size),breaks=pl.col)
  if(!is.matrix(output)) {
    image(matrix(output,nrow=row,ncol=col),main=paste("Predicted data for simulation number",simulation,"map 1"),col=terrain.colors(size),breaks=pl.col)
    difer <- ((output+k)-(input.test+k))/(input.test+k)
    plotDifer(difer)
  } else {
    for(i in 1:ncol(output)) {
      image(matrix(output[,i],nrow=row,ncol=col),main=paste("Predicted data for simulation number",simulation,"map",i),col=terrain.colors(size),breaks=pl.col)
      difer <- ((output[,i]+k)-(input.test+k))/(input.test+k)
      plotDifer(difer)
    }
  } 
  dev.off()
}

plotDifer <- function(difer) {
  difer[which(is.na(difer))] <- 0
  difer[difer==Inf] <- 0
  difer.breaks <- c(floor(min(difer))-1,-0.5,-0.2,0.2,0.5,1,5,10,trunc(max(difer)+1))
  difer.count <- rep(0, length(difer.breaks)-1)
  difer.names <- rep("", length(difer.breaks)-1)
  for(l in 1:(length(difer.breaks)-1)) {
    difer.count[l] <- sum((difer>=difer.breaks[l]) & (difer<difer.breaks[l+1]))
    difer.names[l] <- paste(difer.breaks[l],"-",difer.breaks[l+1])
  }
  difer.names[1] <- paste('<',difer.breaks[2])
  difer.names[length(difer.names)] <- paste('>',difer.breaks[length(difer.breaks)-1])
  bp <- barplot(difer.count, names.arg=difer.names, xlab="Relative error", main="Distribution of errors", ylab="Number of cells",beside=TRUE)
  text(bp, 0, round(difer.count, 1),cex=1,pos=3)
}

plotResult <- function(output.regions,output=NULL,row,col,addr,simulation,iteration,end='') {
  size <- 140 
  if(simulation==-1) {
    name.file <- paste(addr,'/iteration-',iteration,'-output',end,'.pdf',sep="")
  } else {
    name.file <- paste(addr,'/iteration-',iteration,'-simulation-',simulation,end,'.pdf',sep="")
  }
  pdf(name.file, bg="white")
  for(i in 1:ncol(output.regions)) {
    if(simulation==-1) {
      text.main.regions <- paste("Predicted regions for measurements map",i)
      text.main <- paste("Predicted data for measurements map",i)     
    } else {
      text.main.regions <- paste("Predicted regions for simulation number",simulation,"map",i)
      text.main <- paste("Predicted data for simulation number",simulation,"map",i)
    }
    image(matrix(output.regions[,i],nrow=row,ncol=col),main=text.main.regions,col=terrain.colors(size))
    if(!is.null(output)) {
      image(matrix(output[,i],nrow=row,ncol=col),main=text.main,col=terrain.colors(size))
    }
  }
  dev.off()
}

#distance of two matrices (or vectors)
compare <- function(X,Y) {
  if(!is.matrix(X)) {
    return(sqrt(sum((X-Y)^2, na.rm=TRUE))) #smaller is better
  } else {
    res <- rep(NA,ncol(X))
    for(i in 1:ncol(X)) {
      res[i] <- sqrt(sum((X[,i]-Y)^2, na.rm=TRUE))
    }
    return(res)
  }
}

#distance of two matrices (or vectors)
corCompare <- function(X,Y) {
  if(!is.matrix(X)) {
    return(cor(X,Y))
  } else {
    res <- rep(NA,ncol(X))
    for(i in 1:ncol(X)) {
      res[i] <- cor(X[,i],Y)
    }
    return(res)
  }
}

corOutput <- function(input,output) {
  if(is.matrix(output)) {
    res <- rep(0,ncol(output))
    for(i in 1:ncol(output)) {
      res[i] <- cor(as.vector(input),as.vector(output[,i]))
    }
    return(res)
  } else {
    return(cor(as.vector(input),as.vector(output)))
  }
}

infoTest <- function(file.info,simulation,input.test,output) {
  sink(file.info,append=TRUE,type="output")
  cat('\n')
  cat(paste('\"Distances from original for test ',post.len[i],':\" ',sep=""))
  cat(paste('\"',paste(compare(output,input.test),sep="",collapse=" "),'\"',sep=""))
  cat('\n')
  cat(paste('\"Correlation between original and test ',post.len[i],':\" ',sep=""))
  cat(paste('\"',paste(corOutput(input.test,output),sep="",collapse=" "),'\"',sep=""))
  cat('\n')
  cat('\"Summary of original values (min, 1st Qu., Median, Mean, 3rd Qu. Max, Sd):\" ')
  cat(paste('\"',c(as.vector(summary(input.test)),sd(input.test)),'\"',sep=""))
  if(!is.matrix(output)) {
    cat('\n')
    cat('\"Summary of test values sample 1 (min, 1st Qu., Median, Mean, 3rd Qu. Max, Sd):\" ')
    cat(paste('\"',paste(c(as.vector(summary(output)),sd(output)),collapse=" ", sep=""),'\"',sep=""))
  } else {
    for(j in 1:ncol(output)) {
      cat('\n')
      cat(paste('\"Summary of test values sample',j,'(min, 1st Qu., Median, Mean, 3rd Qu. Max, Sd):\" '))
      cat(paste('\"',paste(c(as.vector(summary(output[,j])),sd(output[,j])),collapse=" ", sep=""),'\"',sep=""))
    }
  }
  sink(NULL)
}