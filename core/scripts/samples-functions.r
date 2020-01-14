norm.func <- function(value, norm) {
  return((value-norm[1])/(norm[2]-norm[1]))
}

normalize <- function(data,data.norm) {
  if(is.matrix(data)) {
    len <- ncol(data)
    for(i in 1:len) {
      data[,i] <- norm.func(data[,i],data.norm[,i])
    }
  } else {
    len <- length(data)
    for(i in 1:len) {
      data[i] <- norm.func(data[i],data.norm[,i])
    }
  }
  data[which(is.na(data))] <- 0
  return(data)
}

checkMatrix <- function(x) {
  if(!is.matrix(x)) return(t(as.matrix(x)))
  return(x)
}

#creates classification problem (how much some values increased/decreased in some region)
classificationValues <- function(x,y) {
  for(i in 1:length(y)) {
    x[,i] <- x[,i]-y[i]
  }
  return(apply(x,1,mean))
}

createRegion <- function(input,regions.splits) {
  output <- rep(Inf,length(input))
  for(j in 2:length(regions.splits)) {    
    output[input>=regions.splits[j-1] & input<=regions.splits[j] & output==Inf] <- j-1
  }
  return(output)
}

#removes unused regions
normalizeRegions <- function(output) {
  for(i in 1:ncol(output)) {
    output[,i] <- factor(output[,i],labels=c(1:length(table(output[,i]))))
  }
  return(output)
}