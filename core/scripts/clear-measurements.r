
unifyCellTimeValues <- function(data, id, row, col) { #only passive
  days <- seq(from=min(as.Date(data$from)),to=max(as.Date(data$to)),by="days")
  if(length(days)<2) return(data.frame(value=data$value,from=data$from,to=data$to)) #no need to change anything  
  #passive filters
  result <- data.frame(value=NA,from=days[-length(days)],to=(days[-length(days)]+1))
  for(i in 1:nrow(result)) {
    result$value[i] <- mean(data[which((data$from<=result$from[i]) & (data$to>=result$to[i])),]$value) #average results inside one cell
  }
  result <- na.omit(result)
  if(nrow(result)>2) {
    index <- 1
    for(i in 2:nrow(result)) {
      if(result$value[index] == result$value[i]) {
	result$to[index] <- result$to[i]
	result$value[i] <- NA
      } else {
	index <- i
      }
    }
  }
  result <- na.omit(result)
  #active filters
  result2 <- data.frame(value=NA,from=days,to=days)
  for(i in 1:length(days)) {
    result2$value[i] <- mean(data[which((data$from==result2$from[i]) & (data$to==result2$to[i])),]$value) #average results inside one cell
  }
  result2 <- na.omit(result2)
  return(rbind(result,result2))
}

cleanMeasurements <- function(vystup) {
  date.from <- as.Date(min(vystup$from))
  date.to <- as.Date(max(vystup$to))
  days <- seq(from=date.from,to=date.to,by="days")
  vystup[,c('coordinates')] <- paste(vystup$row,vystup$col)
  vystup$coordinates <- factor(vystup$coordinates, labels=1:length(table(vystup$coordinates)))
  vystup <- vystup[order(vystup$coordinates),]
  output <- NA
  for(k in levels(vystup$coordinates)) {
    data <- vystup[which(vystup$coordinates==k),]
    res <- unifyCellTimeValues(data,k,vystup$row,vystup$col)
    #res[,c('row','col','nameCS.1')] <- data.frame(row=data$row[1],col=data$col[1],nameCS.1=paste('Location',k)) #matrix(c(data$row[1],data$col[1],paste('Location',k)),nrow=nrow(res),ncol=3,byrow=TRUE)
    res <- cbind(res,data.frame(row=data$row[1],col=data$col[1],nameCS.1=paste('Location',k)))
    if(any(is.na(output))) {
      output <- res
    } else {
      output <- rbind(output,res)
    }
  }
  rownames(output) <- 1:nrow(output)
  return(output)
}