library(sp)
library(fields)

theta <- 0.9 #for smoothing #0.65 for idw(could be more), 0.85 for lm,

randomIDW <- function(data,grd,num.na,col,row,max.dist,min.dist) {
  library(gstat)
  data.copy <- data
  soil.na <- waterProcentual(row,col,num.na,data$water)
  data.copy$isoil[soil.na] <- NA
  #image(matrix(soil.init.vector,nrow=row,ncol=col)) 
  data.copy <- na.omit(data.copy)
  coordinates(data.copy) <- ~ row+col
  
  idp <- sample(seq(min.dist,max.dist,by=0.25), 1,replace=FALSE)
  idw.out <- idw(isoil ~ 1,data.copy,newdata=grd,idp=idp)  
  
  return(postProcess(as.vector(idw.out$var1.pred),max(data$isoil),col,row,data$water))
}

randomIDW2 <- function(data,grd,num.na,col,row,max.dist,min.dist) {
  library(gstat)
  data.copy <- data
  soil.na <- waterProcentual(row,col,num.na,data$water)
  data.copy$isoil[soil.na] <- NA
  data.copy <- na.omit(data.copy)
  data.copy$isoil <- rnorm(length(data.copy$isoil),mean=data.copy$isoil,sd=sd(data.copy$isoil)*2)
 
  coordinates(data.copy) <- ~ row+col  
  idp <- sample(seq(min.dist,max.dist,by=0.25), 1,replace=FALSE)
  idw.out <- idw(isoil ~ urban+dluse+oc+tsoil+temperature+elevation,data.copy,newdata=grd,idp=idp) 
  
  return(postProcess(as.vector(idw.out$var1.pred),max(data$isoil),col,row,data$water))
}

randomLM <- function(data,num.na,col,row) {
  data.copy <- data
  soil.na <- waterProcentual(row,col,num.na,data$water)
  data.copy$isoil[soil.na] <- NA
  data.copy <- na.omit(data.copy)
  data.copy$isoil <- rnorm(length(data.copy$isoil),mean=data.copy$isoil,sd=sd(data.copy$isoil)*2) #change a bit original concentrations
  data.pred <- lm(isoil~row+col+urban+dluse+oc+tsoil+elevation, data.copy)
  n <- predict(data.pred,newdata=data)
  
  return(postProcess(n,max(data$isoil),col,row,data$water))
}

randomBRNN <- function(data,num.na,col,row) {
  library(caret)
  data.copy <- data
  soil.na <- waterProcentual(row,col,num.na,data$water)
  data.copy$isoil[soil.na] <- NA
  data.copy <- na.omit(data.copy)
  data.copy$isoil <- rnorm(length(data.copy$isoil),mean=data.copy$isoil,sd=sd(data.copy$isoil)*2)
  data.pred <- train(data.copy[,-c(2,3,5,11)],data.copy$isoil,method='brnn',tuneLength=1) #defined variables from analysis.csv file
  n <- predict(data.pred,newdata=data[,-c(2,3,5,11)])  
  
  return(postProcess(n,max(data$isoil),col,row,data$water))
}

randomKriging <- function(data,grd,num.na,col,row) {
  library(gstat)
  data.copy <- data
  soil.na <- waterProcentual(row,col,num.na,data$water)
  data.copy$isoil[soil.na] <- NA
  data.copy <- na.omit(data.copy)
  #data.copy$isoil <- rnorm(length(data.copy$isoil),mean=data.copy$isoil,sd=sd(data.copy$isoil)/5)
  
  coordinates(data.copy) <- ~ row+col  
  #krige.out <- krige(value ~ 1,data,newdata=grd)
  out <- tryCatch({
    g <- gstat(id="value", formula=isoil ~ 1, data=data.copy)
    var <- variogram(g,alpha = c(0, 45,90, 135))
    #v.fit <- fit.variogram(var, model=vgm(1.896, "Gau", 12, 2.371))
    v.fit <- fit.variogram(var, model=vgm(model='Lin' , anis=c(0, 0.5)))
    n <- krige(isoil ~ 1,data.copy,newdata=grd,v.fit)$var1.pred
    #if(any(is.na(krige.out$var1.pred))) randomKriging(data,grd,num.na,col,row) #if it contains some NA values, use kriging once more time
    return(postProcess(n,max(data$isoil),col,row,data$water))
  }, error = function(err) {
    #wrong LDL factor, imposible to interpolate values
    #use different approach
    return(randomIDW(data,grd,num.na,col,row,8,5))
    #return(err)
  })
}

postProcess <- function(data,max.value,col,row,water) {
  data[which(is.na(data))] <- 0
  data[data>max.value] <-max.value
  data[data<0] <- 0
  data <- image.smooth(matrix(data,nrow=col,ncol=row),theta=theta)$z
  data[which(water==1 | data<0)] <- 0
  return(data)
}

#at all random cells cannot be water
waterProcentual <- function(row,col,num.na,water) {
#  soil.na <- sample(1:(row*col),num.na,replace=FALSE) 
#   if(sum(water[soil.na])>(0.5*num.na)) {
#     waterProcentual(row,col,num.na.water)
#   }
  soil.ok <- sample(which(water!=1),(row*col)-num.na,replace=FALSE) #take values only from soil (exclude water totally)
  soil.na <- c(1:(row*col))[-soil.ok]
  return(soil.na)
}

