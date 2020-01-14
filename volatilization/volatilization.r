library(ncdf)

args <- commandArgs(trailingOnly = TRUE) #returned only arguments after --args

file.addr <- args[1] #"/home/trn/Desktop/diploma-thesis/software/cmaq-multiday/vol.nc"

func.addr <- args[2] #"/home/trn/diploma-thesis/R/scripts/volatilization-function"
source(paste(func.addr,"/Hlconst",sep=""))
source(paste(func.addr,"/Koc",sep=""))

Dlw <- 5*10^(-6) #will change in future
Dga <- 5*10^(-10) #will change in future

Salpha <- 0.2 #will change in future

met.addr <- args[3] #"/home/trn/Desktop/CMAQ/cmaq471_bap/data/mcip3/001/METCRO2D_aa2006test"
met.file <-  open.ncdf(met.addr)
Stheta <- get.var.ncdf(met.file, "SOIM1")
Temp <- get.var.ncdf(met.file, "TEMPG") #load temperature
time <- get.var.ncdf(met.file, "TSTEP")
time <- time[length(time)]

Sphi <- 0.45 # depends on type of soil -> will change in future

Srhobulk <- 1350

Sfoc.addr <- args[4] #"/home/trn/Desktop/diploma-thesis/R/scripts/resample-organic/resampled-organic-carbon-map.nc"
Sfoc.file <-  open.ncdf(Sfoc.addr)
Sfoc <- get.var.ncdf(Sfoc.file, "Sfoc")
#Sfoc <- t(Sfoc[nrow(Sfoc):1,]) #flip it
row <- get.var.ncdf(Sfoc.file, "row")
col <- get.var.ncdf(Sfoc.file, "col")
row <- row[length(row)]
col <- col[length(col)]

Csoil.addr <- args[5] #"/home/trn/Desktop/CMAQ/cmaq471_bap/init/AI219_pop_stor_1983_87.nc" #will be taken as argument
Csoil.file <-  open.ncdf(Csoil.addr)
Csoil <- get.var.ncdf(Csoil.file, "STOR_pop2")[,,1] #read first level of STOR_pop2
#Csoil <- t(Csoil[nrow(Csoil):1,]) #flip it

Cair <- 0.00000001 #!!!!needs to be changed, I don't know exact value 

Time_mod <- 1
SL_height <- 0.1

#where is water
water.addr <- args[6] #"/home/trn/Desktop/CMAQ/cmaq471_bap/data/mcip3/001/GRIDCRO2D_aa2006test"
water.file <- open.ncdf(water.addr)
water <- get.var.ncdf(water.file,"LWMASK")

DesF <- function(Salpha, Stheta, Sphi, Srhobulk, Sfoc, Koc, Hlconst, Dga, Dlw) {
  Des <- array(0, dim=c(row,col,time))
  for(i in 1:time) {
    Des[,,i]<-(((Salpha^(10/3))*Dga*Hlconst[,,i]+(Stheta[,,i]^(10/3))*Dlw)/(Sphi^2))/((Srhobulk*Sfoc*Koc)+Stheta[,,i]+(Salpha*Hlconst[,,i]))
  }
  return(Des)
}

KsaF <- function(Salpha, Stheta, Srhobulk, Sfoc, Koc, Hlconst) {
  Ksa <- array(0, dim=c(row,col,time))
  for(i in 1:time) {
    Ksa[,,i]<-Srhobulk*Sfoc*Koc*Hlconst[,,i] + Salpha + Stheta[,,i]
  }
  return(Ksa)
}

FasF <- function(Csoil,Cair,Ksa,Des,Time_mod,SL_height,water) {
  Fsa <- array(0, dim=c(row,col,time))
  for(i in 1:time) {
    Fsa[,,i]<-water*(Csoil - (Cair*Ksa[,,i]))*sqrt((Des[,,i]/(pi*Time_mod)))*(1-(exp(-(SL_height^2)/(4*Des[,,i]*Time_mod))))
  }
  return(Fsa)
}

Koc <- KocF('BAP')
Hlconst <- HlconstF('BAP',Temp)

Des <- DesF(Salpha, Stheta, Sphi, Srhobulk, Sfoc, Koc, Hlconst, Dga, Dlw)
Ksa <- KsaF(Salpha, Stheta, Srhobulk, Sfoc, Koc, Hlconst)
Fas <- FasF(Csoil,Cair,Ksa,Des,Time_mod,SL_height,water)

file <- open.ncdf(file.addr, write=TRUE)
put.var.ncdf(file,"Vol",Fas)
close.ncdf(file)


