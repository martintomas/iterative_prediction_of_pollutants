library(ncdf)

args <- commandArgs(trailingOnly = TRUE)

Cair.addr <- args[1] #'/home/trn/Desktop/CMAQ/cmaq471_bap/data/cctm/CCTM_e1a_Linux2_x86_64ifort.benchmark.ACONC.2006001.ncf' #args[1]
file.save.addr <- args[2] #'/home/trn/Desktop/CMAQ/cmaq471_bap/save/cctm/accon_1.nc' #add it to the vol file
func.addr <- args[3] #"/home/trn/diploma-thesis/R/scripts/volatilization-function"

source(paste(func.addr,"/create-ncdf.r",sep=""))

Cair.file <-  open.ncdf(Cair.addr)
# sum all variables (ABAPI, ABAPJ, ABAPK)
Cair.abapi <- get.var.ncdf(Cair.file,"ABAPI") 
Cair.abapj <- get.var.ncdf(Cair.file,"ABAPJ") 
Cair.abapk <- get.var.ncdf(Cair.file,"ABAPK")
Cair.bap <- Cair.abapi + Cair.abapj + Cair.abapk
row <- max(get.var.ncdf(Cair.file,"ROW"))
col <- max(get.var.ncdf(Cair.file,"COL"))
lev <- max(get.var.ncdf(Cair.file,"LAY"))
time <- max(get.var.ncdf(Cair.file,"TSTEP"))

variables <- matrix(NA, nrow=1,ncol=2)
variables[1,] <- c("BAP", "g/s -- sum of ABAPI, ABAPJ, ABAPK selected from ACON file")

file.new <- createNcdf(file.save.addr, col, row, lev, time, variables)
put.var.ncdf(file.new,"BAP",Cair.bap)
close.ncdf(file.new)

