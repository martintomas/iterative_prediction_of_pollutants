library(ncdf)

args <- commandArgs(trailingOnly = TRUE) #returned only arguments after --args

file.addr <- args[1] #'/home/trn/Desktop/diploma-thesis/software/cmaq-multiday/vol.nc'
emis.addr <- args[2] #"/home/trn/Desktop/CMAQ/cmaq471_bap/data/emis/ren_POA36_001_copy"

file.ncdf <- open.ncdf(file.addr)
Vol <- get.var.ncdf(file.ncdf, "Vol")

emis.ncdf <- open.ncdf(emis.addr, write=TRUE)
BAP <- get.var.ncdf(emis.ncdf, "BAP")

BAP[,,1,] <- BAP[,,1,]+Vol

put.var.ncdf(emis.ncdf,"BAP",BAP)
close.ncdf(emis.ncdf)