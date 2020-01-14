# !!! UPDATE WHEN USED PROJECTION CHANGES
library(ncdf)

args <- commandArgs(trailingOnly = TRUE) #returned only arguments after --args

file.addr <- args[1] #'/home/trn/Desktop/diploma-thesis/software/cmaq-multiday/vol.nc'
func.addr <- args[2] #"/home/trn/diploma-thesis/R/scripts/volatilization-function"
#row <- 121
#col <- 121
#time <- 25
#lev <- 1

source(paste(func.addr,"/create-ncdf.r",sep=""))

variables <- matrix(NA, nrow=1,ncol=2)
variables[1,] <- c("Vol", "result computed with volatilization function")

file.new <- createNcdf(file.addr=file.addr, variables=variables)
close.ncdf(file.new)