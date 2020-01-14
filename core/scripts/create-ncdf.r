createNcdf <- function(file.addr, col=121, row=121, lev=1, time=25, variables) { #time=default values of emis file
  #setup dimensions
  dim3 <- dim.def.ncdf("TSTEP","time",1:time, unlim=TRUE)
  dim2 <- dim.def.ncdf("LAY","levels",1:lev) #only one level, under ground
  dim1 <- dim.def.ncdf("ROW", "degrees_north", 1:row)
  dim0 <- dim.def.ncdf("COL", "degrees_east", 1:col)  
  mv <- 1.e30 # missing value to use !!! TAKEN FROM DOCUMENTATION OF ncdf
  vard <- list()
  for(i in 1:nrow(variables)) {
    vard[[i]] <- var.def.ncdf(variables[i,1], variables[i,2], list(dim0,dim1,dim2,dim3),mv)
  }
  #add some atributes
  file.new <- create.ncdf(file.addr, vard) #want to use upstairs
  att.put.ncdf(file.new,0,'XCELL',36000)
  att.put.ncdf(file.new,0,'YCELL',36000)
  att.put.ncdf(file.new,0,'XORIG',-1746000)
  att.put.ncdf(file.new,0,'YORIG',-2142000)
  #att.put.ncdf(file.new,0,'CDATE',2014274,prec="int")
  #att.put.ncdf(file.new,0,'CTIME',171708,prec="int")
  #att.put.ncdf(file.new,0,'WDATE',2014274,prec="int")
  #att.put.ncdf(file.new,0,'WTIME',171708,prec="int")
  att.put.ncdf(file.new,0,'SDATE',2006001,prec="int") #data for date
  att.put.ncdf(file.new,0,'GDNAM','LCC36',prec="int") #used projection
  att.put.ncdf(file.new,0,'NLAYS',1,prec="int")
  att.put.ncdf(file.new,0,'STIME',0,prec="int")
  att.put.ncdf(file.new,0,'TSTEP',time,prec="int")
  att.put.ncdf(file.new,0,'NROWS',row,prec="int")
  att.put.ncdf(file.new,0,'NCOLS',col,prec="int")
  att.put.ncdf(file.new,0,'XCENT',10)
  att.put.ncdf(file.new,0,'YCENT',50)
  att.put.ncdf(file.new,0,'FTYPE',1,prec="int")
  att.put.ncdf(file.new,0,'NTHIK',1,prec="int")
  att.put.ncdf(file.new,0,'NVARS',2,prec="int")
  att.put.ncdf(file.new,0,'GDTYP',2,prec="int")
  att.put.ncdf(file.new,0,'P_ALP',48)
  att.put.ncdf(file.new,0,'P_BET',52)
  att.put.ncdf(file.new,0,'P_GAM',10)
  #att.put.ncdf(file.new,0,'VGTYP',7,prec="int")
  #att.put.ncdf(file.new,0,'VGTOP',2000,prec="double")
  #att.put.ncdf(file.new,0,'VGLVLS',c(1,0),prec="double")
  return(file.new)
}