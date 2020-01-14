library(RSQLite)

args <- commandArgs(trailingOnly = TRUE)

file.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning2.db'
save.dir <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/learning/dummy-region.csv'
file.analysis.addr <- args[3] #'/home/trn/Desktop/diploma-thesis/R/scripts/init-soil/analysis.csv' #to obtain water file

data.water <- read.csv(file.analysis.addr)$water

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = file.db)

res <- dbSendQuery(con,'select max(row),max(col) from inputInfo')
dim <- as.matrix(fetch(res,n=-1))
row <- dim[1,1]
col <- dim[1,2]
dbClearResult(res)

x <- matrix(Inf,nrow=row,ncol=col)
x[which(data.water==1)] <- 1 #water is one group
x[which(data.water!=1)] <- 2:(length(data.water[data.water<1])+1) #other regions correspond with cells

print('Saving dummy region learning map')
write.csv(x,save.dir,row.names=FALSE)

dbDisconnect(con)