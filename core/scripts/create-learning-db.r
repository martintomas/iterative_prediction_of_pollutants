library(RSQLite)

args <- commandArgs(trailingOnly = TRUE)

save.db <- args[1] #'/home/trn/Desktop/diploma-thesis/R/scripts/data-preparation/learning.db'
measurements.file <- args[2] #'/home/trn/Desktop/diploma-thesis/R/scripts/measurements/artificial-measurements-cleared.rda'
func.addr <- args[3] #'/home/trn/diploma-thesis/R/scripts/measurements' #where is create-ncdf & clear measurements files script

source(paste(func.addr,"/clear-measurements.r",sep=""))

load(measurements.file)

          
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = save.db)

#clear db
dbSendQuery(con, "drop table if exists measurement;")
dbSendQuery(con, "drop table if exists input;")
dbSendQuery(con, "drop table if exists inputInfo;")
dbSendQuery(con, "drop table if exists output;")
dbSendQuery(con, "drop table if exists test;")

#create tables
dbSendQuery(con, "create table measurement (id integer not null, row integer not null, col integer not null, fromT varchar(50) not null, toT varchar(50) not null, value double not null, max integer, min integer, CONSTRAINT id_pk PRIMARY KEY (id));")
dbSendQuery(con, "create table output (id integer, measurement integer not null, simulation integer not null, value double not null, CONSTRAINT id_pk PRIMARY KEY (id asc), CONSTRAINT measurement_fk FOREIGN KEY (measurement) REFERENCES measurement(id), CONSTRAINT simulation_fk FOREIGN KEY (simulation) REFERENCES test(simulation));")
dbSendQuery(con, "create table inputInfo (id integer not null, row integer not null, col integer not null, max integer, min integer,CONSTRAINT id_pk PRIMARY KEY (id asc));")
dbSendQuery(con, "create table input (id integer not null, info integer not null, simulation integer not null, value double not null,CONSTRAINT id_pk PRIMARY KEY (id asc), CONSTRAINT info_fk FOREIGN KEY (info) REFERENCES inputInfo(id), CONSTRAINT simulation_fk FOREIGN KEY (simulation) REFERENCES test(simulation));")
dbSendQuery(con, "create table test (simulation integer not null, test boolean not null, CONSTRAINT simulation_pk PRIMARY KEY (simulation asc));")
dbGetQuery(con, "create index iinfo on input(info)")

vystup <- cleanMeasurements(vystup)

#insert measurements
insert.measurements <- "insert into measurement values(:aa,:bb,:cc,:dd,:ee,:ff,:gg,:hh);"
dbBeginTransaction(con)
res <- dbGetPreparedQuery(con,insert.measurements,data.frame(aa=as.numeric(rownames(vystup)), bb=vystup$row, cc=vystup$col, dd=as.character(vystup$from), ee=as.character(vystup$to), ff=vystup$value,gg=vystup$value,hh=vystup$value))
dbCommit(con)

dbDisconnect(con)