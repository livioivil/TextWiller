#fix ts and sort db by ts
fixTimeStamp <- function(db,campoData="created",timeRange=NULL){
  # assegna fuso orario al ts
  if(is.integer(db[,campoData])) 
    db[,campoData] <- as.POSIXct(db[,campoData], origin="1970-01-01", tz="UTC") else
      db[,campoData] <- as.POSIXct(db[,campoData]) + 3600
  
  if(!is.null(timeRange)){
    db <- selezionaIntervalloTimeStamp(db,timeRange=timeRange,campoData=campoData)
  }
  
  # assegna rownames come id (in funzione del tempo)
  db <- db[order(db[,campoData]),]
  rownames(db) <- c(1:nrow(db))

  db
}
#########################
selezionaIntervalloTimeStamp <- function(db, timeRange=range(db$created),campoData="created"){
  sel=(db[,campoData]>=as.POSIXct(timeRange[1]))&(db[,campoData]<=as.POSIXct(timeRange[2]))
  db[sel,]
}