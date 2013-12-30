#fix ts and sort db by ts
fixTimeStamp <- function(db,timeRange=NULL, cutBy=){
  # assegna fuso orario al ts
  if(is.integer(db$ts)) 
    db$ts <- as.POSIXct(db$ts, origin="1970-01-01", tz="UTC") else
      db$ts <- as.POSIXct(db$ts) + 3600
  
  if(!is.null(timeRange)){
    # subset in funzione del perimetro temporale
    db <- db[which(db$ts>=as.POSIXct(timeRange[1])),]
    db <- db[which(db$ts<=as.POSIXct(timeRange[2])),]
  }
  
  # assegna rownames come id (in funzione del tempo)
  db <- db[order(db$ts),]
  rownames(db) <- c(1:nrow(db))
  
  if(!is.null(cutBy)) if(!(cutBy==FALSE))
  {
    
  }
  db
}