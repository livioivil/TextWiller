#' Funzioni di gestione delle date
#' 
#' Funzioni di gestione delle date
#' 
#' aggiungere dettagli qui
#' 
#' @aliases fixTimeStamp selezionaIntervalloTimeStamp
#' @export fixTimeStamp 
#' @export selezionaIntervalloTimeStamp
#' @param db data.frame contenente i tweets.
#' @param timeRange due valori di tipo data indicanti inizio e fine.
#' @param campoData "created" e "ts" sono due campi data del db estratto da
#' dump_twitter.R
#' @return l'output e' db "aggiustato"
#' @note %% ~~further notes~~
#' @author Dario Solari, Livio Finos
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'  \dontrun{TW=fixTimeStamp(TW)}
#'  \dontrun{TW=selezionaIntervallo(TW,as.POSIXct(c("2013-12-27 17:54:42 CET", "2013-12-27 22:33:38 CET")))}
#'  \dontrun{TW$created.round <- as.POSIXct(round(t$created,"hour"))}
#' 
#' 
#' 

#fix ts and sort db by ts
fixTimeStamp <- function(db,campoData="created",timeRange=NULL){
  # assegna fuso orario al ts
  if(is.integer(db[,campoData])) 
    db[,campoData] <- as.POSIXct(db[,campoData], origin="1970-01-01", tz="UTC") else
      db[,campoData] <- as.POSIXct(db[,campoData]) 
  
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