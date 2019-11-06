#' removeStopwords
#' 
#' normalize the given text by finding and removing stopwords of the italian language 
#' (listed in itastopwords)
#' 
#' 
#' \code{removeStopwords} removes stopwords of the italian language in \code{testo}
#' 
#' 
#' @param testo a vector of characters to be normalized
#' @return a vector of normalized characters
#' @author Livio Finos
#' @examples 
#' testo<-("mangio una mela e una banana che ho appena comprato")
#' removeStopwords(testo)
#'  


removeStopwords <- function(testo, stopwords=itastopwords){
  testo <- strsplit(testo, "\\s")
  testo <- lapply(testo, function(x) ifelse((x %in% stopwords | nchar(x)<=1),"", x) )
  testo <- unlist(lapply(testo, function(x) paste(x, collapse=" ")))
#   testo <- paste(" ",testo," ", sep="")
#   testo <- gsub("\\s+", " ", testo, perl=FALSE)
#   testo <- gsub("^[[:blank:]]","",testo)
#   testo <- gsub("[[:blank:]]$","",testo)
  testo
}

