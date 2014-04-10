removeStopwords <- function(testo, stopwords=itastopwords){
  testo <- strsplit(testo, "\\s")
  testo <- lapply(testo, function(x) { x[-which(x %in% stopwords | nchar(x)<=1)]} )
  testo <- unlist(lapply(testo, function(x) paste(x, collapse=" ")))
#   testo <- paste(" ",testo," ", sep="")
#   testo <- gsub("\\s+", " ", testo, perl=FALSE)
#   testo <- gsub("^[[:blank:]]","",testo)
#   testo <- gsub("[[:blank:]]$","",testo)
  testo
}