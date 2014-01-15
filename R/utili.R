##toglie gli spazi in prima e ultima posizione
.togliSpaziEsterni <- function(testo){ 
  testo <- gsub("^[[:blank:]]","",testo)
  testo <- gsub("[[:blank:]]$","",testo)
  testo
}