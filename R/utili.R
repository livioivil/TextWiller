##toglie gli spazi in prima e ultima posizione
.togliSpaziEsterni <- function(testo){ 
  testo <- gsub("^[[:blank:]]*","",testo)
  testo <- gsub("[[:blank:]]*$","",testo)
  testo
}

##########################

.contaStringhe  <- function(testo,
                            stringhe=c("\\?","\\!","#","@","(€|euro)","(\\$|dollar)")                            
                            ){
  conteggi=sapply(stringhe,function(stringa) str_count(testo,stringa))
  colnames(conteggi)=paste("Conteggi.",colnames(conteggi),sep="")
  conteggi
}
