##toglie gli spazi in prima e ultima posizione
.togliSpaziEsterni <- function(testo){ 
  testo <- gsub("^[[:blank:]]*","",testo)
  testo <- gsub("[[:blank:]]*$","",testo)
  testo
}

##########################
# questo è stato aggiornato

.contaStringhe<-function(testo){
  toMatch<-c("\\?","\\!","(€|euro)","(\\$|dollar)","@\\w+","#\\w+")
  conteggi<-sapply(toMatch,function(stringa) str_count(testo,stringa))
  colnames(conteggi)<-c("?","!","(€|euro)","$|dollar","retweet","hashtag")
  conteggi
}