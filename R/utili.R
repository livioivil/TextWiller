##toglie gli spazi in prima e ultima posizione
.togliSpaziEsterni <- function(testo){ 
  testo <- gsub("^[[:blank:]]","",testo)
  testo <- gsub("[[:blank:]]$","",testo)
  testo
}



.contaStringhe  <- function(testo,stringhe=c("\\?","\\!","#","@","(€|euro)","(\\$|dollar)")){
  conteggi=sapply(stringhe,function(stringa) str_count(testo,stringa))
  conteggi
}


.preprocessing <- function(testo, encoding="UTF-8"){
  if(is.logical(encoding)) {
    if(encoding){ 
      encoding="UTF-8"  
      testo=sapply(testo, function(text) {
        if(Encoding(text) == "unknown")                      
          Encoding(text) <- encoding
        text <- enc2utf8(text)
      })
    } 
  } else {
    testo=sapply(testo, function(text) {
      if(Encoding(text) == "unknown")
        Encoding(text) <- encoding
      text <- enc2utf8(text)
    })
  }
  
}