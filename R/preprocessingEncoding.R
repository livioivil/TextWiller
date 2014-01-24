preprocessingEncoding <- function(testo, 
                                  encoding="UTF-8",
                                  suppressStingsWithInvalidMultibye=TRUE,
                                  verbatim=TRUE){
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
  
  #  elimina testi con encoding non riconoscibile:
  if(suppressStingsWithInvalidMultibye){
    droptxt=sapply(testo,function(txt) is(try(nchar(txt),silent=TRUE),"try-error"))
    testo[droptxt]="SUPPRESSEDTEXT"
    if(verbatim) cat("\n NB: ",sum(droptxt)," documenti eliminati (",
                     round(mean(droptxt)*100,3),"%) a causa di multibyte invalidi.\n",sep="")
  }
  testo
}