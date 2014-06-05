preprocessingEncoding <- function(testo, 
                                  encoding="UTF-8",
                                  suppressInvalidTexts=TRUE,
                                  verbatim=TRUE){
   if(is.logical(encoding)) {
    if(encoding) {encoding="UTF-8"  
    } else if(encoding=="") encoding="UTF-8"  
  }

  if(is.logical(encoding)&&(!encoding)){  
    #do nothing
    } else     {
      encs=unique(Encoding(testo))
      encs[encs=="unknown"]=""
      for(i in encs){
        idi=Encoding(testo)==i
        testo[idi]=iconv(testo[idi],from=i,to = encoding)
      }
    }
  
  #  elimina testi con encoding non riconoscibile:
  if(suppressInvalidTexts){
    droptxt=sapply(testo,function(txt) is(try(nchar(txt),silent=TRUE),"try-error"))
    testo[droptxt]="SUPPRESSEDTEXT"
    if(verbatim&(sum(droptxt)>0)) warning(paste(sum(droptxt)," documenti eliminati (",
                     round(mean(droptxt)*100,3),"%) a causa di multibyte invalidi.\n",sep=""))
  }
  testo
}

###OLD VERSION
#   if(is.logical(encoding)) {
#     if(encoding){ 
#       encoding="UTF-8"  
#       testo=sapply(testo, function(text) {
#         if(Encoding(text) == "unknown")                      
#           Encoding(text) <- encoding
#         text <- enc2utf8(text)
#       })
#     } 
#   } else {
#     testo=sapply(testo, function(text) {
#       if(Encoding(text) == "unknown")
#         Encoding(text) <- encoding
#       text <- enc2utf8(text)
#     })
#   }
