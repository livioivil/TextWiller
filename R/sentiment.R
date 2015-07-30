sentiment <- function(text, algorithm="Maddalena", vocabularies=NULL,...){
  if(!is.null(text)){ #se c'e' almeno un testo
    if(is.null(algorithm)) algorithm="Maddalena"
    if(is.null(vocabularies)) {
      data(vocabolariMadda) #lo decommenti ed eventualmente cambi nomi dei dati
      vocabularies=vocabolariMadda
    }
    
    #choose and perform algorithm
    if(algorithm=="Maddalena") {
      sent=.sentiment.maddalena(text=text, vocabularies=vocabularies,...)
      return(sent)
    } else NULL
  } #end if(!is.null(text))
}

