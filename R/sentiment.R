sentiment <- function(text, algorithm="Maddalena", vocabularies=NULL){
  if(!is.null(text)){ #se c'e' almeno un testo
    if(!is.null(algorithm)) algorithm="Maddalena"
    if(!is.null(vocabularies)) {
      #       data(pos) #lo decommenti ed eventualmente cambi nomi dei dati 
      #       data(neg) 
      #       vocabularies =list(pos=pos,neg=neg)
      #     per ora:
      vocabularies =list(pos="pos",neg="neg")
    }
    
    #choose and perform algorithm
    if(algorithm=="Maddalena") {
      sent=.sentiment.maddalena(text=text, vocabularies=vocabularies)
      return(sent)
      } else NULL
  } #end if(!is.null(text))
}

# ci sostituisca la sua funzione:
.sentiment.maddalena <- function(text, vocabularies){rep(c("pos","neg","neutro"), length.out=length(text))}