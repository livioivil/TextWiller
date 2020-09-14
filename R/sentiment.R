#' Performs sentiment analysis
#' 
#' Assegna una sentiment per ogni testo in \code{text}
#' 
#' aggiungere dettagli qui
#' 
#' @aliases sentiment sentimentVocabularies vocabolariMadda
#' @param text vector of texts
#' @param algorithm \code{"Mattivio"} (default), \code{"Maddalena"} or a function returning the score.
#' @param vocabularies \code{vocabolarioMattivio} (default), \code{vocabolariMadda} or an object used by the algorithm
#' @param normalizzaTesti TRUE by default
#' @return l'output etc
#' @note %% ~~further notes~~
#' @author Maddalena Branca, Livio Finos
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' sentiment(c("ciao bella", "ciao", "good","casa", "farabutto!"))
#' 
#' @export sentiment
sentiment <- function(text, algorithm="Maddalena", 
                      vocabularies=NULL,
                      normalizzaTesti=TRUE, ...){
  if(!is.null(text)){ #se c'e' almeno un testo
    if(is.null(algorithm)) algorithm="Mattivio"
    if(is.null(vocabularies)) {
      data(vocabolariMadda) #lo decommenti ed eventualmente cambi nomi dei dati
      vocabularies=vocabolarioMattivio
    }
    
    if(normalizzaTesti==TRUE)
      text<-normalizzaTesti(text,suppressInvalidTexts=FALSE,contaStringhe=NULL,...)
    
    #choose and perform algorithm
    if(is.function(algorithm)) {
      sent=algorithm(text=text, vocabularies=vocabularies,...)
      return(sent)} else 
        if(algorithm=="Maddalena") {
      sent=.sentiment.maddalena(text=text, vocabularies=vocabularies,...)
      return(sent)
    } else if(algorithm=="Mattivio") {
      sent=.sentiment.mattivio(text=text, vocabularies=vocabularies,...)
      return(sent)
    } else NULL
  } #end if(!is.null(text))
}

