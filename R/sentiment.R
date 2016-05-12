#' Performs sentiment analysis
#' 
#' Assegna una sentiment per ogni testo in \code{text}
#' 
#' aggiungere dettagli qui
#' 
#' @aliases sentiment sentimentVocabularies vocabolariMadda
#' @param text descr .
#' @param algorithm descr .
#' @param vocabularies \code{vocabolariMadda} by default.
#' @return l'output etc
#' @note %% ~~further notes~~
#' @author Maddalena Branca, Livio Finos
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' sentiment(c("ciao bella", "ciao", "good","casa", "farabutto!"))
#' 
#' @export sentiment
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

