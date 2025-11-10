#' Sentiment analysis
#' 
#' Assigns a sentiment score to each text in \code{text}.
#' 
#' @aliases sentiment sentimentVocabularies vocabolariMadda vocabolarioMattivio
#' @param text Vector of texts.
#' @param algorithm \code{"Mattivio"} by default, or \code{"Maddalena"}, or a function returning the sentiment score.
#' @param vocabularies \code{vocabolarioMattivio} by default if \code{algorithm == "Mattivio"}); 
#' \code{vocabolariMadda} by default if \code{algorithm == "Maddalena"}), or an object used by the algorithm.
#' @param normalizzaTesti \code{TRUE} by default.
#' @param get_labels \code{TRUE} by default. If \code{FLASE}, gives the quantitative score; 
#' if \code{TRUE}, provides the labels -1, 0, +1 (i.e. \code{sign(score)}).
#' @return An array containing a single numerical value for each element of \code{text}.
#' @note %% ~~further notes~~
#' @author Maddalena Branca, Mattia Da Pont, Livio Finos
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' sentiment(c("ciao bella", "mi piaci", "wow!!","good","casa", "farabutto!","ti odio"))
#' 
#' @export sentiment
#' 
sentiment <- function(text, algorithm="Mattivio", 
                      vocabularies=NULL,
                      normalizzaTesti=TRUE, get_labels=TRUE,...){
  if(!is.null(text)){ #se c'e' almeno un testo
    if(is.null(algorithm)) algorithm="Mattivio"
    if(is.null(vocabularies)) {
      if(algorithm=="Mattivio") {
        data(vocabolarioMattivio) 
        vocabularies=vocabolarioMattivio
        } else if(algorithm=="Maddalena") {
          data(vocabolariMadda) 
          vocabularies=vocabolariMadda
        }
    }
    
    if(normalizzaTesti==TRUE)
      text<-normalizzaTesti(text,suppressInvalidTexts=FALSE,contaStringhe=NULL,...)
    
    #choose and perform algorithm
    if(is.function(algorithm)) {
      sent=algorithm(text=text, vocabularies=vocabularies,...)
      return(sent)} else 
        if(algorithm=="Maddalena") {
      sent=.sentiment.maddalena(text=text, vocabularies=vocabularies,...)
      
    } else if(algorithm=="Mattivio") {
      sent=.sentiment.mattivio(text=text, vocabularies=vocabularies,...)
    } else sent=NULL
    
    if(get_labels)
      return(sign(sent)) else
        return(sent)
    
  } #end if(!is.null(text))
}

