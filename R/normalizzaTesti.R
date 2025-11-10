#' Text normalization functions
#' 
#' Normalization of various characteristics of texts for analysis.
#' 
#' \code{stopwords_ita} is a list of Italian stopwords.
#' 
#' @aliases normalizzaTesti normalizzacaratteri normalizzaemoticon normalizzahtml
#' normalizzaslang normalizzapunteggiatura tryTolower stopwords_ita
#' removeStopwords preprocessingEncoding
#' @param testo Character vector of texts.
#' @param tolower \code{TRUE} by default.
#' @param normalizzahtml \code{TRUE} by default.
#' @param normalizzacaratteri \code{TRUE} by default.
#' @param normalizza_emoticon \code{TRUE} by default.
#' @param normalizzapunteggiatura \code{TRUE} by default.
#' @param normalizzaslang \code{TRUE} by default.
#' @param fixed vedi \code{\link[base:gsub]{base:gsub}}. Preferably do not use.
#' @param perl vedi \code{\link[base:gsub]{base:gsub}}. Preferably do not use.
#' @param preprocessingEncoding Logical.
#' @param encoding \code{"UTF-8"} by default. If \code{FALSE}, it avoids conversion.
#' @param sub Character string. If not NA, it is used to replace any
#' non-convertible bytes in the input. See also parameter \code{sub} in
#' function \code{iconv}.
#' @param contaStringhe Strings to count in documents. Default: \code{
#' c("\\?","\\!","#","@", "(€|euro)","(\\$|dollar)","SUPPRESSEDTEXT")}
#' @param suppressInvalidTexts Substitutes strings with invalid multibytes with
#' \code{"SUPPRESSEDTEXT"}, as they would likely produce errors in subsequent
#' normalizations. Default \code{TRUE}.
#' @param removeUnderscore Remove underscores?
#' @param ifErrorReturnText What to return for texts with a wrong encoding.
#' @param stopwords List of words to exclude form analysis. \code{stopwords_ita} by default.
#' @param verbatim Shows statistics during the process. \code{TRUE} by default.
#' @param remove \code{TRUE} by default. A vector of words to be removed.
#' @return For \code{normalizzaTesti} the output is the vector of normalized texts. 
#' The counts table in \code{contaStringhe} is assigned as \code{counts} table in 
#' the vector's \code{attributes}. For all other functions, the output is a \code{vector} 
#' of the same length as \code{testo} but with normalized texts.
#' @note %% ~~further notes~~
#' @author Dario Solari, Livio Finos, Maddalena Branca, Mattia Da Pont
#' @keywords ~kwd1 ~kwd2
#' @examples
#' testoNorm <- normalizzaTesti(c('ciao bella!','www.associazionerospo.org','noooo, che grandeeeeee!!!!!','mitticooo', 'mai possibile?!?!'))
#' testoNorm
#' attr(testoNorm,"counts")
#' 
#' @export normalizzaTesti
#' @export normalizza_emoticon
#' @export normalizzacaratteri
#' @export normalizzahtml
#' @export normalizzapunteggiatura
#' @export normalizzaslang
#' @export preprocessingEncoding
#' @export tryTolower 
#' @export removeStopwords

normalizzaTesti <- function(testo, tolower=TRUE,normalizzahtml=TRUE,
                            normalizzacaratteri=TRUE,
                            normalizza_emoticon=TRUE,
                            normalizzapunteggiatura=TRUE,
                            normalizzaslang=TRUE,
                            fixed=TRUE,perl=TRUE,
                            preprocessingEncoding=TRUE, encoding="UTF-8", sub="",
                            contaStringhe=c("\\?","\\!","@","#",
                                            "(\u20AC|euro)","(\\$|dollar)",
                                            "SUPPRESSEDTEXT"),
                            suppressInvalidTexts=TRUE,
                            verbatim=TRUE, remove=TRUE,removeUnderscore=FALSE){
  Sys.setlocale("LC_ALL", "")
  if(preprocessingEncoding) testo<-preprocessingEncoding(testo,
                                                         encoding=encoding,
                                                         sub=sub,
                                                         suppressInvalidTexts=suppressInvalidTexts,
                                                         verbatim=verbatim)
  #######################
  # PREPROCESSING #
  #######################
  # aggiunta spazi per preprocessing
  testo <- paste(" ",testo," ",sep="")
  # normalizza encoding
  if(normalizzacaratteri) testo <- normalizzacaratteri(testo,fixed=fixed)
  # pulizia testo preliminare (html)
  if(normalizzahtml) testo <- normalizzahtml(testo)
  if(!is.null(contaStringhe))
    conteggiStringhe=.contaStringhe(testo,contaStringhe) else
      conteggiStringhe=NULL
  # identifica emoticon
  #source(paste(functiondir,"/normalizza_emoticon.R",sep=""), .GlobalEnv)
   
  if(normalizza_emoticon) testo <- normalizza_emoticon(testo,perl=perl)
  # pulizia punteggiatura
  #source(paste(functiondir,"/normalizzapunteggiatura.R",sep=""), .GlobalEnv)
  if(normalizzapunteggiatura) testo <- normalizzapunteggiatura(testo,perl=perl,fixed=fixed,removeUnderscore=removeUnderscore)
  # normalizza slang
  #source(paste(functiondir,"/normalizzaslang.R",sep=""), .GlobalEnv)
  if(normalizzaslang) testo <- normalizzaslang(testo,perl=perl)
  # tolower
  if(tolower) testo <- tryTolower(testo,ifErrorReturnText=TRUE)
  if(is.null(remove)) remove=TRUE
  if(is.character(remove)) {
    testo <- removeStopwords(testo,remove)
    } else {
      if(remove) {
        data(stopwords_ita)
        testo <- removeStopwords(testo,stopwords_ita)
      }
    }
  testo <- gsub("\\s+", " ", testo, perl=perl)
  testo <- .togliSpaziEsterni(testo)
  attr(testo,"counts")=conteggiStringhe
  testo
}
