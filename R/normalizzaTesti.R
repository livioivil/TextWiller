#' Varie funzioni di normalizzazione del testo
#' 
#' Varie funzioni di normalizzazione del testo
#' 
#' \code{itastopwords} e' una lista di stopwords italiane.
#' 
#' @aliases normalizzaTesti normalizzacaratteri normalizzaemote normalizzahtml
#' normalizzaslang normalizzapunteggiatura tryTolower itastopwords
#' removeStopwords preprocessingEncoding
#' @param testo character vector of texts
#' @param tolower \code{TRUE} by default
#' @param normalizzahtml \code{TRUE} by default
#' @param normalizzacaratteri \code{TRUE} by default
#' @param normalizzaEmoticons \code{TRUE} by default
#' @param normalizzaemote \code{TRUE} by default
#' @param normalizzapunteggiatura \code{TRUE} by default
#' @param normalizzaslang \code{TRUE} by default
#' @param fixed vedi \code{\link[base:gsub]{base:gsub}}. Preferibilmente non
#' usare l'opzione.
#' @param perl vedi \code{\link[base:gsub]{base:gsub}}. Preferibilmente non
#' usare l'opzione.
#' @param preprocessingEncoding logical
#' @param encoding \code{"UTF-8"} default. Se \code{FALSE} evita la
#' conversione.
#' @param sub character string. If not NA it is used to replace any
#' non-convertible bytes in the input. See also parameter \code{sub} in
#' function \code{iconv}.
#' @param contaStringhe stringhe da contare nei documenti. Default: \code{
#' c("\\?","\\!","#","@", "(€|euro)","(\\$|dollar)","SUPPRESSEDTEXT")}
#' @param suppressInvalidTexts Sostituisce con \code{"SUPPRESSEDTEXT"} le
#' stringhe con mutibyte non valida (che produrrebbero verosimilmente errori
#' nelle successive normalizzazioni). Default \code{TRUE}.
#' @param removeUnderscore rimuovere gli underscore?
#' @param ifErrorReturnText what to return for tests with a wrong encoding.
#' @param stopwords Lista di parole da escludere dall'analisi. A list of words
#' to be excluded from the process. \code{itastopwords} by default.
#' @param verbatim Mostra statitiche durante il processo. Default \code{TRUE}
#' @param remove \code{TRUE} by default. Possibily, a vector of stopwords to be
#' removed.
#' @return Per \code{normalizzaTesti} l'output e' il vettore di testi
#' normalizzati.  La tabella dei conteggi specificati in \code{contaStringhe}
#' e' assegnato come tabella \code{counts} tra gli \code{attributes} del
#' vettore stesso.
#' 
#' Per tutte le altre funzioni, l'output e' un \code{vector} della stessa
#' lunghezza di \code{testo} ma con testi normalizzati.
#' @note %% ~~further notes~~
#' @author Dario Solari, Livio Finos, Maddalena Branca
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' testoNorm <- normalizzaTesti(c('ciao bella!','www.associazionerospo.org','noooo, che grandeeeeee!!!!!','mitticooo', 'mai possibile?!?!'))
#' testoNorm
#' attr(testoNorm,"counts")
#' 
#' @export normalizzaTesti
#' @export normalizzaemote
#' @export normalizzaEmoticons
#' @export normalizzacaratteri
#' @export normalizzahtml
#' @export normalizzapunteggiatura
#' @export normalizzaslang
#' @export preprocessingEncoding
#' @export tryTolower 
#' @export removeStopwords

normalizzaTesti <- function(testo, tolower=TRUE,normalizzahtml=TRUE,
                            normalizzacaratteri=TRUE,
                            normalizzaemote=TRUE,
                            normalizzaEmoticons=TRUE,
                            normalizzapunteggiatura=TRUE,
                            normalizzaslang=TRUE,
                            fixed=TRUE,perl=TRUE,
                            preprocessingEncoding=TRUE, encoding="UTF-8", sub="",
                            contaStringhe=c("\\?","\\!","@","#",
                                            "(\u20AC|euro)","(\\$|dollar)",
                                            "SUPPRESSEDTEXT"),
                            suppressInvalidTexts=TRUE,
                            verbatim=TRUE, remove=TRUE){
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
  # identifica emote
  #source(paste(functiondir,"/normalizzaemote.R",sep=""), .GlobalEnv)
   
  if(normalizzaemote) testo <- normalizzaemote(testo,perl=perl)
  # pulizia punteggiatura
  #source(paste(functiondir,"/normalizzapunteggiatura.R",sep=""), .GlobalEnv)
  if(normalizzapunteggiatura) testo <- normalizzapunteggiatura(testo,perl=perl,fixed=fixed)
  if(normalizzaEmoticons) testo<-normalizzaEmoticons(testo)
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
        data(itastopwords)
        testo <- removeStopwords(testo,itastopwords)
      }
    }
  testo <- gsub("\\s+", " ", testo, perl=perl)
  testo <- .togliSpaziEsterni(testo)
  attr(testo,"counts")=conteggiStringhe
  testo
}
