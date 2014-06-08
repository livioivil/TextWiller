normalizzaTesti <- function(testo, tolower=TRUE,normalizzahtml=TRUE,
                             normalizzacaratteri=TRUE,fixed=TRUE,perl=TRUE,
                             preprocessingEncoding=TRUE, encoding="UTF-8",
                            contaStringhe=c("\\?","\\!","@","#",
                                           "(\u20AC|euro)","(\\$|dollar)",
                                           "SUPPRESSEDTEXT"),
                            suppressInvalidTexts=TRUE,
                             verbatim=TRUE){
  Sys.setlocale("LC_ALL", "")
  if(preprocessingEncoding)  testo<-preprocessingEncoding(testo,encoding=encoding,
                               suppressInvalidTexts=suppressInvalidTexts,
                               verbatim=verbatim)
	#######################
	#   PREPROCESSING     #
	#######################
  
  # aggiunta spazi per preprocessing
  testo <- paste(" ",testo," ",sep="")
  
  
  # normalizza encoding
  if(normalizzacaratteri) testo <- normalizzacaratteri(testo,fixed=fixed)
  
  # pulizia testo preliminare (html)
  if(normalizzahtml) testo <- normalizzahtml(testo)
  
  # tolower
  if(tolower)  testo <- tryTolower(testo,ifErrorReturnText=TRUE)
  
  if(!is.null(contaStringhe)) 
    conteggiStringhe=.contaStringhe(testo,contaStringhe) else 
    conteggiStringhe=NULL
	# identifica emote
	#source(paste(functiondir,"/normalizzaemote.R",sep=""), .GlobalEnv)
	testo <- normalizzaemote(testo,perl=perl)

	# pulizia punteggiatura
	#source(paste(functiondir,"/normalizzapunteggiatura.R",sep=""), .GlobalEnv)
	testo <- normalizzapunteggiatura(testo,perl=perl,fixed=fixed)

  
	# normalizza slang 
	#source(paste(functiondir,"/normalizzaslang.R",sep=""), .GlobalEnv)
	testo <- normalizzaslang(testo,perl=perl)

  testo <- gsub("\\s+", " ", testo, perl=perl)
  testo <- .togliSpaziEsterni(testo)
  attr(testo,"counts")=conteggiStringhe
  testo
}