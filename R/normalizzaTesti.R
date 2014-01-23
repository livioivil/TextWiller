normalizzaTesti <- function(testo, tolower=TRUE,normalizzahtml=TRUE,normalizzacaratteri=TRUE,
                            fixed=TRUE,perl=TRUE,encoding="UTF-8", contaStrighe=c("\\?","\\!"))
                            ){
  Sys.setlocale("LC_ALL", "")
  
  testo<-.preprocessing(testo,encoding=encoding)
	#######################
	#   PREPROCESSING     #
	#######################
  
  # aggiunta spazi per preprocessing
  testo <- paste(" ",testo," ",sep="")
  
  # pulizia testo preliminare (html)
  #source(paste(functiondir,"/normalizzahtml.R",sep=""), .GlobalEnv)
  if(normalizzahtml) testo <- normalizzahtml(testo)
  
  # tolower
  if(tolower)  testo <- tryTolower(testo,ifErrorReturnText=TRUE)
  
  # normalizza encoding
	if(normalizzacaratteri) testo <- normalizzacaratteri(testo,fixed=fixed)

  conteggiStrighe=.contaStringhe(testo,stringhe)
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
	list(testo=testo,conteggi=conteggi)
}