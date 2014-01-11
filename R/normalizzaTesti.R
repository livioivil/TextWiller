normalizzaTesti <- function(testo, tolower=TRUE,normalizzahtml=TRUE){
  Sys.setlocale("LC_ALL", "C")
	Sys.setlocale("LC_TIME", "C")
  
	#######################
	#   PREPROCESSING     #
	#######################

	# normalizza encoding
	testo <- normalizzacaratteri(testo)

	# aggiunta spazi per preprocessing
	testo <- paste(" ",testo," ",sep="")

	# tolower
	if(tolower)	testo <- tryTolower(testo)
  
	# pulizia testo preliminare (html)
	#source(paste(functiondir,"/normalizzahtml.R",sep=""), .GlobalEnv)
	if(normalizzahtml) testo <- normalizzahtml(testo)

	# identifica emote
	#source(paste(functiondir,"/normalizzaemote.R",sep=""), .GlobalEnv)
	testo <- normalizzaemote(testo)

	# pulizia punteggiatura
	#source(paste(functiondir,"/normalizzapunteggiatura.R",sep=""), .GlobalEnv)
	testo <- normalizzapunteggiatura(testo)

	# normalizza slang 
	#source(paste(functiondir,"/normalizzaslang.R",sep=""), .GlobalEnv)
	testo <- normalizzaslang(testo)
	
	testo
}