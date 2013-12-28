normalizzacaratteri <-
function(testo,fromToCharTab=NULL){
	# fonte: http://www.utf8-chartable.de/
	#        http://www.utf8-chartable.de/unicode-utf8-table.pl?start=4736&number=128&utf8=string-literal&unicodeinhtml=dec
  if(is.null(fromToCharTab)) fromToCharTab=data(gsubCharTab)
	for (i in nrow(fromToCharTab)) testo <- gsub(fromToCharTab[i,"from"],fromToCharTab[i,"to"], testo, fixed=TRUE, useBytes=TRUE)
  testo
	}