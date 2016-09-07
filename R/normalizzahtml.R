normalizzahtml <-
function(testo,perl=TRUE,fixed=TRUE){
	
	# fonte: http://htmlhelp.com/reference/html40/entities/special.html
testo <- gsub("&quot;",'"',testo)
testo <- gsub("&amp;","&",testo)
testo <- gsub("&lt;","<",testo)
testo <- gsub("&gt;",">",testo)
testo <- gsub("&circ;","^",testo)
testo <- gsub("&tilde;","~",testo)	
testo <- gsub("&lsquo;","'",testo)
testo <- gsub("&rsquo;","'",testo)
testo <- gsub("&ldquo;",'"',testo)	

testo <- gsub("http([[:graph:]]+)"," WWWURLWWW ",testo, perl=perl)
testo <- gsub("https([[:graph:]]+)"," WWWURLWWW ",testo, perl=perl)
testo <- gsub("www\\.([[:graph:]]+)"," WWWURLWWW ",testo, perl=perl)


#testo <- gsub("\n"," ",testo,fixed=TRUE)
#testo <- gsub("\t"," ",testo,fixed=TRUE)
#testo <- gsub('\\\"','"',testo,perl=TRUE)	

testo <- gsub("\u0093",'"',testo,fixed=fixed)
testo <- gsub("\u0094",'"',testo,fixed=fixed)

testo <- gsub("[[:blank:]]+"," ",testo, perl=perl)
testo
	}
