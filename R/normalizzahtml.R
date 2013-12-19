normalizzahtml <-
function(testo, ...){
	
   y = NA
   try_error = tryCatch(tolower(testo), error=function(e) e)
   # if not an error
   if (!inherits(try_error, "error"))
     testo=try_error
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
testo <- gsub("http([[:graph:]]+)"," WWWURLWWW ",testo, perl=TRUE)
testo <- gsub("www\\.([[:graph:]]+)"," WWWURLWWW ",testo, perl=TRUE)


#testo <- gsub("\n"," ",testo,fixed=TRUE)
#testo <- gsub("\t"," ",testo,fixed=TRUE)
#testo <- gsub('\\\"','"',testo,perl=TRUE)	

testo <- gsub("\u0093",'"',testo,fixed=TRUE)
testo <- gsub("\u0094",'"',testo,fixed=TRUE)

testo <- gsub("[[:blank:]]+"," ",testo, perl=TRUE)
    y <- testo
    return(y)
	}
