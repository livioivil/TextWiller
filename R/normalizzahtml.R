#'normalizzahtml
#'
#' 
#' \code{normalizzahtml} replaces links and URLs in \code{testo} with the keyword WWWURLWWW
#' 
#' @param testo a set of texts to be preprocessed
#' @return a set of texts where URLS have been replaced by the keyword WWWURLWWW
#' @param fixed logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param perl logical. If TRUE Perl-compatible regexps are used.
#' @author Livio Finos
#' @examples 
#' 
#'  testo<-c("http:textwiller.com","www.textwiller.com") 
#'  normalizzahtml(testo)
#'  
#'  








normalizzahtml <-
function(testo,perl=TRUE,fixed=TRUE){
	
	# fonte: http://htmlhelp.com/reference/html40/entities/special.html

  testo <- gsub("https.*"," WWWURLWWW ",testo,perl=perl)
  testo <- gsub("http.*"," WWWURLWWW ",testo,perl=perl)
  testo <- gsub("www.*"," WWWURLWWW ",testo,perl=perl)
  
  
  testo <- gsub("&quot;",'"',testo)
  testo <- gsub("&amp;","&",testo)
  testo <- gsub("&lt;","<",testo)
  testo <- gsub("&gt;",">",testo)
  testo <- gsub("&circ;","^",testo)
  testo <- gsub("&tilde;","~",testo)	
  testo <- gsub("&lsquo;","'",testo)
  testo <- gsub("&rsquo;","'",testo)
  testo <- gsub("&ldquo;",'"',testo)	




#testo <- gsub("\n"," ",testo,fixed=TRUE)
#testo <- gsub("\t"," ",testo,fixed=TRUE)
#testo <- gsub('\\\"','"',testo,perl=TRUE)	


testo <- gsub("[[:blank:]]+"," ",testo, perl=perl)
testo
	}
