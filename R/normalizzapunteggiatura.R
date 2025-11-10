#' normalizzapunteggiatura
#'
#' \code{normalizzapunteggiatura} Removes escape characters and punctation from a set of texts.
#' 
#' @param testo A set of texts to be stripped of escape characters and punctation.
#' @param removeUnderscore  Logical. If \code{TRUE}, removes underscore characters ("_") from texts.
#' @param fixed Logical. If \code{TRUE}, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param perl Logical. If \code{TRUE}, Perl-compatible regexes are used.
#' @return A set of processed texts.
#' @author Livio Finos
#' @examples 
#'
#'  testo<-c(testo<-c("@ ","@retweet","# ","#ciao")) 
#'  normalizzapunteggiatura(testo)
#'  
#'  

normalizzapunteggiatura <-
function(testo,removeUnderscore=TRUE, perl=TRUE,fixed=TRUE){
	
	testo <- paste(" ",testo," ", sep="")
	testo <- gsub("#\\s+", "#", testo, perl=perl)
	testo <- gsub("#", " #", testo, perl=perl)
	testo <- gsub("@\\s+", "@", testo, perl=perl)	
	testo <- gsub("@", " @", testo, perl=perl)
	
	testo <- gsub("\n",' ',testo,fixed=fixed)
	testo <- gsub("\t",' ',testo,fixed=fixed)
	testo <- gsub("\r",' ',testo,fixed=fixed)
	testo <- gsub("\\s+", " ", testo, perl=perl)	
  
#	testo <- gsub('^( ["][@]| [@]| \034[@])',' RT @', testo, perl=perl)	
#	testo <- gsub("^( ['][@])",' RT @', testo, perl=perl)
#	testo <- gsub("^( RT: @)",' RT @', testo, perl=perl)
#	testo <- gsub("^( RT '@)",' RT @', testo, perl=perl)			
	
  
  testo <- gsub('[//?]+',' ', testo, perl=perl)
	testo <- gsub('[//!]+',' ', testo, perl=perl)
	
  
  testo <- gsub('[///]+',' ', testo, perl=perl)
	testo <- gsub('[//?]+',' ', testo, perl=perl)
	testo <- gsub('[//!]+',' ', testo, perl=perl)
	testo <- gsub('[//|]+',' ', testo, perl=perl)
	testo <- gsub("'"," ",testo,fixed=fixed)
	testo <- gsub("’"," ",testo,fixed=fixed)
	testo <- gsub("‘"," ",testo,fixed=fixed)
	testo <- gsub('"',' ',testo,fixed=fixed)
	testo <- gsub('\uc294|\uc293|\ucc8f|\ucc8e|\ucc8b|\ucbb6|\ucbb5|\ucbae|\ucb9d|\ucaba',' ',testo)
	testo <- gsub('\uc2bb|\uc2ab',' ',testo,fixed=fixed)
	testo <- gsub('\uc291|`|\uc292',' ',testo)	
	testo <- gsub('\uc285',' ',testo,fixed=fixed)	
	testo <- gsub("\\\\"," ",testo)
	testo <- gsub("\\/"," ",testo)
	testo <- gsub("[=]"," ",testo,perl=perl)
	testo <- gsub("[+]"," ",testo,perl=perl)
	testo <- gsub("[-]"," ",testo,perl=perl)
	testo <- gsub('\\*{1}',' ', testo, perl=perl)
	testo <- gsub('«',' ', testo, perl=perl)
	testo <- gsub('»',' ', testo, perl=perl)
	testo <- gsub("[<]"," ",testo,perl=perl)
	testo <- gsub("[>]"," ",testo,perl=perl)
	testo <- gsub("[~]"," ",testo,perl=perl)
	testo <- gsub("[\\^]"," ",testo,perl=perl)
	testo <- gsub("[,]"," ",testo,perl=perl)
	testo <- gsub("[;]"," ",testo,perl=perl)
	testo <- gsub("[:]"," ",testo,perl=perl)
	testo <- gsub('[\\.]',' ', testo, perl=perl)
	testo <- gsub('…',' ', testo, perl=perl)
	testo <- gsub("[&]"," & ",testo,perl=perl)
	# parentesi
	testo <- gsub("[(]"," ",testo,perl=perl)
	testo <- gsub("[)]"," ",testo,perl=perl)
	testo <- gsub("[{]"," ",testo,perl=perl)
	testo <- gsub("[}]"," ",testo,perl=perl)
	testo <- gsub("[[]"," ",testo,perl=perl)
	testo <- gsub("[]]"," ",testo,perl=perl)

  if(removeUnderscore) 
    testo=gsub("_", " ",testo, perl=perl)
    
	testo <- gsub("\\s+", " ", testo, perl=perl)
testo
	}
