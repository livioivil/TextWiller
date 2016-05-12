#' Estrazione di regular expression (e quindi users, hashtag) e shorturl
#' 
#' \code{patternExtract} estrae i pattern contenuti in in \code{testo}.
#' \code{urlExtract} estrae e converte gli shorturl contenuti in \code{testo}
#' in url. \code{shorturl2url} sostituisce gli shorturl contenuti in
#' \code{testo} in url.
#' 
#' 
#' @aliases urlExtract shorturl2url patternExtract
#' @param testo Vettore (eventualmente con nomi) di testi contenenti shorturl.
#' @param pattern stringa di testo da cercare ed estrarre. \code{"@\\w+"}
#' (default) estrae i riferimenti ad uno user nei tweets. \code{"#\\w+"} estrae
#' gli hashtag.
#' @param id se \code{testo} e' un vettore con nomi, questi vengono presi come
#' id. In caso contrario, gli id sono numeri progressivi da 1 a length(testo)
#' @return \code{patternExtract} restituisce un \code{data.frame} con
#' colonne:id, pattern
#' 
#' \code{urlExtract} restituisce un \code{data.frame} con colonne:id, shorturl
#' e url
#' @note %% ~~further notes~~
#' @author Dario Solari, Livio Finos
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'  \dontrun{
#'  testo=c("Influenza Vaccination | ONS - Oncology Nursing Society http://t.co/924sRKGBU9 See All http://t.co/dbtPJRMl00,See All http://t.co/dbtPJRMl00")
#'  shorturl2url(testo,id=names(testo))
#' urls=urlExtract(testo)
#' patternExtract(c("@luca @paolo: buon giorno!", "@matteo: a te!"), pattern="@\w+")
#' }
#' 

urlExtract <- function(testo,id=names(testo)){

  if(is.null(id)) id=1:length(testo)
	urls    <- str_extract_all(testo, "http([[:graph:]]+)|www\\.([[:graph:]]+)")
	
	db.urls <- data.frame(cbind(id=rep(id, unlist(lapply(urls,length)) ),shorturl=unlist(urls)))
	db.urls$id <- as.character(db.urls$id)
	db.urls$shorturl <- as.character(db.urls$shorturl)
	db.urls <- db.urls[which(nchar(db.urls$shorturl)>21),]
	
	db.urls$shorturl[ grep("^(http://t.co)",db.urls$shorturl, perl=TRUE) ] <- substr(db.urls$shorturl[ grep("^(http://t.co)",db.urls$shorturl, perl=TRUE) ],1,22)
	db.urls$shorturl[ grep("^(https://t.co)",db.urls$shorturl, perl=TRUE) ] <- substr(db.urls$shorturl[ grep("^(https://t.co)",db.urls$shorturl, perl=TRUE) ],1,23)
	
  uniqueShorturl <- unique(db.urls$shorturl)
  uniqueUrl <- as.character(sapply(db.urls$shorturl, decode_short_url,USE.NAMES = FALSE))
  for( i in 1:length(uniqueShorturl)){
    db.urls$url[db.urls$shorturl==uniqueShorturl[i]] <- uniqueUrl[i]
  }
	db.urls$url[which(db.urls$url=="NULL")] <- NA
	return(db.urls)	
}