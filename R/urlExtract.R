#' Extrects regexes (users, hashtags) and short URLs.
#' 
#' \code{patternExtract} extracts patterns contained in \code{testo}.
#' \code{urlExtract} extracts and converts the short URLs contained in \code{testo}
#' to URLs. \code{shorturl2url} substitutes the short URLs in \code{testo} with URLs.
#' 
#' 
#' @aliases urlExtract shorturl2url patternExtract
#' @param testo Vector (potentially with names) of texts containing short URLs.
#' @param pattern Text string to find and extract. \code{"@\\w+"}
#' (default) extracts references to a user in tweets. \code{"#\\w+"} extracts hashtags.
#' @param id If \code{testo} is a vector containing names, these are used as IDs. 
#' Otherwise, IDs are consecutive numbers from 1 to \code{length(testo)}.
#' @return \code{patternExtract} restituisce a \code{data.frame} with columns: id, pattern.
#' \code{urlExtract} returns a \code{data.frame} with columns: id, short URL, URL.
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
#' @export urlExtract 
#' @export shorturl2url 
#' @export patternExtract

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