urlextract <- function(testo,id=names(testo)){

  if(is.null(id)) id=1:length(testo)
	urls    <- str_extract_all(testo, "http([[:graph:]]+)|www\\.([[:graph:]]+)")
	
	db.urls <- data.frame(cbind(id=rep(id, unlist(lapply(urls,length)) ),shorturl=unlist(urls)))
	db.urls$id <- as.character(db.urls$id)
	db.urls$shorturl <- as.character(db.urls$shorturl)
	db.urls <- db.urls[which(nchar(db.urls$shorturl)>21),]
	
	db.urls$shorturl[ grep("^(http://t.co)",db.urls$shorturl, perl=TRUE) ] <- substr(db.urls$shorturl[ grep("^(http://t.co)",db.urls$shorturl, perl=TRUE) ],1,22)
	db.urls$shorturl[ grep("^(https://t.co)",db.urls$shorturl, perl=TRUE) ] <- substr(db.urls$shorturl[ grep("^(https://t.co)",db.urls$shorturl, perl=TRUE) ],1,23)
	
	db.urls$url <- as.character(sapply(db.urls$shorturl, decode_short_url,USE.NAMES = FALSE))
	db.urls$url[which(db.urls$url=="NULL")] <- NA
	
	return(db.urls)	
}