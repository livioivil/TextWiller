shorturl2url <- function(testo,id=names(testo)){
  if(is.null(id)) {
    id=1:length(testo) 
    names(testo)=id
  }
  db.urls=urlExtract(testo,id=id)
  
  for(i in 1:nrow(db.urls)){
    testo[db.urls$id[i]]=gsub(db.urls$shorturl[i],db.urls$url[i],testo[db.urls$id[i]])
  }
  testo
}