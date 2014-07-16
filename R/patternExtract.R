patternExtract <- function(testo, pattern="@( *)\\w+", id = names(testo)){
  
  if(is.null(id)) id=1:length(testo)
     pattern <- str_extract_all(testo, pattern)
  names(pattern)=id
  
  pattern=pattern[sapply(pattern,length)>0]
  db.pattern=lapply(1:length(pattern),function(i){ rbind(id=names(pattern)[i],pattern=unlist(pattern[i]))})
  db.pattern=data.frame(t(data.frame(db.pattern)))
  rownames(db.pattern)=NULL
  return(db.pattern)	
}