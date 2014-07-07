#Legge in input un corpus
#Restituisce un corpus "pulito"

charnorm <- function(corpus,fixed=TRUE){
  
	corpus <- tm_map(corpus, function(t)  gsub("\001" ," ", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\002" ," ", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\003" ," ", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\004" ," ", corpus, fixed=fixed, useBytes=FALSE))	
	corpus <- tm_map(corpus, function(t)  gsub("\005" ," ", corpus, fixed=fixed, useBytes=FALSE))	
	corpus <- tm_map(corpus, function(t)  gsub("\006" ," ", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\007" ," ", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\016" ," ", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\017" ," ", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\020" ," ", corpus, fixed=fixed, useBytes=FALSE))	
	corpus <- tm_map(corpus, function(t)  gsub("\021" ,"!", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\022" ,'"', corpus, fixed=fixed, useBytes=FALSE))	
	corpus <- tm_map(corpus, function(t)  gsub("\023" ,' ', corpus, fixed=fixed, useBytes=FALSE))	
	corpus <- tm_map(corpus, function(t)  gsub("\027" ,"\'", corpus, fixed=fixed, useBytes=FALSE))	
	corpus <- tm_map(corpus, function(t)  gsub("\030" ,"\'", corpus, fixed=fixed, useBytes=FALSE))	
	corpus <- tm_map(corpus, function(t)  gsub("\031" ," ", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\032" ,"\'", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\033" ," ", corpus, fixed=fixed, useBytes=FALSE))	
	corpus <- tm_map(corpus, function(t)  gsub("\034" ,"\'", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\035" ,"\'", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\036" ," ", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\n"," ", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\r"," ", corpus, fixed=fixed, useBytes=FALSE))
	corpus <- tm_map(corpus, function(t)  gsub("\t"," ", corpus, fixed=fixed, useBytes=FALSE))
	#Nuove righe di codice aggiunte su altri caratteri da normalizzare.
	corpus <- tm_map(corpus, function(t) gsub(" e\' "," è ",t,fixed=TRUE))
	corpus <- tm_map(corpus, function(t) gsub("a\' ","à ",t,fixed=TRUE))
	corpus <- tm_map(corpus, function(t) gsub("u\' ","ù ",t,fixed=TRUE))
	corpus <- tm_map(corpus, function(t) gsub("i\' ","ì ",t,fixed=TRUE))
	corpus <- tm_map(corpus, function(t) gsub("[-\']"," ",t))
	corpus <- tm_map(corpus, function(t) gsub(" +"," ",t))
	corpus <- tm_map(corpus, function(t) gsub("á","à",t,fixed=TRUE))
	corpus <- tm_map(corpus, function(t) gsub("ó","ò",t,fixed=TRUE))

  corpus
}
