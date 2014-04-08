sentiment <- function(text, algorithm="Maddalena", vocabularies=NULL){
  if(!is.null(text)){ #se c'e' almeno un testo
    if(is.null(algorithm)) algorithm="Maddalena"
    if(is.null(vocabularies)) {
      # data(sentimentVocabularies) #lo decommenti ed eventualmente cambi nomi dei dati
      load("../data/dizionariMadda.rda")
      vocabularies=sentimentVocabularies
    }
    
    #choose and perform algorithm
    if(algorithm=="Maddalena") {
      sent=.sentiment.maddalena(text=text, vocabularies=vocabularies)
      return(sent)
    } else NULL
  } #end if(!is.null(text))
}

.sentiment.maddalena<- function(text, vocabularies){
  
  source("funzioniMadda.R")
  
  #pos.words=lapply((vocabularies$positive),SnowballStemmer,control=Weka_control(S="italian"))
  #neg.words=lapply((vocabularies$negative),SnowballStemmer,control=Weka_control(S="italian"))
  pos.words=lapply((vocabularies$positive),wordStem,language="italian")
  neg.words=lapply((vocabularies$negative),wordStem,language="italian")
  
  pos.words = unique(pos.words)
  neg.words = unique(neg.words)
  
  tweet_array2<-pulizia(text)
  
  
  #####################################################sentiment analisys
  result1= score.sentiment((tweet_array2), pos.words, neg.words) #,p.neutral.words,n.neutral.words)
  #write.table(result1[1:30,],"risultatiMio.txt")
  
  
}
