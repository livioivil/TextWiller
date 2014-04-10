.sentiment.maddalena<- function(text, vocabularies){
  text<-normalizzaTesti(text,suppressInvalidTexts=FALSE,contaStringhe=NULL)$testo
  #####################################################sentiment analisys
  
# sfInit(par=T,cp=1) #cp = 10 calculus, devo metterci il numero di processori 
# sfExport("text", "pos.words", "neg.words") 
# sfLibrary(stringr)
#sfLibrary(Snowball)
# sfLibrary(SnowballC)
#sfLibrary(RWeka)
.get.scores <- function(txt, pos.words, neg.words) {
  # tokenaizer parole
  word.list = str_split(txt, '\\s+')  
  # sometimes a list() is one level of hierarchy too much
  words = unlist(word.list)
  words = wordStem(words, language = "italian")
  # compara le parole nei dizionari
  pos_matches =match (words, pos.words)
  neg_matches =match (words, neg.words)
  ##bgrammi
  pos_matches2<-apply(as.array(cbind(words,c(words[-1],NA))),1,function(x)match(paste(x[1],"-",x[2],sep=""),pos.words))[-c(length(words))]
  neg_matches2<-apply(as.array(cbind(words,c(words[-1],NA))),1,function(x)match(paste(x[1],"-",x[2],sep=""), neg.words))[-c(length(words))]
  
  pos_matches3<-apply(as.array(cbind(words,c(words[-1],NA),c(words[-c(1:2)],NA,NA))),1,function(x)match(paste(x[1],"-",x[2],"-",x[3],sep=""), pos.words))[-c((length(words)-1):length(words))]
  neg_matches3<-apply(as.array(cbind(words,c(words[-1],NA),c(words[-c(1:2)],NA,NA))),1,function(x)match(paste(x[1],"-",x[2],"-",x[3],sep=""), neg.words))[-c((length(words)-1):length(words))]
  
  pos_matches <-c(pos_matches ,pos_matches2,pos_matches3)
  neg_matches<-c(neg_matches,neg_matches2,neg_matches3)
  # TRUE/FALSE:
  pos_matches = !is.na(pos_matches)
  neg_matches = !is.na(neg_matches)
  
  p<-sum(pos_matches) #-sum(p_neutral_matches)
  n<-sum(neg_matches) #-sum(n_neutral_matches)
  score = sign( p - n )
  return(as.array(as.vector(score)))  
}
scores = sapply(text, .get.scores, vocabularies$positive, vocabularies$negative)
return(scores)
}