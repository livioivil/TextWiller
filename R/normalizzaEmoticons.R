

normalizzaEmoticons<-function(testo){
  data(vocabolarioEmote)
  for(i in 1:nrow(vocabolarioEmote)){
    testo<-gsub(vocabolarioEmote[,1],vocabolarioEmote[,2],testo)
  }
  testo
}