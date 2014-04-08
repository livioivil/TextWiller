# wekajar="/usr/local/lib/R/site-library/RWekajars/java/weka.jar"
# oldcp=Sys.getenv("CLASSPATH")
# newcp=NULL
# Sys.setenv(CLASSPATH=paste(wekajar,newcp, sep=":"))

strsplit_space_tokenizer <- function(x) unique(unlist(strsplit(x, " ")))


 clean.text <- function(some_txt){   	
#######################
## metto tutto minuscolo prima di far partire le varie funzioni

source("tryTolower.R")
some_txt = sapply(some_txt, tryTolower)

source("unicode.R")
some_txt<- .clean.unicode(some_txt)

source("toeliminate.R")
some_txt<-.toeliminate(some_txt)

source("emoticon.R")
some_txt<-.emoticon(some_txt)

source("correggiVari.R")
some_txt<- .correggiVari(some_txt)


#######################
names(some_txt) = NULL
return(some_txt)
}


#cambiare il numero di cpu
score.sentiment = function(sentence, pos.words, neg.words){

sfInit(par=T,cp=4) #cp = 10 calculus, devo metterci il numero di processori 
sfExport("sentence", "pos.words", "neg.words") 
sfLibrary(stringr)
#sfLibrary(Snowball)
sfLibrary(SnowballC)
#sfLibrary(RWeka)
scores = sfSapply(sentence, function(sentence, pos.words, neg.words) {
 # tokenaizer parole
word.list = str_split(sentence, '\\s+')  
# sometimes a list() is one level of hierarchy too much
words = unlist(word.list)
#words = SnowballStemmer(words, control=Weka_control(S="italian"))# stemmer porter
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

}, pos.words, neg.words)

sfStop() 

return(scores)
}



pulizia<-function(x2){

sfInit(par=T,cp=4) #cp = 10 calculus, devo metterci il numero di processori 
sfLibrary(stringr)
sfExport( "x2") 
x4<-sfApply(as.array(x2),1,clean.text)#clean.text(x2)#
sfStop()

stima<-as.character(x4)

## pulisco le ultime cose
source("finale.R")
array_tweet2<-apply(as.array(stima),1,.finale)

return(array_tweet2)
}

