#' Associa i nomi in names ai valori indicati da vocabolarioNomiPropri
#' 
#' Associa i nomi in names ai valori indicati da un vocabolario. ad esempio
#' vocabolarioNomiPropri assegna il genere e \code{data(vocabolarioLuoghi)}
#' l'area geografica (vedi esempio)
#' 
#' vedi esempio sotto.
#' 
#' Per il \code{data(vocabolarioLuoghi)} abbiamo escluso i paesi Re (800
#' abitanti, Nord-ovest) e Lu (1200 abitanti, Nord-ovest) perche' in conflitto
#' con le sigle delle province.
#' 
#' @aliases classificaUtenti vocabolarioNomiPropri vocabolarioLuoghi
#' @param names vettore di nomi
#' @param vocabolario \code{data.frame} di una colonna con la classificazione
#' da associare. I \code{rownames(vocabolario)} devono essere unici (sono i
#' nomi unici su cui viene fatto il controllo). il vocabolario fornito da noi
#' e' \code{data(vocabolarioNomiPropri)}. ATTENZIONE, nel \code{vocabolario}
#' usare solo lower-case e non usare mai "NA" (mentre "na" e' valido).
#' @param ifManyUseFirst \code{TRUE} by default. Nel caso di molteplici
#' classificazioni, assegna alla prima categoria di
#' \code{unique(vocabolario$categoria)}.
#' @param NAasExtraLevel gli NA diventano una categoria a parte.
#' @return caccia fuori un named vector con elementi dalla colonna
#' \code{categoria} del data.frame \code{vocabolario}. Per
#' \code{vocabolario=vocabolarioNomiPropri} le modalita' sono
#' \code{c('masc','femm','ente')}
#' @author Livio, Andrea Mamprin, Dario Solari
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'  \dontrun{data(vocabolarioNomiPropri)}
#'  \dontrun{str(vocabolarioNomiPropri)}
#' classificaUtenti(c('livio','alessandra'))
#' data(vocabolarioLuoghi)
#' classificaUtenti(c('Bosa','Pordenone, Italy'),vocabolarioLuoghi)
#' 
#' @export classificaUtenti
classificaUtenti <-function (names, vocabolario=NULL,ifManyUseFirst=TRUE,NAasExtraLevel=FALSE){
if(is.null(vocabolario)){ data(vocabolarioNomiPropri) 
                                    vocabolario=vocabolarioNomiPropri}
names=tolower(names)
names <- .togliSpaziEsterni(names)
names <- gsub("\\d","",names) #toglie i numeri
idspazi=grep("\\W",rownames(vocabolario))
conspazi=rownames(vocabolario)[idspazi]
conspazi=conspazi[order(sapply(conspazi,nchar),decreasing =TRUE)]
for(i in conspazi){
  names=gsub(i,gsub("\\W","_",i),names)
}
rownames(vocabolario)[idspazi]=gsub("\\W","_",rownames(vocabolario)[idspazi])
classNomi=sapply(names,function(txt){ 
  class=as.character(vocabolario[match(strsplit(txt,"\\W")[[1]], row.names(vocabolario)),"categoria"])
  class=class[!is.na(class)]
  if(length(class)==0) classtab=NA else
    if(length(class)==1) classtab=class else   {
      classtab=table(class)
      if(ifManyUseFirst) 
        classtab=names(which.max(classtab)) else{
        classtab=names(classtab[classtab==max(classtab)])
        if(length(classtab)>1) classtab=NA
      }
    } 
  if(NAasExtraLevel) classtab[is.na(classtab)]="NA"
  classtab
  
})
#names(classNomi) =paste(1:length(names),names,sep="-")
classNomi
}
