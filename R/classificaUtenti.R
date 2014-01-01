classificaUtenti <-
function (names, vocabolario=NULL){
if(is.null(vocabolario)){ data(vocabolarioNomiPropri) 
                                    vocabolario=vocabolarioNomiPropri}
classNomi=sapply(names,function(txt){ 
  class=as.character(vocabolario[strsplit(txt," |,|'|_")[[1]],])
  class=class[!is.na(class)]
  if(length(class)==0) classtab=NA else
    if(length(class)==1) classtab=class else   {
      classtab=table(class)
      classtab=names(classtab[classtab==max(classtab)])
      if(length(class)>1) classtab=NA
    } 
  classtab
})
classNomi
}
