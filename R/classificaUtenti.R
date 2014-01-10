classificaUtenti <-
function (names, vocabolario=NULL){
if(is.null(vocabolario)){ data(vocabolarioNomiPropri) 
                                    vocabolario=vocabolarioNomiPropri}
names=tolower(names)
classNomi=sapply(names,function(txt){ 
  class=as.character(vocabolario[match(strsplit(txt,"\\W")[[1]], row.names(vocabolario)),"categoria"])
  class=class[!is.na(class)]
  if(length(class)==0) classtab=NA else
    if(length(class)==1) classtab=class else   {
      classtab=table(class)
      classtab=names(classtab[classtab==max(classtab)])
      if(length(classtab)>1) classtab=NA
    } 
  classtab
})
classNomi
}
