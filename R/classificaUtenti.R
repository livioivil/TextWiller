classificaUtenti <-function (names, vocabolario=NULL){
if(is.null(vocabolario)){ data(vocabolarioNomiPropri) 
                                    vocabolario=vocabolarioNomiPropri}
names=tolower(names)
names <- .togliSpaziEsterni(names)
idspazi=grep("\\W[^_]",rownames(vocabolario))
conspazi=rownames(vocabolario)[idspazi]
conspazi=conspazi[order(sapply(conspazi,nchar),decreasing =TRUE)]
for(i in conspazi){
  names=gsub(i,gsub("\\W","_",i),names)
}
rownames(vocabolario)[idspazi]=gsub("\\W","_",rownames(vocabolario)[idspazi])
classNomi=sapply(names,function(txt){ 
  class=as.character(vocabolario[match(strsplit(txt,"\\W[^_]")[[1]], row.names(vocabolario)),"categoria"])
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