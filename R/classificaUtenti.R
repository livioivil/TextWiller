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