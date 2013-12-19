classificaUtenti <-
function (names, vocabolarioNomiProri){
data(vocabolarioNomiProri)
classNomi=sapply(names,function(txt){ 
  class=as.character(vocabolarioNomiProri[strsplit(txt," |,|'|_")[[1]],])
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
