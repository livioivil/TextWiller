RTHound=function(testo, S = 500, L = 100, 
                 hclust.dist = 100, hclust.method = "complete",
                 showTopN=5){ 
  testo.na=which(is.na(testo))
  ntesti=length(testo)
  
  if(length(testo.na)>0) testo=testo[-testo.na]
  nWindows=(floor(length(testo)/S)-1)
  s=c(0:nWindows)
  for(l in 1:length(s)) {
    cat("\nWindow #", l)
    if(l!=length(s))  {
      ids=c(((S)*s[l]):((S)*s[l+1]))
      select=testo[ids] 
    } else {
      select=testo[((S)*s[l]):length(testo)] 
    }
    if(l>1)   {     
      selectPeriodoPrima=testo[((S)*s[l]-(L+1)):((S)*s[l]-1)] 
      select=c(selectPeriodoPrima,select)
    }
    m=matrix(ncol=length(select),nrow=length(select))          
    for(i in 1:(length(select)-1))  {
      for(j in (i+1):length(select)){
        m[i,j]=levenshteinDist(testo[i],testo[j])
      }
    }
    m= as.dist(t(m))                            
    h=hclust(dist(m),method=hclust.method)
    tree=cutree(h,h=hclust.dist)
    idClusters=sapply(unique(tree), function(x) which(tree==x))
    
    for (i in 1:length(idClusters))
      testo[idClusters[[i]]]=testo[idClusters[[i]][1]]
  }
  
  if(showTopN>0) cat("\n",showTopN," most frequent RTs\n",
                     sort(table(testo),decreasing=T)[1:showTopN])
  
  if(length(testo.na)>0){
    testoOut=rep("",ntesti)
    testoOut[-testo.na]=testo
    testo=testoOut
  }
  testo
}