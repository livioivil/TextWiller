RTHound=function(testo, S = 500, L = 100, 
                 hclust.dist = 100, hclust.method = "complete",
                 showTopN=5, dist="levenshtein",verbatim=TRUE){ 
  testo=iconv(testo,to="UTF-8")
  testo=gsub("^( *)(RT|rt|Rt)( *)","",testo)
  testo.na=which(is.na(testo))
  ntesti=length(testo)
  if(is.null(names(testo))) names(testo)=1:length(testo)
  if(length(testo.na)>0) testo=testo[-testo.na]
  ntestiNA=length(testo)
  
  #   if(dist=="profile") {
  #     if(verbatim) cat("\n Making profile matrix..")
  #     profile=make.profile(testo)
  #   }
  #     
  nWindows=(floor(ntestiNA/S)-1)
  if(verbatim) cat("\n There will be ",nWindows, " sliding windows:")
  s=c(0:nWindows)
  for(l in 1:length(s)) {
    if(verbatim) cat("\nWindow #", l)
    if(l<length(s))  
      select=c(((S)*s[l]+1):((S)*s[l+1])) else
    if(l==length(s))  
      select=((S)*s[l]+1):length(testo)
    
    if(l>1)   {     
      selectPeriodoPrima=((S)*s[l]-(L+1)):((S)*s[l]-1)
      select=c(selectPeriodoPrima,select)
    }
#     m=matrix(ncol=length(select),nrow=length(select))          
#     for(i in 1:(length(select)-1))  {
#       for(j in (i+1):length(select)){
#         m[i,j]=levenshteinDist(testo[i],testo[j])
#       }
#     }
#     m= as.dist(t(m))                            
    if(dist=="levenshtein")
      m= as.dist(adist(testo[select]))
    if(dist=="profile")  
      m= dist(make.profile(testo[select]))


    h=hclust(dist(m),method=hclust.method)
    tree=cutree(h,h=hclust.dist)
    idClusters=sapply(unique(tree), function(x) which(tree==x))
    
    for (i in 1:length(idClusters))
      testo[names(idClusters[[i]])]=testo[names(idClusters[[i]])[1]]
  }
  
  if(showTopN>0) {
    cat("\n",showTopN," most frequent RTs:")
    out=sort(table(testo),decreasing=TRUE)[1:showTopN]
    cat("\n",
      paste("(fr ",out,") ",names(out),sep="","\n")
      )
  }
  
  if(length(testo.na)>0){
    testoOut=rep("",ntesti)
    testoOut[-testo.na]=testo
    testo=testoOut
  }
  return(testo)
}

####### util for profile-based distance
make.profile <- function(testo){
  split=strsplit(testo,"")
  profileNames=table(unlist(split))
  split=sapply(split,function(x)factor(x,levels=names(profileNames)))
  profile=t(sapply(split,function(x)table(x)))
  as.matrix(profile)
}