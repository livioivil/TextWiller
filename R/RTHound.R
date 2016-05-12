#' RTHound
#' 
#' Identifies the most frequent retweets through hierarchical clustering on
#' Levenshtein distance (dissimilarity) matrix.
#' 
#' \code{RTHound} divides \code{testo} in subsets of length \code{S} (from the
#' second subset also incorporates \code{L} tweets of the previous subset);
#' calculate a dissimilarity matrix based on Levenshtein distance for each
#' subsets and clusterize tweets throught hierarchical clustering algorithm.
#' 
#' @param testo Tweets or generic texts vector.
#' @param S Number of tweets (or texts) for each subset. \code{500} by
#' deafault.
#' @param L Number of tweets (or texts) belonging to the previous subset to
#' embed in subset analysis. \code{100} by default.
#' @param hclust.dist Numeric scalar with height where the trees should be cut.
#' \code{100} by deafault.
#' @param hclust.method The agglomeration method to be used. This should be (an
#' unambiguous abbreviation of) one of \code{"ward"}, \code{"single"},
#' \code{"complete"}, \code{"average"}, \code{"mcquitty"}, \code{"median"} or
#' \code{"centroid"}. \code{"complete"} by default.
#' @param showTopN Number of most frequent retweets to show. \code{5} by
#' deafault.
#' @param dist \code{"levenshtein"} is the default. \code{"profile"} is the
#' other - quicker - accepted value.
#' @param verbatim logical
#' @return \code{RTHound} replaces the tweets belong to the same cluster with
#' the oldest, identifying them as retweets, and returns a list of the most
#' frequent retweets (\code{top}).
#' @note %% ~~further notes~~
#' @author Federico Ferraccioli, Livio Finos
#' @seealso \code{hclust}
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'  \dontrun{
#'  testo=c(
#'  "RT @LAVonlus: Tre miti da sfatare sulla #vivisezione. Le risposte  ai luoghi comuni della sperimentazione animale  http://t.co/zHSfam16DT",
#'  "Tre miti da sfatare sulla #vivisezione. Le risposte  ai luoghi comuni della sperimentazione animale  http://t.co/zHSfam16DT",
#'  "RT @LAVonlus: Tre miti da sfatare sulla #vivisezione. Le risposte  ai luoghi comuni della sperimentazione animale  http://t.co/zHSfam16DT",
#'  "RT @orianoPER: La #sperimentazioneanimale è inutile perché non predittiva per la specie umana. MEDICI ANTI #VIVISEZIONE- LIMAV http://t.co/" ,
#'  "La #sperimentazioneanimale è inutile perché non predittiva per la specie umana. MEDICI ANTI #VIVISEZIONE- LIMAV http://t.co/3MwubXIH8g",
#'  "RT @orianoPER: La #ricerca in #Medicina con #sperimentazioneanimale non e' predittiva per la specie umana. MEDICI ANTI #VIVISEZIONE http://t",
#'  "RT @HuffPostItalia: Il Governo italiano non fermi la sperimentazione animale. Intervista a Elena Cattaneo http://t.co/q1dm430a9j",
#'  "RT @HuffPostItalia: \"Il Governo italiano non fermi la sperimentazione animale\". Intervista a Elena Cattaneo http://t.co/q1dm430a9j",
#'  "\"Il Governo italiano non fermi la sperimentazione animale\". Intervista a Elena Cattaneo http://t.co/q1dm430a9j",
#'  "RT @orianoPER: @EnricoLetta LA #VIVISEZIONE NON SERVE: PAROLA DI GLAXO-APTUIT http://t.co/mtsHJjDIvu #StopVivisection #SperimentazioneAnima&")
#'  
#'  testo=RTHound(testo, S = 3, L = 1, 
#'                  hclust.dist = 100, hclust.method = "complete",
#'                  showTopN=3)
#' 
#' }
#' 
#' @export RTHound
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
