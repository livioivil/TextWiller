#' User classification
#' 
#' The functions associates names in \code{names} to the items in a dictionary. E.g.,
#' \code{data(dizionario_nomi_propri)} classifies names based on gender and \code{data(dizionario_luoghi)}
#' classifies a city's geographic area (vedi esempio).
#' 
#' 
#' In \code{data(dizionario_luoghi)} the towns of Re (800
#' residents, north-west) and Lu (1200 residents, north-west) were excluded as 
#' they were conflicting with provinces' abbreviations.
#' 
#' (update 05-09-2016) For names detection, we intriduced 
#' three different classification options: the first two look for words divided by blanks, 
#' while the third also finds character strings within words using the function \code{grepl}. 
#' The parameters that define this part of the classification process are described below.
#' 
#' @aliases classificaUtenti dizionario_nomi_propri dizionario_luoghi
#' @param names Vector containing names.
#' @param vocabolario Single column \code{data.frame} used for the classification. 
#' \code{rownames(vocabolario)} must be unique (these are the values that will be used for classification). 
#' For names, the dictionary \code{data(dizionario_nomi_propri)} can be used. NOTE: In \code{vocabolario}
#' use lower case only and do not use "NA" ("na" is accepted).
#' @param scan_interno If \code{TRUE}, the function also performs the (\code{grepl} internal scan on the dictionaries.
#' @param vocab_interno Dictionary used for internal scan. By default, 
#' it is equal to all vocabulary names of length >= 5.
#' @param how_class Defines classification in ambiguous cases, i.e. when 
#' multiple matches are found for a single name. We have defined three possible cases:
#' \code{modeFirst} (default) uses a recognized modal category and, in case of multimodality,
#' the first match; \code{first} performs classification using the first match, 
#' and \code{last} performs classification using the last match.
#' @param cat_interna \code{NULL} (default) allows to identify one or more classification categories such that
#' all values of a dictionary are used in the internal scan (instead of just those with length >= 5).
#' @return A named vector with elements from the \code{categoria} columns of the 
#' \code{vocabolario} data.frame. For \code{vocabolario=dizionario_nomi_propri}, the levels are
#' \code{c('masc','femm','ente')}.
#' @author Mattia Uttini, Livio Finos, Andrea Mamprin, Dario Solari
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' \dontrun{data(dizionario_nomi_propri)}
#' \dontrun{str(dizionario_nomi_propri)}
#' classificaUtenti(c('livio','alessandra','alessandraRossi', 'mariobianchi'), scan_interno=TRUE)
#' \dontrun{data(dizionario_luoghi)}
#' classificaUtenti(c('Bosa','Pordenone, Italy'), dizionario_luoghi)
#' 
#' @export classificaUtenti
classificaUtenti <- function(names, vocabolario=NULL, scan_interno=FALSE, vocab_interno=NULL, how_class="modeFirst", cat_interna=NULL){
  
  if (is.factor(names)){
    names <- as.character(names)
  }
  
  
  if (is.null(vocabolario)) {
    data(vocabolario_nomi_propri)
    vocabolario = vocabolario_nomi_propri
  }
  
  if(scan_interno&is.null(vocab_interno)){
    vocab_interno <- vocabolario[which(nchar((rownames(vocabolario))>=5)|(substr(rownames(vocabolario),1,1)==".")|(vocabolario[,1]%in%cat_interna)),,drop=FALSE]
  }
  
  Encoding(names) <- "UTF-8"
  names <- iconv(names, "UTF-8", "UTF-8", sub='')
  nomi_originali <- .togliSpaziEsterni(names)
  
  names <- tolower(names)
  
  idspazi = grep("\\W", rownames(vocabolario))
  conspazi = rownames(vocabolario)[idspazi]
  conspazi = conspazi[order(sapply(conspazi, nchar), decreasing = TRUE)]
  for (i in conspazi) {
    names = gsub(i, gsub("\\W", "_", i), names)
  }
  rownames(vocabolario)[idspazi] = gsub("\\W", "_", rownames(vocabolario)[idspazi])
  

  classifica <- function(class,how_class){
    
    if(how_class=="modeFirst"){
      ct <- table(class)
      return(ifelse(sum(ct==max(ct))==1, names(ct)[which.max(ct)], class[1]))
    }
    
    if(how_class=="first") return(class[1])
    
    if(how_class=="last") return(class[length(class)])
    
  }
  
  
  ## prima classificazione
  txt1 <- names
  classificazione <- sapply(txt1, function(txt){
    cl <- as.character(vocabolario[match(strsplit(txt, split="[[:punct:][:space:]]")[[1]], row.names(vocabolario)), "categoria"])
    cl <- cl[!is.na(cl)]
    return(ifelse(length(cl)==0,NA,classifica(cl, how_class)))
  }
  )
  
  ## seconda classificazione su NA della prima
  na1 <- is.na(classificazione)
  txt2 <- nomi_originali[na1]
  txt2 <- gsub('([[:upper:]])',  ' \\1', txt2)
  txt2 <- tolower(txt2)
  txt2 <- gsub("\\d", "", txt2)
  classificazione[which(na1)] <- sapply(txt2, function(txt){
    cl <- as.character(vocabolario[match(strsplit(txt, "\\W")[[1]], row.names(vocabolario)), "categoria"])
    cl <- cl[!is.na(cl)]
    return(ifelse(length(cl)==0,NA,classifica(cl, how_class)))
  }
  )
  
  ## terza classificazione, se scan_interno==T
  if(scan_interno){
    na2 <- is.na(classificazione)
    txt3 <- nomi_originali[na2]
    txt3 <- tolower(txt3)
    classificazione[which(na2)] <- sapply(txt3, function(txt){
      cl <- as.character(vocab_interno$categoria[sapply(ifelse(substr(rownames(vocab_interno),1,1)==".",paste0("\\",rownames(vocab_interno)),rownames(vocab_interno)), function(x) grepl(x, txt))])
      return(ifelse(length(cl)==0,NA,classifica(cl, how_class)))
    }
    )
  }
  
  return(unlist(classificazione))
  
}
