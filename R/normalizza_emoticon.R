#' Normalize emoticons
#' 
#' Using regexes, finds emoticons (e.g.,: :), :D) in a text and substitutes them with a keyword.
#' 
#' @param testo Character vector of texts.
#' @param perl See \code{\link[base:gsub]{base:gsub}}. Preferably do no use.
#' 
#' @examples
#' test_emoticon <- c(":)", ":(", ";)", "*_*", ":P", "O_o", "a   b")
#' normalizza_emoticon(test_emoticon, perl = T)

normalizza_emoticon <-
  function(testo, perl = TRUE){
    testo <- gsub("([:=8]([- '])?[])Dd>]+)|(\\^[-_o]?\\^)",
                  "emoticon_good", 
                  testo, 
                  perl = perl)
    testo <- gsub("([:=]([- '])?[(|/x*[])|([>Xx#][._][>Xx<#])|(\\):)",
                  "emoticon_bad", 
                  testo, 
                  perl = perl)
    testo <- gsub(";-?[])>Ddo]",
                  "emoticon_wink", 
                  testo)
    testo <- gsub("\\*[-._o]\\*",
                  "emoticon_amazed", 
                  testo)
    testo <- gsub("([:=]-?[pP]+)|(\\b[xX][dD]+\\b)|(\\bd:\\b)",
                  "emoticon_joke", 
                  testo, 
                  perl = perl)
    testo <- gsub("[0Oo]+[\\._-]+[0Oo]+",
                  "emoticon_shock", 
                  testo, 
                  perl = perl)
    testo <- gsub("[[:blank:]]+",
                  " ", 
                  testo, 
                  perl = perl) # substitute multiple blank spaces with a single blank space
    testo
  }

