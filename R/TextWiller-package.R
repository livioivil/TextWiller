#' A package for text mining, specially devoted to the italian
#' language
#' 
#' A package for text mining, specially devoted to the italian
#' language
#' 
#' \tabular{ll}{ Package: \tab TextWiller\cr Type: \tab Package\cr Version:
#' \tab 2.0\cr Date: \tab 2016-05-12\cr License: GPL (>= 2) }
#' 
#' @name TextWiller-package
#' @aliases TextWiller-package TextWiller
#' @docType package
#' @author Dario Solari, Andrea Sciandra, Matteo Redaelli, Livio
#' Finos (con contributi di Mattia Uttini, Marco Rinaldo, Maddalena Branca, Federico
#' Ferraccioli).
#' 
#' Maintainer: dario solari <dario.solari@@gmail.com>
#' @keywords package
#' @examples
#' 
#'  \dontrun{# install.packages("devtools") # if you don't already have it.
#' library(devtools)
#' install_github("livioivil/TextWiller")
#' library(TextWiller)
#' }
#' 
#' 
#' ### normalize texts
#' normalizzaTesti(c('ciao bella!','www.associazionerospo.org','noooo, che grandeeeeee!!!!!','mitticooo', 'mai possibile?!?!'))
#' 
#' # get the sentiment of a document
#' sentiment(c("ciao bella!","farabutto!","fofi sei figo!"))
#' 
#' 
#' # Classify users' gender by (italian) names
#' classificaUtenti(c('livio','alessandra','andrea'))
#' # and classify location
#' data(vocabolarioLuoghi)
#' classificaUtenti(c('Bosa','Pordenone, Italy','Milan'),vocabolarioLuoghi)
#' 
#' 
#' # find re-tweets (RT) by texts similarity:
#' data(TWsperimentazioneanimale)
#' RTHound(TWsperimentazioneanimale[1:10,"text"], S = 3, L = 1, 
#'                  hclust.dist = 100, hclust.method = "complete",
#'                  showTopN=3)
#' 
#' 
#' #extract short urls and get the long ones
#' ## Not run: urls=urlExtract("Influenza Vaccination | ONS - Oncology Nursing Society http://t.co/924sRKGBU9 See All http://t.co/dbtPJRMl00")
#' 
#' #extract short urls and get the long ones
#' \dontrun{urls=urlExtract("Influenza Vaccination | ONS - Oncology Nursing Society http://t.co/924sRKGBU9 See All http://t.co/dbtPJRMl00")}
#' 
#' #extract users:
#' ## Not run: patternExtract(c("@luca @paolo: buon giorno!", "@matteo: a te!"), pattern="@\w+")
#' 
#' 
NULL
#' Funzioni di gestione delle date
#' 
#' Funzioni di gestione delle date
#' 
#' aggiungere dettagli qui
#' 
#' @aliases fixTimeStamp selezionaIntervalloTimeStamp
#' @param db data.frame contenente i tweets.
#' @param timeRange due valori di tipo data indicanti inizio e fine.
#' @param campoData "created" e "ts" sono due campi data del db estratto da
#' dump_twitter.R
#' @return l'output e' db "aggiustato"
#' @note %% ~~further notes~~
#' @author Dario Solari, Livio Finos
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'  \dontrun{TW=fixTimeStamp(TW)}
#'  \dontrun{TW=selezionaIntervallo(TW,as.POSIXct(c("2013-12-27 17:54:42 CET", "2013-12-27 22:33:38 CET")))}
#'  \dontrun{TW$created.round <- as.POSIXct(round(t$created,"hour"))}
#' 
#' 
#' 
#' Funzioni di gestione delle date
#' 
#' Funzioni di gestione delle date
#' 
#' aggiungere dettagli qui
#' 
#' @aliases fixTimeStamp selezionaIntervalloTimeStamp
#' @param db data.frame contenente i tweets.
#' @param timeRange due valori di tipo data indicanti inizio e fine.
#' @param campoData "created" e "ts" sono due campi data del db estratto da
#' dump_twitter.R
#' @return l'output e' db "aggiustato"
#' @note %% ~~further notes~~
#' @author Dario Solari, Livio Finos
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#'  \dontrun{TW=fixTimeStamp(TW)}
#'  \dontrun{TW=selezionaIntervallo(TW,as.POSIXct(c("2013-12-27 17:54:42 CET", "2013-12-27 22:33:38 CET")))}
#'  \dontrun{TW$created.round <- as.POSIXct(round(t$created,"hour"))}
#' 
#' 
#' 


