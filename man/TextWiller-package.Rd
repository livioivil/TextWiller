\name{TextWiller-package}
\alias{TextWiller-package}
\alias{TextWiller}
\docType{package}
\title{
 Collection of functions for text mining, specially devoted to the italian language
}
\description{
 Collection of functions for text mining, specially devoted to the italian language
}
\details{
\tabular{ll}{
Package: \tab TextWiller\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-12-19\cr
License: GPL (>= 2)
}
}
\author{
Dario Solari, Andrea Sciandra, Marco Rinaldo, Matteo Redaelli, Livio Finos (con contributi di Marco Rinaldo, Maddalena Branca, Federico Ferraccioli).

Maintainer: dario solari <dario.solari@gmail.com>
}
\keyword{ package }
\examples{
 \dontrun{# install.packages("devtools") # if you don't already have it.
library(devtools)
install_github("livioivil/TextWiller")
library(TextWiller)
}


### normalize texts
normalizzaTesti(c('ciao bella!','www.associazionerospo.org','noooo, che grandeeeeee!!!!!','mitticooo', 'mai possibile?!?!'))

# get the sentiment of a document
sentiment(c("ciao bella!","farabutto!","fofi sei figo!"))


# Classify users' gender by (italian) names
classificaUtenti(c('livio','alessandra','andrea'))
# and classify location
data(vocabolarioLuoghi)
classificaUtenti(c('Bosa','Pordenone, Italy','Milan'),vocabolarioLuoghi)


# find re-tweets (RT) by texts similarity:
data(TWsperimentazioneanimale)
RTHound(TWsperimentazioneanimale[1:10,"text"], S = 3, L = 1, 
                 hclust.dist = 100, hclust.method = "complete",
                 showTopN=3)


#extract short urls and get the long ones
## Not run: urls=urlExtract("Influenza Vaccination | ONS - Oncology Nursing Society http://t.co/924sRKGBU9 See All http://t.co/dbtPJRMl00")

#extract short urls and get the long ones
\dontrun{urls=urlExtract("Influenza Vaccination | ONS - Oncology Nursing Society http://t.co/924sRKGBU9 See All http://t.co/dbtPJRMl00")}

#extract users:
## Not run: patternExtract(c("@luca @paolo: buon giorno!", "@matteo: a te!"), pattern="@\\w+")

}