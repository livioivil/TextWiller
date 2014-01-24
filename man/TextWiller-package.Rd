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
Dario Solari, Livio Finos, Matteo Redaelli, Marco Rinaldo

Maintainer: dario solari <dario.solari@gmail.com>
}
\keyword{ package }
\examples{
 \dontrun{# install.packages("devtools") # if you don't already have it.
library(devtools)
install_github("TextWiller", "livioivil")
library(TextWiller)
}

normalizzaTesti(c('ciao bella!','www.associazionerospo.org','noooo, che grandeeeeee!!!!!','mitticooo', 'è mai possibile?!?!'))

classificaUtenti(c('livio','alessandra'))

\dontrun{urls=urlextract("Influenza Vaccination | ONS - Oncology Nursing Society http://t.co/924sRKGBU9 See All http://t.co/dbtPJRMl00")}
 
}