\name{normalizzaTesti}
\alias{normalizzaTesti}
\alias{normalizzacaratteri}
\alias{normalizzaemote}
\alias{normalizzahtml}
\alias{normalizzaslang}
\alias{normalizzapunteggiatura}
\alias{tryTolower}
\alias{itastopwords}
\alias{removeStopwords}
\alias{preprocessingEncoding}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Varie funzioni di normalizzazione del testo
}
\description{
Varie funzioni di normalizzazione del testo
}
\usage{
normalizzaTesti(testo, tolower = TRUE, normalizzahtml = TRUE, 
  normalizzacaratteri = TRUE, fixed = TRUE, perl = TRUE, 
  preprocessingEncoding=TRUE, encoding="UTF-8",
  contaStringhe = c("\\\\?","\\\\!","@","#","\\\\?",                                               "(\\u20AC|euro)","(\\\\$|dollar)",                                           "SUPPRESSEDTEXT"),
  suppressInvalidTexts=TRUE,
  verbatim=TRUE)  

preprocessingEncoding(testo, encoding="UTF-8",
                           suppressInvalidTexts=TRUE, verbatim = TRUE)

normalizzacaratteri(testo, removeUnderscore=TRUE, fixed = TRUE, fixed = TRUE)
normalizzapunteggiatura(testo,perl = TRUE,fixed = TRUE)
normalizzaslang(testo,perl = TRUE)
normalizzahtml(testo,perl = TRUE,fixed = TRUE)
normalizzaemote(testo,perl = TRUE)
tryTolower(testo, ifErrorReturnText = FALSE)
removeStopwords(testo, stopwords = itastopwords)
data(itastopwords)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{testo}{character vector of texts}
\item{tolower}{\code{TRUE} by default}
\item{normalizzahtml}{\code{TRUE} by default}
\item{normalizzacaratteri}{\code{TRUE} by default}
\item{fixed}{ vedi \code{\link[base:gsub]{base:gsub}}. Preferibilmente non usare l'opzione.}
\item{perl}{ vedi \code{\link[base:gsub]{base:gsub}}. Preferibilmente non usare l'opzione.}
\item{preprocessingEncoding}{}
\item{encoding}{ \code{"UTF-8"} default. Se \code{FALSE} evita la conversione.}
\item{contaStringhe}{stringhe da contare nei documenti. Default: \code{ c("\\\\?","\\\\!","#","@", "(€|euro)","(\\\\$|dollar)","SUPPRESSEDTEXT")}}
\item{suppressInvalidTexts}{Sostituisce con \code{"SUPPRESSEDTEXT"} le stringhe con mutibyte non valida (che produrrebbero verosimilmente errori nelle successive normalizzazioni). Default \code{TRUE}.}
\item{verbatim}{Mostra statitiche durante il processo. Default \code{TRUE}}
}
\details{
\code{itastopwords} e' una lista di stopwords italiane.
}
\value{Per \code{normalizzaTesti} l'output  e' una \code{list} con due oggetti: \code{$testo} e' un vettore contenente i testi normalizzati, \code{$conteggi} e' una matrice con i conteggi indicati dal parametro \code{contaStringhe}.

Per tutte le altre funzioni, l'output e' un \code{vector} della stessa lunghezza di \code{testo} ma con testi normalizzati.
}

\author{
Dario Solari, Livio Finos, Maddalena Branca
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
normalizzaTesti(c('ciao bella!','www.associazionerospo.org','noooo, che grandeeeeee!!!!!','mitticooo', 'mai possibile?!?!'))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
