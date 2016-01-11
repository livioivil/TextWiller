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
  normalizzacaratteri = TRUE, normalizzaemote = TRUE,
  normalizzapunteggiatura = TRUE, normalizzaslang = TRUE, 
  fixed = TRUE, perl = TRUE, 
  preprocessingEncoding=TRUE, encoding="UTF-8", sub = "",
  contaStringhe = c("\\\\?","\\\\!","@","#",
  "(\\u20AC|euro)","(\\\\$|dollar)", "SUPPRESSEDTEXT"),
  suppressInvalidTexts=TRUE,
  verbatim=TRUE, remove = TRUE)  

preprocessingEncoding(testo, encoding="UTF-8",
                           suppressInvalidTexts=TRUE, verbatim = TRUE, sub = "")

normalizzacaratteri(testo, fixed = TRUE)
normalizzapunteggiatura(testo, removeUnderscore = TRUE, perl = TRUE, fixed = TRUE)
normalizzaslang(testo, perl = TRUE)
normalizzahtml(testo, perl = TRUE, fixed = TRUE)
normalizzaemote(testo, perl = TRUE)
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
\item{normalizzaemote}{\code{TRUE} by default}
\item{normalizzapunteggiatura}{\code{TRUE} by default}
\item{normalizzaslang}{\code{TRUE} by default}
\item{fixed}{ vedi \code{\link[base:gsub]{base:gsub}}. Preferibilmente non usare l'opzione.}
\item{perl}{ vedi \code{\link[base:gsub]{base:gsub}}. Preferibilmente non usare l'opzione.}
\item{preprocessingEncoding}{logical}
\item{encoding}{ \code{"UTF-8"} default. Se \code{FALSE} evita la conversione.}
\item{sub}{ character string. If not NA it is used to replace any non-convertible bytes in the input. See also parameter \code{sub} in function \code{iconv}.}
\item{contaStringhe}{stringhe da contare nei documenti. Default: \code{ c("\\\\?","\\\\!","#","@", "(€|euro)","(\\\\$|dollar)","SUPPRESSEDTEXT")}}
\item{suppressInvalidTexts}{Sostituisce con \code{"SUPPRESSEDTEXT"} le stringhe con mutibyte non valida (che produrrebbero verosimilmente errori nelle successive normalizzazioni). Default \code{TRUE}.}
\item{removeUnderscore}{rimuovere gli underscore? }
\item{ifErrorReturnText}{what to return for tests with a wrong encoding.}
\item{stopwords}{Lista di parole da escludere dall'analisi. A list of words to be excluded from the process. \code{itastopwords} by default.}
\item{verbatim}{Mostra statitiche durante il processo. Default \code{TRUE}}
\item{remove}{\code{TRUE} by default. Possibily, a vector of stopwords to be removed.}
}
\details{
\code{itastopwords} e' una lista di stopwords italiane.
}
\value{Per \code{normalizzaTesti} l'output e' il vettore di testi normalizzati. 
La tabella dei conteggi specificati in \code{contaStringhe} e' assegnato come tabella \code{counts} tra gli \code{attributes}  del vettore stesso.

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
testoNorm <- normalizzaTesti(c('ciao bella!','www.associazionerospo.org','noooo, che grandeeeeee!!!!!','mitticooo', 'mai possibile?!?!'))
testoNorm
attr(testoNorm,"counts")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
