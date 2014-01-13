\name{normalizzaTesti}
\alias{normalizzaTesti}
\alias{normalizzacaratteri}
\alias{normalizzaemote}
\alias{normalizzahtml}
\alias{normalizzaslang}
\alias{normalizzapunteggiatura}
\alias{tryTolower}
\alias{.itastopwords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Varie funzioni di normalizzazione del testo
}
\description{
Varie funzioni di normalizzazione del testo
}
\usage{
normalizzaTesti(testo, tolower = TRUE, normalizzahtml = TRUE)
normalizzacaratteri(testo,fixed=TRUE)
normalizzapunteggiatura(testo,perl=TRUE,fixed=TRUE)
normalizzaslang(testo,perl=TRUE)
normalizzahtml(testo,perl=TRUE,fixed=TRUE))
normalizzaemote(testo,perl=TRUE)
tryTolower(testo, ifErrorReturnText=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{testo}{
charachter vector of texts
}
  \item{tolower}{
TRUE/FALSE
}
  \item{normalizzahtml}{
TRUE/FALSE
}
\item{fixed}{ vedi \code{\link[base:gsub]{base:gsub}})}
\item{perl}{ vedi \code{\link[base:gsub]{base:gsub}})}
}
\details{
aggiungere dettagli qui
}
\value{
l'output e' un \code{vector} della stessa lunghezza di \code{testo} ma con testi normalizzati.
}

\author{
Dario Solari, Livio Finos
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
normalizzaTesti(c('ciao bella! <3 :)','www.associazionerospo.org'))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
