\name{normalizzaTesti}
\alias{normalizzaTesti}
\alias{normalizzacaratteri}
\alias{normalizzaemote}
\alias{normalizzahtml}
\alias{normalizzaslang}
\alias{normalizzapunteggiatura}
\alias{tryTolower}
\alias{.itastopwords}
\alias{removeStopwords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Varie funzioni di normalizzazione del testo
}
\description{
Varie funzioni di normalizzazione del testo
}
\usage{
normalizzaTesti(testo, tolower = TRUE, normalizzahtml = TRUE, normalizzacaratteri = TRUE, fixed = TRUE, perl = TRUE, encoding = "UTF-8", contaStrighe = c("\\?","\\!","@","#"))                            
normalizzacaratteri(testo,fixed = TRUE)
normalizzapunteggiatura(testo,perl = TRUE,fixed = TRUE)
normalizzaslang(testo,perl = TRUE)
normalizzahtml(testo,perl = TRUE,fixed = TRUE))
normalizzaemote(testo,perl = TRUE)
tryTolower(testo, ifErrorReturnText = FALSE)
removeStopwords(testo, stopwords = .itastopwords)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{testo}{character vector of texts}
\item{tolower}{TRUE/FALSE}
\item{normalizzahtml}{ TRUE/FALSE}
\item{normalizzacaratteri}{=TRUE}
\item{fixed}{ vedi \code{\link[base:gsub]{base:gsub}}. Preferibilmente non usare l'opzione.}
\item{perl}{ vedi \code{\link[base:gsub]{base:gsub}}. Preferibilmente non usare l'opzione.}
\item{encoding}{ \code{"UTF-8"} default. Se \code{FALSE} evita la conversione.}
\item{contaStrighe}{=c("\\?","\\!","@","#")}

}
\details{
\code{.itastopwords} e' l'unione di \code{\link[tm:stopwords]{stopwords('italian')}} con altre parole italiane.
}
\value{
Per \code{normalizzaTesti} l'output \e' una \code{list} con due oggetti: \code{$testo} e' un vettore contenente i testi normalizzati, \code{$conteggi} e' una matrice con i conteggi indicati dal parametro \code{contaStrighe}.

Per tutte le altre funzioni, l'output e' un \code{vector} della stessa lunghezza di \code{testo} ma con testi normalizzati.
}

\author{
Dario Solari, Livio Finos
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
normalizzaTesti(c('ciao bella!','www.associazionerospo.org','noooo, che grandeeeeee!!!!!','mitticooo', 'è mai possibile?!?!'))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
