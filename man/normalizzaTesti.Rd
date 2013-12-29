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
normalizzacaratteri(testo)
normalizzapunteggiatura(testo)
normalizzaslang(testo)
normalizzahtml(testo)
normalizzaemote(testo)
tryTolower(testo)
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
}
\details{
aggiungere dettagli qui
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
