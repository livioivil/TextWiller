\name{extract}
\alias{urlExtract}
\alias{patternExtract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{converte gli shorturl in url}
\description{converte gli shorturl in url contenuti in \code{testo}.
}
\usage{urlExtract(testo, id = names(testo))}
\usage{patternExtract(testo, pattern="@\\w+", id = names(testo))}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{testo}{Vettore (eventualmente con nomi) di testi contenenti shorturl.}
\item{pattern}{stringa di testo da cercare ed estrarre. \code{\"@\\\\w+\"} (default) estrae i riferimenti ad uno user nei tweets. \code{\"#\\\\w+\"} estrae gli hashtag.}
\item{id}{se \code{testo} e' un vettore con nomi, questi vengono presi come id. In caso contrario, gli id sono numeri progressivi da 1 a length(testo)}
}
\details{
aggiungere dettagli qui
}
\value{
\code{patternExtract} restituisce un \code{data.frame} con colonne:id, pattern

\code{urlExtract} restituisce un \code{data.frame} con colonne:id, shorturl e url
}

\author{
Dario Solari, Livio Finos
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
 \dontrun{urls=urlExtract("Influenza Vaccination | ONS - Oncology Nursing Society http://t.co/924sRKGBU9 See All http://t.co/dbtPJRMl00")}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
