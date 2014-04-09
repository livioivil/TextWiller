\name{sentiment}
\alias{sentiment}
\alias{sentimentVocabularies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performs sentiment analysis}
\description{
Assegna una sentiment per ogni testo in \code{text}
}
\usage{
sentiment(text, algorithm="Maddalena", 
          vocabularies= NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{text}{descr .}
\item{algorithm}{descr .}
\item{vocabularies}{descr .}}

\details{
aggiungere dettagli qui
}
\value{
l'output etc
}

\author{
Maddalena Branca, Livio Finos
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
sentiment(c("ciao", "ciao", "good","casa"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
