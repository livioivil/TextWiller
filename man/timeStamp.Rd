\name{TimeStamp}
\alias{fixTimeStamp}
\alias{selezionaIntervalloTimeStamp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Funzioni di gestione delle date
}
\description{
Funzioni di gestione delle date
}
\usage{
fixTimeStamp(db, campoData = "created", timeRange = NULL)
selezionaIntervalloTimeStamp(db, timeRange = range(db$created) , campoData = "created")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{db}{data.frame contenente i tweets.}
\item{timeRange}{due valori di tipo data indicanti inizio e fine.}
\item{campoData}{ "created" e "ts" sono due campi data del db estratto da dump_twitter.R}
}
\details{
aggiungere dettagli qui
}
\value{
l'output e' db "aggiustato"
}

\author{
Dario Solari, Livio Finos
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
 \dontrun{TW=fixTimeStamp(TW)}
 \dontrun{TW=selezionaIntervallo(TW,as.POSIXct(c("2013-12-27 17:54:42 CET", "2013-12-27 22:33:38 CET")))}
 \dontrun{TW$created.round <- as.POSIXct(round(t$created,"hour"))}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
