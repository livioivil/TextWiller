\name{RTHound}
\alias{RTHound}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{title RTHound}
\description{description RTHound
}
\usage{
RTHound(testo, S = 500, L = 100, 
                 hclust.dist = 100, hclust.method = "complete",
                 showTopN=5)
}
\arguments{
\item{testo}{Vettore (eventualmente con nomi) di testi contenenti shorturl.}
\item{S}{\code{500} by deafault.}
\item{L}{\code{100} by deafault.}
\item{hclust.dist}{\code{100} by deafault.}
\item{hclust.method}{\code{"complete"}  by deafault.}
\item{showTopN}{\code{5} by deafault.}
}
%\details{
%}
\value{
\code{RTHound} returns etc
}
\author{
Federico Ferraccioli, Livio Finos
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
 \dontrun{
 #modifichi l'esempio con tw veri, anche se pochi.
 testo=c("uno","RT uno","due","due","uno","RTtre","uno","quattro","tre","uno")
 RTHound(testo, S = 3, L = 1, 
                 hclust.dist = 3, hclust.method = "complete",
                 showTopN=3)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line