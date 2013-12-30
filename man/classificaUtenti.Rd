\name{classificaUtenti}
\alias{classificaUtenti}
\alias{vocabolarioNomiPropri}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Associa i nomi in names ai valori indicati da vocabolarioNomiPropri
}
\description{
Associa i nomi in names ai valori indicati da vocabolarioNomiPropri (per esempio, assegna il genere. vedi esempio)
}
\usage{
classificaUtenti(names, vocabolarioNomiPropri = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{names}{
vettore di nomi
}
  \item{vocabolarioNomiPropri}{
data.frame di una colonna con la classificazione da associare. I row.names devono essere unici (sono i nomi propri su cui viene fatto il controllo). il vocabolario fornito da noi e' \code{data(vocabolarioNomiPropri)}
}
}
\details{
vedi esempio sotto
}
\value{
caccia fuori un named vector con elementi da c('masc','femm','ente')
}

\author{
Livio, Andrea Mamprin, Dario Solari
}
\examples{
data(vocabolarioNomiPropri)
str(vocabolarioNomiPropri)

classificaUtenti(c('livio','alessandra'))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
