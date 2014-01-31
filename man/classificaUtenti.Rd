\name{classificaUtenti}
\alias{classificaUtenti}
\alias{vocabolarioNomiPropri}
\alias{vocabolarioLuoghi}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Associa i nomi in names ai valori indicati da vocabolarioNomiPropri
}
\description{
Associa i nomi in names ai valori indicati da un vocabolario. ad esempio vocabolarioNomiPropri assegna il genere e \code{data(vocabolarioLuoghi)} l'area geografica (vedi esempio)
}
\usage{
classificaUtenti(names, vocabolario = NULL, ifManyUseFirst = TRUE, NAasExtraLevel = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{names}{vettore di nomi}
  \item{vocabolarioNomiPropri}{ data.frame di una colonna con la classificazione da associare. I rownames devono essere unici (sono i nomi unici su cui viene fatto il controllo). il vocabolario fornito da noi e' \code{data(vocabolarioNomiPropri)}. ATTENZIONE, nel \code{vocabolario} usare solo lower-case e non usare mai "NA" (mentre "na" e' valido).}
\item{ifManyUseFirst}{\code{TRUE} by default. Nel caso di molteplici classificazioni, assegna alla prima categoria di \code{unique(vocabolario$categoria)}.}
\item{NAasExtraLevel}{gli NA diventano una categoria a parte.}
}
\details{
vedi esempio sotto.

Per il \code{data(vocabolarioLuoghi)} abbiamo escluso i paesi Re (800 abitanti, Nord-ovest) e Lu (1200 abitanti, Nord-ovest) perche' in conflitto con le sigle delle province.
}
\value{
caccia fuori un named vector con elementi dalla colonna \code{categoria} del data.frame \code{vocabolario}. Per \code{vocabolario=vocabolarioNomiPropri} le modalita' sono  \code{c('masc','femm','ente')}
}

\author{
Livio, Andrea Mamprin, Dario Solari
}
\examples{
 \dontrun{data(vocabolarioNomiPropri)}
 \dontrun{str(vocabolarioNomiPropri)}
classificaUtenti(c('livio','alessandra'))
data(vocabolarioLuoghi)
classificaUtenti(c('Bosa','Pordenone, Italy'),vocabolarioLuoghi)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line