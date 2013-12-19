\name{classificaUtenti}
\alias{classificaUtenti}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
classificaUtenti(names, vocabolarioNomiProri)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{names}{
%%     ~~Describe \code{names} here~~
}
  \item{vocabolarioNomiProri}{
%%     ~~Describe \code{vocabolarioNomiProri} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (names, vocabolarioNomiProri) 
{
    data(nomi)
    classNomi = sapply(names, function(txt) {
        class = as.character(nomi[strsplit(txt, " |,|'|_")[[1]], 
            ])
        class = class[!is.na(class)]
        if (length(class) == 0) 
            classtab = NA
        else if (length(class) == 1) 
            classtab = class
        else {
            classtab = table(class)
            classtab = names(classtab[classtab == max(classtab)])
            if (length(class) > 1) 
                classtab = NA
        }
        classtab
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
