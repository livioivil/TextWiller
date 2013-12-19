\name{normalizzahtml}
\alias{normalizzahtml}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
normalizzahtml(testo, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{testo}{
%%     ~~Describe \code{testo} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (testo, ...) 
{
    y = NA
    try_error = tryCatch(tolower(testo), error = function(e) e)
    if (!inherits(try_error, "error")) 
        testo = try_error
    testo <- gsub("&quot;", "\"", testo)
    testo <- gsub("&amp;", "&", testo)
    testo <- gsub("&lt;", "<", testo)
    testo <- gsub("&gt;", ">", testo)
    testo <- gsub("&circ;", "^", testo)
    testo <- gsub("&tilde;", "~", testo)
    testo <- gsub("&lsquo;", "'", testo)
    testo <- gsub("&rsquo;", "'", testo)
    testo <- gsub("&ldquo;", "\"", testo)
    testo <- gsub("http([[:graph:]]+)", " WWWURLWWW ", testo, 
        perl = TRUE)
    testo <- gsub("www\\.([[:graph:]]+)", " WWWURLWWW ", testo, 
        perl = TRUE)
    testo <- gsub("<U+0093>", "\"", testo, fixed = TRUE)
    testo <- gsub("<U+0094>", "\"", testo, fixed = TRUE)
    testo <- gsub("[[:blank:]]+", " ", testo, perl = TRUE)
    y <- testo
    return(y)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
