\name{normalizzaemote}
\alias{normalizzaemote}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
normalizzaemote(testo, ...)
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
    testo <- gsub("\\:\\)+|\\:\\-\\)+|\\:\\]+|\\:\\-\\]+|\\=\\)+|\\=\\]+|\\=\\>|\\:\\>|\\^\\^|\\^\\_+\\^|\\^\\-\\^|\\^\\o\\^|\\:[[:blank:]]\\)+|[[:blank:]]\\([[:blank:]]?\\:|\\:\\'D+", 
        " EMOTEGOOD ", testo, perl = TRUE)
    testo <- gsub("\\:d+|\\:D+|\\:\\-d+|\\:\\-D+|\\=d+|\\=D+|8d+|8D+|\\:\\'+\\)+|v\\.v", 
        " EMOTEGOOD ", testo, perl = TRUE)
    testo <- gsub("\\<3+|\342\235\244|\342\231\245", " EMOTELOVE ", 
        testo, perl = TRUE)
    testo <- gsub("\\:\\(+|\\:\\-\\(+|\\:\\[+|\\:\\-\\[+|\\=\\[+|\\=\\(+|\\:[[:blank:]]\\(|[[:blank:]]\\([[:blank:]]?\\:", 
        " EMOTEBAD ", testo, perl = TRUE)
    testo <- gsub("\\:\\'+\\(+|\\:\\'\\[|\\:\\*+|\\:\\-[\\|]", 
        " EMOTEBAD ", testo, perl = TRUE)
    testo <- gsub("\\:\\|\\:/+|\\=/+|\\:x", " EMOTEBAD ", testo, 
        perl = TRUE)
    testo <- gsub("\\#\\_+\\#|X\\_+X|x\\_+x|X\\.X|x\\.x|>\\.<|>\\_+<|>\\_+>|>\\.>", 
        " EMOTEBAD ", testo, perl = TRUE)
    testo <- gsub("\\;\\)+|\\;\\-\\)+|\\;\\]|\\;\\-\\]|\\;\\>|;d+|;D+|;o", 
        " EMOTEWINK ", testo, perl = TRUE)
    testo <- gsub("O\\.o|o\\.o|O\\.O|o\\.O|O\\_+o|o\\_+o|O\\_+O|o\\_+O|\\:OO+|\\=O+|\\-\\.\\-|u\\.u|u\\.\303\271|\303\271\\.u|u\\_+u|\303\247\303\247|\303\247_+\303\247|t_+t|\303\271\\_+\303\271|\303\271\\.\303\271|\\:oo+|0\\_+0|\\=\\_+\\=|\\.\\_+\\.|\303\262\303\262|\303\262\\_+\303\262|\\*u+\\*|\\-\\_+\\-|\303\271\303\271|\\-\\,\\-|\\-\\-\\'|\\.\\-\\.|\\'\\-\\'", 
        " EMOTESHOCK ", testo, perl = TRUE)
    testo <- gsub("\\*\\_+\\*|\\*\\-\\*|\\*\\o\\*|\\*\\.\\*", 
        " EMOTEAMAZE ", testo, perl = TRUE)
    testo <- gsub("\\:P+[^e]|\\:p+[^e]|\\=P+|\\=p+|XD+|xD+|xd+|[[:blank:]]d\\:", 
        " EMOTEAMAZE ", testo, perl = TRUE)
    testo <- gsub("#0_o", " EMOTESHOCK ", testo, perl = TRUE)
    testo <- gsub("[[:blank:]]+", " ", testo, perl = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
