\name{normalizzaslang}
\alias{normalizzaslang}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
normalizzaslang(testo, ...)
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
    testo <- gsub(" (#?zz+|#?u+ff[aif]+?|#?r+o+n+f+|#uff|ronf) ", 
        " EMOTEZZZ ", testo, perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" (#?sii+|#si+|#?yes+|#?s\303\254\303\254+) ", 
        " EMOTESIII ", testo, perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" (#?noo+|#no+|#?nuu+) ", " EMOTENOOO ", testo, 
        perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" (#?ahh+) ", " EMOTEAHHH ", testo, perl = TRUE, 
        ignore.case = TRUE)
    testo <- gsub(" (#?ehh+) ", " EMOTEEHHH ", testo, perl = TRUE, 
        ignore.case = TRUE)
    testo <- gsub(" (#?o?h?o+h[oh]+) ", " EMOTEOHOH ", testo, 
        perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" (#?i?h?i+h[ih]+) ", " EMOTEIHIH ", testo, 
        perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" (#?i?h?i+h[ih]+) ", " EMOTEUHUH ", testo, 
        perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" (#?a?h?ah[^h][ah]+) ", " EMOTEAHAH ", testo, 
        perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" (#?e?h?eh[^h][eh]+) ", " EMOTEEHEH ", testo, 
        perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" (#?azz+) ", " EMOTEAZZ ", testo, perl = TRUE, 
        ignore.case = TRUE)
    testo <- gsub(" (#?[dv]aii+|#[dv]ai|forzaa+) ", " EMOTEDAIII ", 
        testo, perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" (#?cazz+) ", " cazzo ", testo, perl = TRUE, 
        ignore.case = TRUE)
    testo <- gsub(" (#?aaa+) ", " EMOTEAAA ", testo, perl = TRUE, 
        ignore.case = TRUE)
    testo <- gsub(" (#?ooo+) ", " EMOTEOOO ", testo, perl = TRUE, 
        ignore.case = TRUE)
    testo <- gsub(" (#?eee+) ", " EMOTEEEE ", testo, perl = TRUE, 
        ignore.case = TRUE)
    testo <- gsub(" (#?l+o+l+|#?r+o+f+t+l+) ", " EMOTELOL ", 
        testo, perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" (#?aiutoo+|#?sos|#?help+) ", " EMOTESOS ", 
        testo, perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" (#ba+sta+|ba+staa+) ", " EMOTEBASTA ", testo, 
        perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" #?([uw]+[ao]+[uw]+) ", " EMOTEWOW ", testo, 
        perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" (#?grandee+|#?grandii+) ", " grandeee ", 
        testo, perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" #?(grand(issim)[aeoi]+) ", " grandissimo ", 
        testo, perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" #?(brav(issim)?[aeoi][aeoi]+) ", " bravooo ", 
        testo, perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" #?(brav(issim)[aeoi]+) ", " bravissimo ", 
        testo, perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" #?(bel(issim)[aeoi]+) ", " bellissimo ", 
        testo, perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" #?(mitic([aio]|he)+) ", " mitico ", testo, 
        perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" #?(graziee+) ", " grazieee ", testo, perl = TRUE, 
        ignore.case = TRUE)
    testo <- gsub(" #?(stronzoo+) ", " stronzooo ", testo, perl = TRUE, 
        ignore.case = TRUE)
    testo <- gsub(" perch[\303\251e\303\250] ", " perch\303\250 ", 
        testo, perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" x(ch|k)[\303\251e\303\250] ", " perch\303\250 ", 
        testo, perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" nn ", " non ", testo, perl = TRUE, ignore.case = TRUE)
    testo <- gsub(" nun ", " non ", testo, perl = TRUE, ignore.case = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
