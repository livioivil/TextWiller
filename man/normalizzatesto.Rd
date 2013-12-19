\name{normalizzatesto}
\alias{normalizzatesto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
normalizzatesto(testo, ...)
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
    testo <- gsub("\303\240", "\303\240", testo, fixed = TRUE, 
        useBytes = TRUE)
    testo <- gsub("\303\250", "\303\250", testo, fixed = TRUE, 
        useBytes = TRUE)
    testo <- gsub("\303\251", "\303\251", testo, fixed = TRUE, 
        useBytes = TRUE)
    testo <- gsub("\303\254", "\303\254", testo, fixed = TRUE, 
        useBytes = TRUE)
    testo <- gsub("\303\255", "i", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\303\262", "\303\262", testo, fixed = TRUE, 
        useBytes = TRUE)
    testo <- gsub("\303\272", "\303\271", testo, fixed = TRUE, 
        useBytes = TRUE)
    testo <- gsub("\001", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\002", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\003", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\004", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\005", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\006", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\a", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\016", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\017", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\020", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\021", "!", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\022", "\"", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\023", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\027", "'", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\030", "'", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\031", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\032", "'", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\033", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\034", "'", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\035", "'", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\036", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\037", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\200", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\201", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\202", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\203", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\204", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\205", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\206", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\207", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\210", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\211", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\212", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\213", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\214", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\215", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\216", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\217", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\220", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\221", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\222", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\223", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\224", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\225", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\226", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\227", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\230", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\231", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\232", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\233", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\234", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\235", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\236", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\237", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\303\254\240", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\240", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\241", "!", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\242", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\243", "L", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\244", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\245", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\246", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\247", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\250", "\"", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\251", "C", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\252", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\253", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\254", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\255", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\256", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\257", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\260", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\261", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\262", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\263", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\264", "'", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\265", "u", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\266", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\267", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\270", ",", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\271", "1", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\272", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\273", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\274", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\275", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\276", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\277", "?", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\300", "A", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\301", "A", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\302", "\303\240", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\303", "\303\250", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\304", "a", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\305", "A", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\306", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\307", "\303\247", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\310", "\303\250", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\311", "\303\251", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\312", "E", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\313", "E", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\314", "\303\254", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\315", "I", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\316", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\317", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\340", "\303\240", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\341", "a", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\342", "a", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\343", "a", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\344", "a", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\345", "a", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\346", "ae", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\347", "\303\247", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\350", "\303\250", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\351", "\303\251", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\352", "e", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\353", "e", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\354", "\303\254", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\355", "i", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\356", "i", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\357", "i", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\320", "D", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\321", "N", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\322", "O", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\323", "O", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\324", "O", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\325", "O", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\326", "O", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\327", "x", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\330", "O", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\331", "U", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\332", "U", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\333", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\334", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\335", "Y", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\336", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\337", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\360", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\361", "n", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\362", "\303\262", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\363", "o", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\364", "o", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\365", "o", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\366", "o", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\370", "o", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\371", "\303\271", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\372", "\303\271", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\373", "\303\271", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\374", "u", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\375", "y", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\376", "y", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("(<U+3E35393C> ? <U+3E35393C>)", " ", testo, 
        fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\n", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\r", " ", testo, fixed = TRUE, useBytes = TRUE)
    testo <- gsub("\t", " ", testo, fixed = TRUE, useBytes = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
