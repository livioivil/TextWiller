\name{RTHound}
\alias{RTHound}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RTHound} 
\description{Identifies the most frequent retweets through hierarchical clustering on Levenshtein distance (dissimilarity) matrix.}
\usage{ 
RTHound(testo, S = 500, L = 100, 
                 hclust.dist = 100, hclust.method = "complete",
                 showTopN=5, dist="levenshtein", verbatim=TRUE)
}
\arguments{
\item{testo}{Tweets or generic texts vector.}
\item{S}{Number of tweets (or texts) for each subset. \code{500} by deafault.}
\item{L}{Number of tweets (or texts) belonging to the previous subset to embed in subset analysis. \code{100} by default.}
\item{hclust.dist}{Numeric scalar with height where the trees should be cut. \code{100} by deafault.}
\item{hclust.method}{The agglomeration method to be used. This should be (an unambiguous abbreviation of) one of \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"}, \code{"mcquitty"}, \code{"median"} or \code{"centroid"}. \code{"complete"}  by default.}
\item{showTopN}{Number of most frequent retweets to show. \code{5} by deafault.}
\item{dist}{\code{"levenshtein"} is the default. \code{"profile"} is the other - quicker - accepted value.}
\item{verbatim}{logical}
}
\details{
\code{RTHound} divides \code{testo} in subsets of length \code{S} (from the second subset also incorporates \code{L} tweets of the previous subset); calculate a dissimilarity matrix based on Levenshtein distance for each subsets and clusterize tweets throught hierarchical clustering algorithm. 
}
\value{
\code{RTHound} replaces the tweets belong to the same cluster with the oldest, identifying them as retweets, and returns a list of the most frequent retweets (\code{top}).
}
\author{
Federico Ferraccioli, Livio Finos
}
\note{
%%  ~~further notes~~
}
\seealso{\code{hclust}}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
 \dontrun{
 testo=c(
 "RT @LAVonlus: Tre miti da sfatare sulla #vivisezione. Le risposte  ai luoghi comuni della sperimentazione animale  http://t.co/zHSfam16DT",
 "Tre miti da sfatare sulla #vivisezione. Le risposte  ai luoghi comuni della sperimentazione animale  http://t.co/zHSfam16DT",
 "RT @LAVonlus: Tre miti da sfatare sulla #vivisezione. Le risposte  ai luoghi comuni della sperimentazione animale  http://t.co/zHSfam16DT",
 "RT @orianoPER: La #sperimentazioneanimale è inutile perché non predittiva per la specie umana. MEDICI ANTI #VIVISEZIONE- LIMAV http://t.co/" ,
 "La #sperimentazioneanimale è inutile perché non predittiva per la specie umana. MEDICI ANTI #VIVISEZIONE- LIMAV http://t.co/3MwubXIH8g",
 "RT @orianoPER: La #ricerca in #Medicina con #sperimentazioneanimale non e' predittiva per la specie umana. MEDICI ANTI #VIVISEZIONE http://t",
 "RT @HuffPostItalia: Il Governo italiano non fermi la sperimentazione animale. Intervista a Elena Cattaneo http://t.co/q1dm430a9j",
 "RT @HuffPostItalia: \"Il Governo italiano non fermi la sperimentazione animale\". Intervista a Elena Cattaneo http://t.co/q1dm430a9j",
 "\"Il Governo italiano non fermi la sperimentazione animale\". Intervista a Elena Cattaneo http://t.co/q1dm430a9j",
 "RT @orianoPER: @EnricoLetta LA #VIVISEZIONE NON SERVE: PAROLA DI GLAXO-APTUIT http://t.co/mtsHJjDIvu #StopVivisection #SperimentazioneAnima&")
 
 testo=RTHound(testo, S = 3, L = 1, 
                 hclust.dist = 100, hclust.method = "complete",
                 showTopN=3)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
