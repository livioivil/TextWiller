#' Vocabolari
#' 
#' TextWiller contains various Italian dictionaries for text analysis. This document 
#' describes their uses based on their primary function: sentiment analysis,
#' text normalization, categorization.
#'
#' @name vocabolari
#' @docType data
#' @references
#' Gupta, S., Singh, A., & Ranjan, J. (2021). Emoji Score and Polarity Evaluation 
#' Using CLDR Short Name and Expression Sentiment. Advances in Intelligent Systems 
#' and Computing, 1009-1016. \url{https://doi.org/10.1007/978-3-030-73689-7_95}\cr
#' 
#' Kralj Novak, P., Smailovic, J., Sluban, B., & Mozetic, I. (2015). Sentiment 
#' of Emojis. PLOS ONE, 10(12), e0144296. \url{https://doi.org/10.1371/journal.pone.0144296}\cr
#' 
#' Katherine Roehrick (2020). vader: Valence Aware Dictionary and sEntiment Reasoner 
#' (VADER). R package version 0.2.1. \url{https://CRAN.R-project.org/package=vader}\cr
#' 
#' Tim Loughran & Bill McDonald. (n.d.). Loughran-McDonald Master Dictionary w/ 
#' Sentiment Word Lists [Dataset]. \url{https://sraf.nd.edu/loughranmcdonald-master-dictionary/}\cr
#' 
#' Rinker, T. W. (2018). lexicon: Lexicon Data version 1.2.1. 
#' \url{http://github.com/trinker/lexicon}\cr
#' 
#' Rinker, T. W. (2021). sentimentr: Calculate Text Polarity Sentiment 
#' version 7.1.2. \url{https://github.com/trinker/sentimentr}
#' 
#' @keywords vocabolari, dizionari
#' 
#' @section Sentiment analysis:
#' 
#' \enumerate{
#' \item \code{dizionario_sentiment_ita}:
#' This dictionary contains 3179 general Italian words and 1853 emojis. \cr
#' Type: data.table.
#' \itemize{
#' \item \code{keyword}: Italian word or English emoji identifier, 
#' the latter in the form "emoji_cldr_short_name". \cr
#' Type: character.
#' \item \code{code}: For emojis, their UTF-8 byte representation in the form <xx><xx><xx>. 
#' For words, its value is NA. \cr
#' Type: character.
#' \item \code{score}: Polarity score. Range = -1, +1 where -1 = negative polarity 
#' and +1 = positive polarity. Words are classified as either negative (-1) or positive (+1).
#' Emojis are classified using the full range of values (with 0 = neutral). 
#' Emoji scores were calulated using Gupta et al.'s (2021) procedure: 
#' 1) Polarity scores for 676 emojis were retreived from  \pkg{lexicon::emojis_sentiment} (Rinker, 2019), 
#' a slightly modified version of Novak et al.'s (2015) emoji sentiment data; 
#' 2) Using \pkg{VADER} (Roehrick, 2020)), an additional polarity score was 
#' calculated for all 1853 emojis through a sentiment analysis of their CLDR short name; 
#' 3) The final polarity score therefore is: the VADER score for emojis that 
#' do not appear in  \pkg{lexicon::emojis_sentiment} (n = 1177); the arithmetic mean 
#' of the \pkg{VADER} and \pkg{lexicon::emojis_sentiment} scores for the remaining emojis (n = 676). 
#' All emojis were retrieved from: \url{http://www.unicode.org/emoji/charts/full-emoji-list.html}.\cr 
#' Type: numeric.
#' \item \code{ngram}: Order of the n-gram appearing in the "keyword" variable, 
#' calculated as the number of words that compose it. \cr
#' Type: integer.}
#' 
#' \item \code{dizionario_sentiment_ita_r}: A version of \code{dizionario_sentiment_ita}
#' compatible with \pkg{sentimentr::sentiment}.\cr
#' Type: data.table.
#' \itemize{
#' \item \code{x}: Word.
#' \item \code{y}: Sentiment score.}
#' 
#' \item \code{dizionario_loughran_ita}: 
#' Italian translation of the Loughran-McDonald dictionary for use with finalcial documents. \cr
#' Type: data.table.
#' \itemize{
#' \item \code{x}: Word.\cr
#' Type: character.
#' \item \code{y}: Polarity score (values: -1 = negative, 0 = uncertain, +1 = positive). \cr
#' Type: numeric.}}
#' 
#' @section Text normalization:
#' 
#' \enumerate{
#' \item \code{dizionario_emoji_id}:
#' This dataset contains 1853 emojis and their respective byte representation. 
#' It is compatible with \pkg{textclean::replace_emoji()} and can be used as a 
#' value for the \code{emoji_dt} argument for text normalization, returning emojis 
#' in a document as their CLDR short name. It represents an expansion of the 
#' \code{lexicon::hash_emoji} dataset.\cr
#' Type: data.table. 
#' \itemize{
#' \item \code{x}: UTF-8 byte representation in the form <xx><xx><xx>. \cr
#' Type: character.
#' \item \code{y}: Emoji identifier in the form "emoji_cldr_short_name". \cr
#' Type: character.}
#' 
#' \item \code{stopwords_ita}: A character list of italian stopwords for use in text normalization.\cr
#' Type: character.}
#' 
#' @section Categorization:
#' 
#' \enumerate{
#' \item \code{dizionario_luoghi}:
#' As a value of the \code{vocabolario} argument in \pkg{TextWiller::classificaUtenti()}, 
#' it categorizes city names contained in a character vector according to their 
#' location ("Estero" for non-Italian cities, "Nord-ovest", "Nord-est". "Centro", 
#' "Sud", "Isole" for Italian cities).
#' 
#' \item \code{dizionario_nomi_propri}:
#' As a value of the \code{vocabolario} argument in \pkg{TextWiller::classificaUtenti()},
#' it categorizes (Italian) first names contained in a character vector as 
#' either "Maschio (male)" or "Femmina (female)".
#' 
NULL