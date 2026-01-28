#' Sentiment analysis
#' 
#' Assigns a sentiment score to each text in \code{text}.
#' 
#' @aliases sentiment sentimentVocabularies vocabolariMadda vocabolarioMattivio
#' @param text Vector of texts.
#' @param algorithm \code{"Mattivio"} by default, or \code{"Maddalena"}, or a function returning the sentiment score.
#' @param vocabularies \code{vocabolarioMattivio} by default if \code{algorithm == "Mattivio"}); 
#' \code{vocabolariMadda} by default if \code{algorithm == "Maddalena"}), or an object used by the algorithm.
#' @param normalizzaTesti \code{TRUE} by default.
#' @param get_labels \code{TRUE} by default. If \code{FLASE}, gives the quantitative score; 
#' if \code{TRUE}, provides the labels -1, 0, +1 (i.e. \code{sign(score)}).
#' @return An array containing a single numerical value for each element of \code{text}.
#' @note %% ~~further notes~~
#' @author Maddalena Branca, Mattia Da Pont, Livio Finos
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' sentiment(c("ciao bella", "mi piaci", "wow!!","good","casa", "farabutto!","ti odio"))
#' 
#' @export sentiment
#' 
sentiment <- function(text, algorithm="Mattivio", 
                      vocabularies=NULL,
                      normalizzaTesti=TRUE, get_labels=TRUE,...){
  if(!is.null(text)){ #se c'e' almeno un testo
    if(is.null(algorithm)) algorithm="Mattivio"
    if(is.null(vocabularies)) {
      if(algorithm=="Mattivio") {
        data(vocabolarioMattivio) 
        vocabularies=vocabolarioMattivio
        } else if(algorithm=="Maddalena") {
          data(vocabolariMadda) 
          vocabularies=vocabolariMadda
        }
    }
    
    if (isTRUE(normalizzaTesti)) {
      if (exists("normalizzaTesti", mode = "function")) {
        text <- normalizzaTesti(text, suppressInvalidTexts = FALSE, contaStringhe = NULL, ...)
      } else {
        # Fallback sul normalizzatore interno quando il legacy non è disponibile
        text <- normalize_text(text, use_legacy = FALSE, ...)
      }
    }
    
    #choose and perform algorithm
    if(is.function(algorithm)) {
      sent=algorithm(text=text, vocabularies=vocabularies,...)
      return(sent)} else 
        if(algorithm=="Maddalena") {
      sent=.sentiment.maddalena(text=text, vocabularies=vocabularies,...)
      
    } else if(algorithm=="Mattivio") {
      sent=.sentiment.mattivio(text=text, vocabularies=vocabularies,...)
    } else sent=NULL
    
    if(get_labels)
      return(sign(sent)) else
        return(sent)
    
  } #end if(!is.null(text))
}

# Legacy Mattivio algorithm using the provided vocabulary with scores
.sentiment.mattivio <- function(text, vocabularies, ...) {
  if (missing(vocabularies) || is.null(vocabularies)) {
    data(vocabolarioMattivio, envir = environment())
    vocabularies <- get("vocabolarioMattivio", envir = environment())
  }
  if (!is.data.frame(vocabularies) || !all(c("keyword", "score") %in% names(vocabularies))) {
    stop("vocabolarioMattivio non contiene colonne 'keyword' e 'score'.")
  }
  keywords <- tolower(vocabularies$keyword)
  scores <- vocabularies$score
  has_code <- "code" %in% names(vocabularies)
  
  vapply(text, function(txt) {
    txt_lower <- tolower(txt)
    tokens <- unlist(strsplit(txt_lower, "\\s+"), use.names = FALSE)
    token_matches <- match(tokens, keywords, nomatch = 0L)
    idx_token <- unique(token_matches[token_matches > 0L])
    
    idx_code <- integer(0)
    if (has_code) {
      codes <- vocabularies$code
      hit <- vapply(seq_along(codes), function(i) {
        code_val <- codes[[i]]
        if (is.na(code_val) || !nzchar(code_val)) return(FALSE)
        grepl(code_val, txt, fixed = TRUE)
      }, logical(1))
      idx_code <- which(hit)
    }
    
    idx <- unique(c(idx_token, idx_code))
    if (length(idx) == 0) return(0)
    sum(scores[idx], na.rm = TRUE)
  }, numeric(1))
}

# Legacy Maddalena algorithm: +1 for positive stems, -1 for negative stems
.sentiment.maddalena <- function(text, vocabularies, ...) {
  if (missing(vocabularies) || is.null(vocabularies)) {
    data(vocabolariMadda, envir = environment())
    vocabularies <- get("vocabolariMadda", envir = environment())
  }
  if (!is.list(vocabularies) || !all(c("positive", "negative") %in% names(vocabularies))) {
    stop("vocabolariMadda deve essere una lista con elementi 'positive' e 'negative'.")
  }
  pos <- tolower(vocabularies$positive)
  neg <- tolower(vocabularies$negative)
  
  vapply(text, function(txt) {
    tokens <- tolower(unlist(strsplit(txt, "\\W+"), use.names = FALSE))
    tokens <- tokens[nzchar(tokens)]
    if (length(tokens) == 0) return(0)
    pos_hits <- vapply(tokens, function(tok) any(startsWith(tok, pos)), logical(1))
    neg_hits <- vapply(tokens, function(tok) any(startsWith(tok, neg)), logical(1))
    sum(pos_hits) - sum(neg_hits)
  }, numeric(1))
}
