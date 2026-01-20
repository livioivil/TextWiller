#' Sentiment Analysis Functions for Italian Text
#' 
#' Integrates original TextWiller sentiment analysis with enhanced features
#' 

#' Italian Sentiment Analysis
#' 
#' @param text Character vector of texts to analyze
#' @param algorithm Sentiment algorithm to use
#' @param normalizzaTesti Whether to normalize text before analysis
#' @param use_legacy Whether to use original TextWiller functions
#' @return Sentiment scores
#' @export
analyze_sentiment_it <- function(text, algorithm = "Mattivio", normalizzaTesti = TRUE, use_legacy = TRUE) {
  mc <- match.call()
  original_text <- text
  if (use_legacy && exists("sentiment")) {
    # Usa la funzione originale, se presente
    result <- sentiment(text, algorithm = algorithm, normalizzaTesti = normalizzaTesti)
  } else {
    # Usa sempre il dizionario completo presente in data/
    result <- sentiment_dictionary_scores(text)
  }
  
  log_reproducibility_action(
    module = "analysis",
    operation = "analyze_sentiment_it",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(original_text),
    output_state = list(length = length(result), summary = summary(result))
  )
  
  result
}

#' Scoring basato sul dizionario di sentiment (data/dizionario_sentiment_ita)
#' @noRd
sentiment_dictionary_scores <- function(text) {
  dict <- load_sentiment_dictionary()
  keywords <- tolower(dict$keyword)
  scores <- dict$score
  vapply(text, function(txt) {
    tokens <- strsplit(tolower(txt), "\\s+")[[1]]
    matches <- match(tokens, keywords)
    sum(scores[matches], na.rm = TRUE)
  }, numeric(1))
}

#' Carica il dizionario di sentiment completo (errore se mancante)
#' @noRd
load_sentiment_dictionary <- function() {
  if (!exists("dizionario_sentiment_ita", envir = environment(), inherits = FALSE)) {
    if (exists("dizionario_sentiment_ita", envir = .GlobalEnv, inherits = FALSE)) {
      assign("dizionario_sentiment_ita", get("dizionario_sentiment_ita", envir = .GlobalEnv), envir = environment())
    } else {
      data(dizionario_sentiment_ita, package = "TextWiller3", envir = environment())
    }
  }
  dict <- get("dizionario_sentiment_ita", envir = environment(), inherits = FALSE)
  if (is.null(dict$keyword) || is.null(dict$score)) {
    stop("dizionario_sentiment_ita non contiene colonne 'keyword' e 'score'.")
  }
  dict
}

#' Load sentiment dictionaries
#' @export
get_sentiment_dictionaries <- function() {
  mc <- match.call()
  dicts <- list()
  
  if (exists("dizionario_sentiment_ita")) {
    data(dizionario_sentiment_ita)
    dicts$italian_general <- dizionario_sentiment_ita
  }
  
  if (exists("dizionario_loughran_ita")) {
    data(dizionario_loughran_ita) 
    dicts$loughran_italian <- dizionario_loughran_ita
  }
  
  if (exists("vocabolarioMattivio")) {
    data(vocabolarioMattivio)
    dicts$mattivio <- vocabolarioMattivio
  }
  
  if (exists("vocabolariMadda")) {
    data(vocabolariMadda)
    dicts$madda <- vocabolariMadda
  }
  
  log_reproducibility_action(
    module = "analysis",
    operation = "get_sentiment_dictionaries",
    parameters = capture_repro_args(mc),
    input_state = NULL,
    output_state = list(names = names(dicts))
  )
  
  return(dicts)
}

#' Sentiment analysis summary
#' @export
summarize_sentiment <- function(sentiment_scores) {
  mc <- match.call()
  summary_result <- list(
    positive = sum(sentiment_scores > 0),
    negative = sum(sentiment_scores < 0),
    neutral = sum(sentiment_scores == 0),
    mean_sentiment = mean(sentiment_scores, na.rm = TRUE),
    sentiment_distribution = table(cut(sentiment_scores, breaks = c(-Inf, -0.5, 0.5, Inf), 
                                      labels = c("Negative", "Neutral", "Positive")))
  )
  
  log_reproducibility_action(
    module = "analysis",
    operation = "summarize_sentiment",
    parameters = capture_repro_args(mc),
    input_state = list(length = length(sentiment_scores)),
    output_state = summary_result
  )
  
  summary_result
}
