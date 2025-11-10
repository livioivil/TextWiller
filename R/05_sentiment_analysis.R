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
    # Use original TextWiller sentiment function
    result <- sentiment(text, algorithm = algorithm, normalizzaTesti = normalizzaTesti)
  } else {
    # Enhanced fallback sentiment analysis
    result <- sentiment_fallback(text)
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

#' Fallback sentiment analysis
#' @noRd
sentiment_fallback <- function(text) {
  # Basic sentiment analysis based on Italian sentiment dictionaries
  if (exists("dizionario_sentiment_ita")) {
    data(dizionario_sentiment_ita)
    dict <- dizionario_sentiment_ita
  } else {
    # Very basic Italian sentiment words
    dict <- data.frame(
      word = c("buono", "bello", "ottimo", "eccellente", "fantastico", 
               "brutto", "cattivo", "pessimo", "terribile", "orribile"),
      score = c(1, 1, 1, 1, 1, -1, -1, -1, -1, -1)
    )
  }
  
  scores <- sapply(text, function(txt) {
    words <- strsplit(tolower(txt), "\\s+")[[1]]
    word_scores <- dict$score[match(words, dict$word)]
    sum(word_scores, na.rm = TRUE)
  })
  
  return(scores)
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
