#' Enhanced Corpus Exploration Functions
#' 
#' Integrates original TextWiller analysis capabilities
#' 

#' Advanced word frequency analysis
#' 
#' Uses original TextWiller preprocessing when available
#' 
#' @param corpus Character vector
#' @param preprocess Whether to preprocess text
#' @param use_legacy Use original TextWiller preprocessing
#' @return Data frame with word frequencies
#' @export
calculate_word_frequencies_enhanced <- function(corpus, preprocess = TRUE, 
                                               use_legacy = TRUE) {
  mc <- match.call()
  original_corpus <- corpus
  
  if (preprocess) {
    corpus <- preprocess_pipeline(corpus, 
                                 pipeline = c("lowercase", "remove_punct", "remove_stopwords"),
                                 use_legacy = use_legacy)
  }
  
  # Combine all documents
  all_text <- paste(corpus, collapse = " ")
  
  # Split into words
  words <- strsplit(all_text, "\\s+")[[1]]
  words <- words[nchar(words) > 1]  # Remove single characters
  
  # Calculate frequencies
  freq_table <- table(words)
  freq_table <- sort(freq_table, decreasing = TRUE)
  
  result <- data.frame(
    word = names(freq_table),
    frequency = as.numeric(freq_table),
    percentage = round(as.numeric(freq_table) / sum(freq_table) * 100, 2),
    stringsAsFactors = FALSE
  )
  
  log_reproducibility_action(
    module = "analysis",
    operation = "calculate_word_frequencies_enhanced",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(original_corpus),
    output_state = snapshot_dataframe(result)
  )
  
  result
}

#' Sentiment analysis using original TextWiller function
#' 
#' @param text Character vector
#' @param use_legacy Whether to use original sentiment function
#' @return Sentiment scores
#' @export
analyze_sentiment <- function(text, use_legacy = TRUE) {
  mc <- match.call()
  original_text <- text
  if (use_legacy && exists("sentiment")) {
    result <- sentiment(text)
  } else {
    result <- TextWiller3::analyze_sentiment_it(text, use_legacy = FALSE)
  }
  
  log_reproducibility_action(
    module = "analysis",
    operation = "analyze_sentiment",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(original_text),
    output_state = snapshot_text_vector(result)
  )
  
  result
}

#' User classification using original classificaUtenti
#' 
#' @param names Character vector of names
#' @param use_legacy Whether to use original classification
#' @return Classification results
#' @export
classify_users <- function(names, use_legacy = TRUE) {
  mc <- match.call()
  original_names <- names
  if (use_legacy && exists("classificaUtenti")) {
    result <- classificaUtenti(names)
  } else {
    result <- TextWiller3::classify_gender_it(names, use_legacy = FALSE)
  }
  
  log_reproducibility_action(
    module = "analysis",
    operation = "classify_users",
    parameters = capture_repro_args(mc),
    input_state = list(length = length(original_names)),
    output_state = snapshot_text_vector(result)
  )
  
  result
}

#' URL extraction using original function
#' 
#' @param text Character vector
#' @param use_legacy Whether to use original urlExtract
#' @return Extracted URLs
#' @export
extract_urls <- function(text, use_legacy = TRUE) {
  mc <- match.call()
  original_text <- text
  if (use_legacy && exists("urlExtract")) {
    result <- urlExtract(text)
  } else {
    # Basic URL extraction fallback
    urls <- regmatches(text, gregexpr("https?://[^\\s]+", text))
    result <- unlist(urls)
  }
  
  log_reproducibility_action(
    module = "analysis",
    operation = "extract_urls",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(original_text),
    output_state = snapshot_text_vector(result)
  )
  
  result
}
