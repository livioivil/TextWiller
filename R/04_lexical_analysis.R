#' Lexical Analysis Functions
#' 
#' Advanced lexical analysis including complexity measures, TTR, and Italian-specific metrics
#' 

#' Calculate Type-Token Ratio (TTR) and variants
#' 
#' @param text Character vector of texts
#' @param variant Type of TTR to calculate ("simple", "root", "corrected", "herdan", "guiraud")
#' @return TTR value
#' @export
calculate_ttr <- function(text, variant = "simple") {
  mc <- match.call()
  original_text <- text
  if (is.null(text) || length(text) == 0) {
    log_reproducibility_action(
      module = "analysis",
      operation = "calculate_ttr",
      parameters = capture_repro_args(mc),
      input_state = snapshot_text_vector(original_text),
      output_state = list(value = NA, variant = variant)
    )
    return(NA)
  }
  
  # Combine all texts
  all_text <- paste(text, collapse = " ")
  words <- strsplit(tolower(all_text), "\\s+")[[1]]
  words <- words[nchar(words) > 0]  # Remove empty strings
  
  if (length(words) == 0) {
    log_reproducibility_action(
      module = "analysis",
      operation = "calculate_ttr",
      parameters = capture_repro_args(mc),
      input_state = snapshot_text_vector(original_text),
      output_state = list(value = NA, variant = variant)
    )
    return(NA)
  }
  
  types <- unique(words)
  tokens <- length(words)
  
  result <- switch(variant,
    "simple" = length(types) / tokens,
    "root" = length(types) / sqrt(tokens),
    "corrected" = length(types) / sqrt(2 * tokens),
    "herdan" = log(length(types)) / log(tokens),
    "guiraud" = length(types) / sqrt(tokens),
    length(types) / tokens  # default to simple
  )
  
  log_reproducibility_action(
    module = "analysis",
    operation = "calculate_ttr",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(original_text),
    output_state = list(value = result, variant = variant)
  )
  
  result
}

#' Calculate Moving Average TTR (MATTR)
#' 
#' @param text Character vector
#' @param window_size Size of moving window
#' @return MATTR value
#' @export
calculate_mattr <- function(text, window_size = 100) {
  mc <- match.call()
  original_text <- text
  if (is.null(text) || length(text) == 0) {
    log_reproducibility_action(
      module = "analysis",
      operation = "calculate_mattr",
      parameters = capture_repro_args(mc),
      input_state = snapshot_text_vector(original_text),
      output_state = list(value = NA, window_size = window_size)
    )
    return(NA)
  }
  
  all_text <- paste(text, collapse = " ")
  words <- strsplit(tolower(all_text), "\\s+")[[1]]
  words <- words[nchar(words) > 0]
  
  if (length(words) <= window_size) {
    result <- calculate_ttr(text)
    log_reproducibility_action(
      module = "analysis",
      operation = "calculate_mattr",
      parameters = capture_repro_args(mc),
      input_state = snapshot_text_vector(original_text),
      output_state = list(value = result, window_size = window_size)
    )
    return(result)
  }
  
  ttr_values <- numeric(length(words) - window_size + 1)
  
  for (i in 1:(length(words) - window_size + 1)) {
    window_words <- words[i:(i + window_size - 1)]
    types <- unique(window_words)
    ttr_values[i] <- length(types) / window_size
  }
  
  result <- mean(ttr_values, na.rm = TRUE)
  
  log_reproducibility_action(
    module = "analysis",
    operation = "calculate_mattr",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(original_text),
    output_state = list(value = result, window_size = window_size)
  )
  
  result
}

#' Calculate lexical density (content word ratio)
#' 
#' @param text Character vector
#' @param language Language for content word identification
#' @return Lexical density value
#' @export
calculate_lexical_density <- function(text, language = "it") {
  mc <- match.call()
  original_text <- text
  if (is.null(text) || length(text) == 0) {
    log_reproducibility_action(
      module = "analysis",
      operation = "calculate_lexical_density",
      parameters = capture_repro_args(mc),
      input_state = snapshot_text_vector(original_text),
      output_state = list(value = NA, language = language)
    )
    return(NA)
  }
  
  all_text <- paste(text, collapse = " ")
  words <- strsplit(tolower(all_text), "\\s+")[[1]]
  words <- words[nchar(words) > 0]
  
  if (length(words) == 0) {
    log_reproducibility_action(
      module = "analysis",
      operation = "calculate_lexical_density",
      parameters = capture_repro_args(mc),
      input_state = snapshot_text_vector(original_text),
      output_state = list(value = NA, language = language)
    )
    return(NA)
  }
  
  # Basic content word identification (can be enhanced with POS tagging)
  if (language == "it") {
    # Italian function words
    function_words <- c("il", "lo", "la", "i", "gli", "le", "un", "uno", "una",
                       "di", "a", "da", "in", "con", "su", "per", "tra", "fra",
                       "è", "sono", "era", "essere", "avere", "ha", "hanno",
                       "questo", "questa", "quello", "quella", "che", "chi")
  } else {
    # English function words
    function_words <- c("the", "a", "an", "and", "or", "but", "in", "on", "at",
                       "to", "for", "of", "with", "by", "as", "is", "are", "was")
  }
  
  content_words <- words[!words %in% function_words]
  result <- length(content_words) / length(words)
  
  log_reproducibility_action(
    module = "analysis",
    operation = "calculate_lexical_density",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(original_text),
    output_state = list(value = result, language = language)
  )
  
  result
}

#' Calculate Brunato's complexity measures for Italian text
#' 
#' Based on Italian linguistic complexity measures
#' 
#' @param text Character vector
#' @return List of complexity measures
#' @export
calculate_brunato_measures <- function(text) {
  mc <- match.call()
  original_text <- text
  if (is.null(text) || length(text) == 0) {
    empty_result <- list(
      basic_vocabulary_ratio = NA,
      content_word_ratio = NA,
      lexical_richness = NA,
      syntactic_complexity = NA
    )
    log_reproducibility_action(
      module = "analysis",
      operation = "calculate_brunato_measures",
      parameters = capture_repro_args(mc),
      input_state = snapshot_text_vector(original_text),
      output_state = empty_result
    )
    return(empty_result)
  }
  
  all_text <- paste(text, collapse = " ")
  words <- strsplit(tolower(all_text), "\\s+")[[1]]
  words <- words[nchar(words) > 0]
  
  if (length(words) == 0) {
    empty_result <- list(
      basic_vocabulary_ratio = NA,
      content_word_ratio = NA,
      lexical_richness = NA,
      syntactic_complexity = NA
    )
    log_reproducibility_action(
      module = "analysis",
      operation = "calculate_brunato_measures",
      parameters = capture_repro_args(mc),
      input_state = snapshot_text_vector(original_text),
      output_state = empty_result
    )
    return(empty_result)
  }
  
  # Italian basic vocabulary (simplified - should be expanded)
  basic_vocab <- c("essere", "avere", "fare", "dire", "andare", "venire",
                  "potere", "volere", "dovere", "sapere", "vedere", "sentire",
                  "parlare", "pensare", "trovare", "prendere", "dare", "mettere")
  
  # Calculate measures
  basic_vocab_ratio <- sum(words %in% basic_vocab) / length(words)
  content_word_ratio <- calculate_lexical_density(text, "it")
  lexical_richness <- calculate_ttr(text, "herdan")
  
  # Simple syntactic complexity (average word length as proxy)
  avg_word_length <- mean(nchar(words), na.rm = TRUE)
  
  result <- list(
    basic_vocabulary_ratio = round(basic_vocab_ratio, 3),
    content_word_ratio = round(content_word_ratio, 3),
    lexical_richness = round(lexical_richness, 3),
    syntactic_complexity = round(avg_word_length, 2)
  )
  
  log_reproducibility_action(
    module = "analysis",
    operation = "calculate_brunato_measures",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(original_text),
    output_state = result
  )
  
  result
}

#' Calculate multiple lexical diversity measures
#' 
#' @param text Character vector
#' @return Data frame with multiple diversity measures
#' @export
calculate_lexical_diversity <- function(text) {
  mc <- match.call()
  original_text <- text
  measures <- list()
  
  measures$ttr_simple <- calculate_ttr(text, "simple")
  measures$ttr_root <- calculate_ttr(text, "root")
  measures$ttr_corrected <- calculate_ttr(text, "corrected")
  measures$ttr_herdan <- calculate_ttr(text, "herdan")
  measures$mattr <- calculate_mattr(text)
  measures$lexical_density <- calculate_lexical_density(text)
  
  # Convert to data frame
  result <- data.frame(
    measure = names(measures),
    value = unlist(measures),
    stringsAsFactors = FALSE
  )
  
  result$value <- round(result$value, 4)
  
  log_reproducibility_action(
    module = "analysis",
    operation = "calculate_lexical_diversity",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(original_text),
    output_state = snapshot_dataframe(result)
  )
  
  return(result)
}

#' Extract keywords based on TF-IDF or log-likelihood
#' 
#' @param corpus Character vector of documents
#' @param method Method for keyword extraction ("tfidf", "loglikelihood")
#' @param top_n Number of top keywords to return
#' @return Data frame with keywords and scores
#' @export
extract_keywords <- function(corpus, method = "tfidf", top_n = 20) {
  mc <- match.call()
  original_corpus <- corpus
  if (is.null(corpus) || length(corpus) == 0) {
    empty_result <- data.frame(term = character(), score = numeric())
    log_reproducibility_action(
      module = "analysis",
      operation = "extract_keywords",
      parameters = capture_repro_args(mc),
      input_state = snapshot_text_vector(original_corpus),
      output_state = snapshot_dataframe(empty_result)
    )
    return(empty_result)
  }
  
  # Calculate term frequencies
  all_terms <- unlist(strsplit(tolower(paste(corpus, collapse = " ")), "\\s+"))
  all_terms <- all_terms[nchar(all_terms) > 1]
  
  if (length(all_terms) == 0) {
    empty_result <- data.frame(term = character(), score = numeric())
    log_reproducibility_action(
      module = "analysis",
      operation = "extract_keywords",
      parameters = capture_repro_args(mc),
      input_state = snapshot_text_vector(original_corpus),
      output_state = snapshot_dataframe(empty_result)
    )
    return(empty_result)
  }
  
  term_freq <- table(all_terms)
  term_freq <- sort(term_freq, decreasing = TRUE)
  
  if (method == "tfidf") {
    # Simple TF-IDF implementation
    doc_freq <- sapply(names(term_freq), function(term) {
      sum(grepl(paste0("\\b", term, "\\b"), tolower(corpus)))
    })
    
    tf <- as.numeric(term_freq)
    idf <- log(length(corpus) / (doc_freq + 1))
    scores <- tf * idf
    
  } else if (method == "loglikelihood") {
    # Log-likelihood ratio (simplified)
    total_terms <- sum(term_freq)
    expected <- total_terms / length(term_freq)
    scores <- 2 * term_freq * log(term_freq / expected)
  } else {
    scores <- as.numeric(term_freq)  # Default to frequency
  }
  
  result <- data.frame(
    term = names(term_freq),
    frequency = as.numeric(term_freq),
    score = round(as.numeric(scores), 4),
    stringsAsFactors = FALSE
  )
  
  result <- result[order(-result$score), ]
  result <- head(result, top_n)
  
  log_reproducibility_action(
    module = "analysis",
    operation = "extract_keywords",
    parameters = capture_repro_args(mc),
    input_state = snapshot_text_vector(original_corpus),
    output_state = snapshot_dataframe(result)
  )
  
  return(result)
}

#' Summarize core lexical features (Brunato-inspired)
#'
#' Provides easily computable proxies for some of the features discussed in
#' Brunato et al. (2018), such as word/sentence length, lexical density, and
#' type-token ratios. More advanced morpho-syntactic metrics require POS/dependency
#' pipelines and are therefore returned as not available placeholders.
#'
#' @param corpus Character vector of documents
#' @return A list with `basic` and `placeholders` data frames
#' @export
summarize_lexical_features <- function(corpus) {
  if (is.null(corpus) || length(corpus) == 0) {
    return(list(
      basic = data.frame(metric = character(), value = character(), stringsAsFactors = FALSE),
      placeholders = data.frame(feature = character(), availability = character(), stringsAsFactors = FALSE)
    ))
  }
  
  tokens <- unlist(strsplit(paste(corpus, collapse = " "), "\\s+"))
  tokens <- tokens[nchar(tokens) > 0]
  avg_word_length <- if (length(tokens) > 0) round(mean(nchar(tokens)), 2) else NA
  
  sentences <- unlist(tokenizers::tokenize_sentences(corpus))
  sentence_lengths <- sapply(sentences, function(s) length(strsplit(s, "\\s+")[[1]]))
  sentence_lengths <- sentence_lengths[!is.na(sentence_lengths) & sentence_lengths > 0]
  avg_sentence_length <- if (length(sentence_lengths) > 0) round(mean(sentence_lengths), 2) else NA
  
  lexical_density <- round(calculate_lexical_density(corpus), 3)
  ttr_form <- round(calculate_ttr(corpus, "simple"), 3)
  ttr_root <- round(calculate_ttr(corpus, "root"), 3)
  
  basic <- data.frame(
    metric = c(
      "Average word length (chars)",
      "Average sentence length (tokens)",
      "Lexical density",
      "TTR (forms)",
      "Root TTR"
    ),
    value = c(
      avg_word_length,
      avg_sentence_length,
      lexical_density,
      ttr_form,
      ttr_root
    ),
    stringsAsFactors = FALSE
  )
  
  placeholders <- data.frame(
    feature = c(
      "POS distribution",
      "Verb mood/tense/person distribution",
      "Dependency relations (subject/object/etc.)",
      "Verbal roots / nominal sentences",
      "Parse tree depth & complement chains",
      "Verb arity and argument order",
      "Clause subordination metrics",
      "Dependency link length",
      "Clause length (T-unit)"
    ),
    availability = rep("Not available (requires POS + dependency parsing pipeline)", 9),
    stringsAsFactors = FALSE
  )
  
  list(basic = basic, placeholders = placeholders)
}
